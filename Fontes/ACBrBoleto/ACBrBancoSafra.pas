{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014 Paulo H. Ribeiro,                      }
{                                       Jackeline Bellon,                      }
{                                       Juliomar Marchetti e                   }
{                                       Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  André Ferreira de Moraes                       }
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
******************************************************************************}

{$I ACBr.inc}

unit ACBrBancoSafra;

interface

uses Classes, SysUtils, StrUtils, ACBrBoleto;

type

  { TACBrBancoSafra }
  TACBrBancoSafra = class(TACBrBancoClass)
  private
  protected
    FNumeroRemessa: Integer;
  public
    constructor Create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo): string; override;
    function MontarCodigoBarras(const ACBrTitulo: TACBrTitulo): string; override;
    function MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): string; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): string; override;
    procedure GerarRegistroHeader400(NumeroRemessa: integer;
      ARemessa: TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo;
      aRemessa: TStringList); override;
    procedure GerarRegistroTrailler400(ARemessa: TStringList); override;
    procedure LerRetorno400(ARetorno: TStringList); override;

    function TipoOcorrenciaToDescricao(
      const TipoOcorrencia: TACBrTipoOcorrencia): string; override;
    function CodOcorrenciaToTipo(const CodOcorrencia: integer): TACBrTipoOcorrencia;
      override;
    function TipoOCorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): string;
      override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia;
      CodMotivo: integer): string; override;

    function CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
  end;

implementation

uses ACBrUtil;

var
  aTotal: Extended;
  aCount: Integer;

{ TACBrBancoSafra }

constructor TACBrBancoSafra.Create(AOwner: TACBrBanco);
begin
  inherited Create(AOwner);
  fpDigito                := 7;
  fpNome                  := 'Banco Safra';
  fpNumero                := 422;
  fpTamanhoAgencia        := 4;
  fpTamanhoConta          := 8;
  fpTamanhoCarteira       := 1;
  fpTamanhoMaximoNossoNum := 9;
end;

function TACBrBancoSafra.CalcularDigitoVerificador(
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

function TACBrBancoSafra.CodMotivoRejeicaoToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: integer): string;
begin
  case TipoOcorrencia of
    toRetornoRegistroRecusado:
      case CodMotivo of
        001: Result := '001-MOEDA INVÁLIDA';
        002: Result := '002-MOEDA INVÁLIDA PARA CARTEIRA';
        003: Result := '003-CARTEIRA TRÊS INVÁLIDA PARA TIPO DE MOEDA';
        004: Result := '004-TIPO DE IOF. INVÁLIDO PARA CPBRANÇA DE SEGUROS';
        005: Result := '005-TIPO DE IOF. INVÁLIDO PARA VALOR DE IOF (SEGUROS)';
        006: Result := '006-VALOR DE IOF INVÁLIDO (SEGUROS)';
        007: Result := '007-CEP NÃO CORRESPONDE UF';
        008: Result := '008-VALOR JUROS AO DIA MAIOR QUE 5% DO VALOR DO TÍTULO';
        009: Result := '009-USO EXCLUSIVO NÃO NUMÉRICO PARA COBRANÇA EXPRESS';
        010: Result := '010-SEU NÚMERO - NÃO NUMÉRICO PARA CHEQUE';
        011: Result := '011-NOSSO NÚMERO FORA DA FAIXA';
        012: Result := '012-CEP DE CIDADE INEXISTENTE';
        013: Result := '013 - CEP FORA DE FAIXA DA CIDADE';
        014: Result := '014 - UF INVALIDO PARA CEP DA CIDADE';
        015: Result := '015 - CEP ZERADO';
        016: Result := '016 - CEP NÃO CONSTA NA TABELA SAFRA';
        017: Result := '017 - CEP NÃO CONSTA TABELA BCO. CORRESPONDENTE';
        018: Result := '018 - DADOS DO CHEQUE NÃO NUMÉRICO';
        019: Result := '019 - PROTESTO IMPRATICÁVEL';
        020: Result := '020 - PRIMEIRA INSTRUÇÃO DE COBRANÇA INVALIDA';
        021: Result := '021 - SEGUNDA INSTRUÇÃO DE COBRANÇA INVÁLIDA';
        022: Result := '022 - SEGUNDA INSTR. (10) E TERCEIRA INSTR. INVALIDA';
        023: Result := '023 - TERCEIRA INSTRUÇÃO DE COBRANÇA INVÁLIDA';
        024: Result := '024 - DIGITO VERIFICADOR C1 INVÁLIDO';
        025: Result := '025 - DIGITO VERIFICADOR C2 INVÁLIDO';
        026: Result := '026 - CÓDIGO DE OPERAÇÃO/OCORRÊNCIA INVÁLIDO';
        027: Result := '027 - OPERAÇÃO INVÁLIDA PARA O CLIENTE';
        028: Result := '028 - NOSSO NÚMERO NÃO NUMÉRICO OU ZERADO';
        029: Result := '029 - NOSSO NÚMERO COM DÍGITO DE CONTROLE ERRADO';
        030: Result := '030 - VALOR DO ABATIMENTO NÃO NUMÉRICO OU ZERADO';
        031: Result := '031 - SEU NÚMERO EM BRANCO';
        032: Result := '032 - CÓDIGO DA CARTEIRA INVÁLIDO';
        033: Result := '033 - DIGITO VERIFICADOR C3 INVÁLIDO';
        034: Result := '034 - CÓDIGO DO TÍTULO INVÁLIDO';
        035: Result := '035 - DATA DE MOVIMENTO INVÁLIDA';
        036: Result := '036 - DATA DE EMISSÃO INVÁLIDA';
        037: Result := '037 - DATA DE VENCIMENTO INVÁLIDA';
        038: Result := '038 - DEPOSITÁRIA INVÁLIDA';
        039: Result := '039 - DEPOSITÁRIA INVÁLIDA PARA O CLIENTE';
        040: Result := '040 - DEPOSITÁRIA NÃO CADASTRADA NO BANCO';
        041: Result := '041 - CÓDIGO DE ACEITE INVÁLIDO';
        042: Result := '042 - ESPÉCIE DE TÍTULO INVÁLIDO';
        043: Result := '043 - INSTRUÇÃO DE COBRANÇA INVÁLIDA';
        044: Result := '044 - VALOR DO TÍTULO NÃO NUMÉRICO OU ZERADO';
        045: Result := '045 - DATA DE OPERAÇÃO INVALIDA';
        046: Result := '046 - VALOR DE JUROS NÃO NUMÉRICO OU ZERADO';
        047: Result := '047 - DATA LIMITE PARA DESCONTO INVÁLIDA';
        048: Result := '048 - VALOR DO DESCONTO INVÁLIDO';
        049: Result := '049 - VALOR IOF. NÃO NUMÉRICO OU ZERADO (SEGUROS)';
        050: Result := '050 - ABATIMENTO COM VALOR PARA OPERAÇÃO "01" (Entrada de Título)';
        051: Result := '051 - CÓDIGO DE INSCRIÇÃO DO SACADO INVÁLIDO';
        052: Result := '052 - CÓDIGO DE INSCRIÇÃO / NÚMERO DE INSCRIÇÃO DO SACADO INVÁLIDO';
        053: Result := '053 - NÚMERO DE INSCRIÇÃO DO SACADO NÃO NUMÉRICO OU DÍGITO ERRADO';
        054: Result := '054 - NOME DO SACADO EM BRANCO';
        055: Result := '055 - ENDEREÇO DO SACADO EM BRANCO';
        056: Result := '056 - CLIENTE NÃO RECADASTRADO';
        057: Result := '057 - CLIENTE BLOQUEADO (quando operação de desconto e cliente sem número; de borderô disponível)';
        058: Result := '058 - PROCESSO DE CARTÓRIO INVÁLIDO';
        059: Result := '059 - ESTADO DO SACADO INVÁLIDO';
        060: Result := '060 - CEP / ENDEREÇO DIVERGEM DO CORREIO';
        061: Result := '061 - INSTRUÇÃO AGENDADA PARA AGÊNCIA';
        062: Result := '062 - OPERAÇÃO INVÁLIDA PARA A CARTEIRA';
        063: Result := '063 - Carteira inválida para Cobrança Direta';
        064: Result := '064 - TÍTULO INEXISTENTE (TFC)';
        065: Result := '065 - OPERAÇÃO / Título JÁ EXISTENTE';
        066: Result := '066 - TÍTULO JÁ EXISTE (TFC)';
        067: Result := '067 - DATA DE VENCIMENTO INVÁLIDA PARA PROTESTO';
        068: Result := '068 - CEP DO SACADO NÃO CONSTA NA TABELA';
        069: Result := '069 - PRAÇA NÃO ATENDIDA PELO SERVIÇO CARTÓRIO';
        070: Result := '070 - AGÊNCIA INVÁLIDA';
        071: Result := '071 - CLIENTE NÃO CADASTRADO';
        072: Result := '072 - TÍTULO JÁ EXISTE (COB)';
        073: Result := '073 - TAXA OPERAÇÃO NÃO NUMÉRICA OU ZERADA (VENDOR)';
        074: Result := '074 - TÍTULO FORA DE SEQÜÊNCIA';
        075: Result := '075 - TAXA DE OPERAÇÃO ZERADA (VENDOR)';
        076: Result := '076 - EQUALIZAÇÃO NÃO NUMÉRICA OU INVÁLIDA (VENDOR)';
        077: Result := '077 - TAXA NEGOCIADA NÃO NUMÉRICA OU ZERADA (VENDOR)';
        078: Result := '078 - TÍTULO INEXISTENTE (COB)';
        079: Result := '079 - OPERAÇÃO NÃO CONCLUÍDA';
        080: Result := '080 - TÍTULO JÁ Baixado';
        081: Result := '081 - TÍTULO NÃO DESCONTADO';
        082: Result := '082 - INTERVALO ENTRE DATA DE OPERAÇÃO E DATA VCTO MENOR QUE UM DIA';
        083: Result := '083 - PRORROGAÇÃO / ALTERAÇÃO DE VENCIMENTO INVÁLIDA';
        084: Result := '084 - MOVIMENTO IGUAL AO CADASTRO DE EXISTÊNCIA DO COB';
        085: Result := '085 - CÓDIGO OPERAÇÃO COM PCB INVÁLIDO (OPERAÇÃO INVÁLIDA P/ CARTEIRA)';
        086: Result := '086 - ABATIMENTO MAIOR QUE VALOR DO TÍTULO';
        087: Result := '087 - ALTERAÇÃO DE CARTÓRIO INVÁLIDA';
        088: Result := '088 - TÍTULO RECUSADO COMO GARANTIA (Sacado / Novo / Exclusivo Alçada comitê)';
        089: Result := '089 - ALTERAÇÃO DE DATA DE PROTESTO INVÁLIDA';
        090: Result := '090 - MODALIDADE DE VENDOR INVALIDO';
        091: Result := '091 - PCB CTO INVALIDA';
        092: Result := '092 - DATA DE OPERAÇÃO CTO INVÁLIDA';
        093: Result := '093 - BAIXA DE TÍTULO DE OUTRA AGÊNCIA';
        094: Result := '094 - ENTRADA TÍTULO COBRANÇA DIRETA INVÁLIDA';
        095: Result := '095 - BAIXA TÍTULO COBRANÇA DIRETA INVÁLIDA';
        096: Result := '096 - VALOR DO TÍTULO INVÁLIDO';
        097: Result := '097 - MOEDA INVÁLIDA PARA BANCO CORRESPONDENTE';
        098: Result := '098 - PCB DO TFC DIVERGEM DA PCB DO COB';
        099: Result := '099 - INCLUSÃO DE TERCEIRA MOEDA INVÁLIDA';
        100: Result := '115 - ESPÉCIE DOC INVÁLIDO PARA MODAL/RAMO DE ATIVIDADE (RESERVADO CTO)';
        else
          Result := IntToStrZero(CodMotivo, 3) + ' - Outros Motivos';
      end;
  end;
end;

function TACBrBancoSafra.CodOcorrenciaToTipo(const CodOcorrencia: integer):
TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02: Result := toRetornoRegistroConfirmado;
    03: Result := toRetornoRegistroRecusado;
    04: Result := toRetornoTransferenciaCarteiraEntrada;
    05: Result := toRetornoTransferenciaCarteiraBaixa;
    06: Result := toRetornoLiquidado;
    07: Result := toRetornoLiquidadoParcialmente;
    09: Result := toRetornoBaixaAutomatica;
    10: Result := toRetornoBaixadoInstAgencia;
    11: Result := toRetornoTituloEmSer;
    12: Result := toRetornoAbatimentoConcedido;
    13: Result := toRetornoAbatimentoCancelado;
    14: Result := toRetornoVencimentoAlterado;
    15: Result := toRetornoLiquidadoEmCartorio;
    16: Result := toRetornoBaixadoFrancoPagamento;
    17: Result := toRetornoEntradaBorderoManual;
    18: Result := toRetornoAlteracaoUsoCedente;
    19: Result := toRetornoRecebimentoInstrucaoProtestar;
    20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
    21: Result := toRetornoTransferenciaCedente;
    23: Result := toRetornoEncaminhadoACartorio;
    40: Result := toRetornoBaixaPorProtesto;
    41: Result := toRetornoLiquidadoAposBaixaOuNaoRegistro;
    42: Result := toRetornoRetiradoDeCartorio;
    43: Result := toRetornoDespesaCartorio;
    44: Result := toRetornoDebitoDiretoAutorizado;
    45: Result := toRetornoDebitoDiretoNaoAutorizado;
    51: Result := toRetornoRecebimentoInstrucaoAlterarValorTitulo;
    52: Result := toRetornoAlteracaoDataEmissao;
    53: Result := toRetornoAlteracaoEspecie;
    54: Result := toRetornoAlteracaoSeuNumero;
    60: Result := toRetornoEqualizacaoVendor;
    77: Result := toRetornoRecebimentoInstrucaoAlterarJuros;
    else
      Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoSafra.CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02 : Result:= toRemessaBaixar;                          {Pedido de Baixa}
    04 : Result:= toRemessaConcederAbatimento;              {Concessão de Abatimento}
    05 : Result:= toRemessaCancelarAbatimento;              {Cancelamento de Abatimento concedido}
    06 : Result:= toRemessaAlterarVencimento;               {Alteração de vencimento}
    07 : Result:= toRemessaAlterarExclusivoCliente;         {Alteração "Uso Exclusivo do Cliente"}
    08 : Result:= toRemessaAlterarNumeroControle;           {Alteração de seu número}
    09 : Result:= toRemessaProtestar;                       {Pedido de protesto}
    10 : Result:= toRemessaNaoProtestar;                    {Não Protestar}
    11 : Result:= toRemessaNaoCobrarJurosMora;              {Não Cobrar Juros de Mora}
    16 : Result:= toRemessaCobrarJurosMora;                 {Cobrar Juros de Mora}
    31 : Result:= toRemessaAlterarValorTitulo;              {Alteração do Valor do Título}
  else
     Result:= toRemessaRegistrar;                           {Remessa}
  end;
end;

procedure TACBrBancoSafra.GerarRegistroHeader400(NumeroRemessa: integer;
  ARemessa: TStringList);
var wLinha: String;
begin
  aTotal := 0;
  aCount := 0;
  FNumeroRemessa := NumeroRemessa;

  with ACBrBanco.ACBrBoleto.Cedente do
  begin
    wLinha := '0'                             + // ID do Registro Header
              '1'                             + // ID do Arquivo de Remessa
              'REMESSA'                       + // Literal de Remessa
              '01'                            + // Código do Tipo de Serviço
              PadRight('COBRANCA', 15)        + // Descrição do tipo de serviço + "brancos"
              PadLeft(CodigoCedente, 14, '0') + // Codigo da Empresa no Banco
              Space(6)                        + // "brancos"
              PadRight(Nome, 30)              + // Nome da Empresa
              IntToStr(Numero)                + // Código do Banco - 237
              PadRight('BANCO SAFRA', 15)     + // Nome do Banco - BANCO SAFRA + "brancos"
              FormatDateTime('ddmmyy', Now)   + // Data de geração do arquivo
              Space(291)                      + // "brancos"
              IntToStrZero(NumeroRemessa, 3)  + // Nr. Sequencial de Geração do Arquivo
              IntToStrZero(1, 6);               // Nr. Sequencial do Registro no Arquivo

    ARemessa.Text := ARemessa.Text + UpperCase(wLinha);
  end;

end;

procedure TACBrBancoSafra.GerarRegistroTrailler400(ARemessa: TStringList);
var
  wLinha: String;
begin
  wLinha := '9'                               + // Identificação do Registro do Trailler
  Space(367)                                  + // "Branc"
  PadLeft(IntToStr(aCount), 8, '0')           + // Quantidade de títulos no arquivo
  FormatCurr('000000000000000', aTotal * 100) + // Valor total dos títulos
  IntToStrZero(FNumeroRemessa, 3)             + // Nr. Sequencial de Geração do Arquivo
  IntToStrZero(ARemessa.Count + 1, 6);          // Número Sequencial do Registro do Arquivo

  ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);

end;

procedure TACBrBancoSafra.GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo;
  aRemessa: TStringList);
var
  wLinha, tipoInscricao, aAgencia, aConta: String;
  Ocorrencia, aEspecie, aAceite, aInstrucao2: String;
  aTipoSacado, sDataDesconto:String;
begin
  with ACBrTitulo do
  begin
    if ACBrBoleto.Cedente.TipoInscricao = pFisica then
      tipoInscricao := '01'
    else
      tipoInscricao := '02';

    aAgencia := PadLeft(ACBrBoleto.Cedente.Agencia, 4, '0') +
                PadLeft(ACBrBoleto.Cedente.AgenciaDigito, 1, '0');

    aConta := PadLeft(ACBrBoleto.Cedente.Conta, 8, '0') +
              PadLeft(ACBrBoleto.Cedente.ContaDigito, 1, '0');

    {Pegando Código da Ocorrencia}
    case OcorrenciaOriginal.Tipo of
      toRemessaBaixar                 : Ocorrencia := '02'; {Pedido de Baixa}
      toRemessaConcederAbatimento     : Ocorrencia := '04'; {Concessão de Abatimento}
      toRemessaCancelarAbatimento     : Ocorrencia := '05'; {Cancelamento de Abatimento concedido}
      toRemessaAlterarVencimento      : Ocorrencia := '06'; {Alteração de vencimento}
      toRemessaAlterarExclusivoCliente: Ocorrencia := '07'; {Alteração "Uso Exclusivo do Cliente"}
      toRemessaAlterarNumeroControle  : Ocorrencia := '08'; {Alteração de seu número}
      toRemessaProtestar              : Ocorrencia := '09'; {Pedido de protesto}
      toRemessaNaoProtestar           : Ocorrencia := '10'; {Não Protestar}
      toRemessaNaoCobrarJurosMora     : Ocorrencia := '11'; {Não Cobrar Juros de Mora}
      toRemessaCobrarJurosMora        : Ocorrencia := '16'; {Cobrar Juros de Mora}
      toRemessaAlterarValorTitulo     : Ocorrencia := '31'; {Alteração do Valor do Título}
      else
        Ocorrencia := '01'; {Remessa}
    end;

    {Pegando Especie}
    if trim(EspecieDoc) = 'DM' then
      aEspecie := '01'
    else if trim(EspecieDoc) = 'NP' then
      aEspecie := '02'
    else if trim(EspecieDoc) = 'NS' then
      aEspecie := '03'
    else if trim(EspecieDoc) = 'RC' then
      aEspecie := '05'
    else if Trim(EspecieDoc) = 'DS' then
      aEspecie := '09'
    else
      aEspecie := EspecieDoc;

    if Aceite = atSim then
      aAceite := 'A'
    else
      aAceite := 'N';

    if StrToIntDef(Instrucao3,0) > 0 then
      aInstrucao2 := '10'
    else
      aInstrucao2 := PadLeft(trim(Instrucao2), 2, '0');

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

    wLinha := '1'                                                                            + //   1 a   1 - Identificação do Registro de Transação
              tipoInscricao                                                                  + //   2 a   3 - Tipo de Inscrição da Empresa
              PadLeft(OnlyNumber(ACBrBoleto.Cedente.CNPJCPF), 14, '0')                       + //   4 a  17 - Número da Inscrição da Empresa
              aAgencia + aConta                                                              + //  18 a  31 - Identificação da Empresa no Banco
              Space(6)                                                                       + //  32 a  37 - "Brancos"
              Space(25)                                                                      + //  38 a  62 - Uso exclusivo da Empresa
              IfThen(NossoNumero = '000000000', '000000000',
                                 PadLeft(RightStr(NossoNumero,8),8,'0') +
                                 CalcularDigitoVerificador(ACBrTitulo)) +                      //  63 a  71 - Número do título no banco
              Space(30)                                                                      + //  72 a 101 - "Brancos"
              '0'                                                                            + // 102 a 102 - Código de IOF sobre Operações de Seguro
              '00'                                                                           + // 103 a 104 - Identificação do Tipo de Moeda, 00=Real
              Space(1)                                                                       + // 105 a 105 - "Branco"
              IntToStrZero(StrToIntDef(Instrucao3,0), 2)                                     + // 106 a 107 - Terceira Instrução de Cobrança. Utilizar somente quando Instrução2 é igual a 10
              Carteira                                                                       + // 108 a 108 - Identificação do Tipo de Carteira
              Ocorrencia                                                                     + // 109 a 110 - Identificação do Tipo de Ocorrência
              PadRight(SeuNumero, 10)                                                        + // 111 a 120 - Identificação do Título da Empresa
              FormatDateTime('ddmmyy', Vencimento)                                           + // 121 a 126 - Data de Vencimento do Título
              IntToStrZero(Round(ValorDocumento * 100), 13)                                  + // 127 a 139 - Valor Nominal do Título
              IntToStr(ACBrBoleto.Banco.Numero)                                              + // 140 a 142 - Código do Banco encarregado da cobraça
              '00000'                                                                        + // 143 a 147 - Agência Encarregada da Cobrança
              aEspecie                                                                       + // 148 a 149 - Espécie do Título
              aAceite                                                                        + // 150 a 150 - Identificação do Aceite do Título
              FormatDateTime('ddmmyy', DataDocumento)                                        + // 151 a 156 - Data de Emissão do Título
              PadLeft(trim(Instrucao1), 2, '0')                                              + // 157 a 158 - Primeira Instrução de Cobrança
              aInstrucao2                                                                    + // 159 a 160 - Segunda Instrução de Cobrança
              IntToStrZero(round(ValorMoraJuros * 100), 13)                                  + // 161 a 173 - Juros de Mora Por Dia de Atraso
              sDataDesconto                                                                  + // 174 a 179 - Data Limite para Desconto
              IntToStrZero(round(ValorDesconto * 100), 13)                                   + // 180 a 192 - Valor Do Desconto Concedido
              IntToStrZero(round(ValorIOF * 100), 13)                                        + // 193 a 205 - Valor De Iof Operações Deseguro

              IfThen( ((Ocorrencia = '01') and (Copy(Instrucao1, 1, 2) = '16')),
                (FormatDateTime('ddmmyy', DataMulta) +                                         // 206 a 211 a data a partir da qual a multa deve ser cobrada
                IntToStrZero(round(PercentualMulta * 100), 4) +                                // 212 a 215 o percentual referente à multa no formato 99v99
                '000'),                                                                        // 216 a 218 zeros
                IntToStrZero(round(ValorAbatimento * 100), 13)  )                            + // Valor Do Abatimento Concedido Ou Cancelado / Multa

              aTipoSacado                                                                    + // 219 a 220 - Código De Inscrição Do Sacado
              PadLeft(OnlyNumber(Sacado.CNPJCPF), 14, '0')                                   + // 221 a 234 - Número de Inscrição do Sacado
              PadRight(Sacado.NomeSacado, 40, ' ')                                           + // 235 a 274 - Nome Do Sacado
              PadRight(Sacado.Logradouro + ' ' + Sacado.Numero, 40, ' ')                     + // 275 a 314 - Endereço Do Sacado
              PadRight(Sacado.Bairro, 10, ' ')                                               + // 315 a 324 - Bairro Do Sacado
              Space(2)                                                                       + // 325 a 326 - Brancos
              PadRight(Sacado.CEP, 8)                                                        + // 327 a 334 - CEP do Sacado
              PadRight(Sacado.Cidade, 15)                                                    + // 335 a 349 - Cidade do Sacado
              PadRight(Sacado.UF, 2)                                                         + // 350 a 351 - UF do Sacado
              PadRight(Sacado.SacadoAvalista.NomeAvalista, 30)                               + // 352 a 381 - Nome do Sacador Avalista / Mensagem específica vide nota 6.1.9 conforme manual do banco
              Space(7)                                                                       + // 382 a 388 - "Brancos"
              '422'                                                                          + // 389 a 391 - Banco Emitente do Boleto
              copy(aRemessa.Text, 392, 3)                                                    + // 392 a 394 - Numero Seqüencial Geração Arquivo Remessa
              IntToStrZero(ARemessa.Count + 1, 6);                                             // 395 a 400 - Número Sequencial De Registro De Arquivo

    aTotal := aTotal + ValorDocumento;
    Inc(aCount);
    aRemessa.Text := aRemessa.Text + wLinha;
  end;
end;

procedure TACBrBancoSafra.LerRetorno400(ARetorno: TStringList);
var
  rCodEmpresa, rCedente, rAgencia, rDigitoAgencia: String;
  rConta, rDigitoConta, rCNPJCPF, Linha: String;
  ContLinha, CodOcorrencia: Integer;
  Titulo: TACBrTitulo;
begin
  if StrToIntDef(copy(ARetorno.Strings[0], 77, 3), -1) <> Numero then
    raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                           'não é um arquivo de retorno do ' + Nome));

  rCodEmpresa    := trim(Copy(ARetorno[0], 27, 14));
  rCedente       := trim(Copy(ARetorno[0], 47, 30));
  rAgencia       := trim(Copy(ARetorno[1], 18, ACBrBanco.TamanhoAgencia));
  rDigitoAgencia := trim(Copy(ARetorno[1], 22, 1));
  rConta         := trim(Copy(ARetorno[1], 23, ACBrBanco.TamanhoConta));
  rDigitoConta   := Copy(ARetorno[1], 31, 1);

  ACBrBanco.ACBrBoleto.DataArquivo := StringToDateTimeDef(Copy(ARetorno[0], 95, 2) + '/' +
                                                          Copy(ARetorno[0], 97, 2) + '/' +
                                                          Copy(ARetorno[0], 99, 2), 0,
                                                          'DD/MM/YY');

  ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0], 392, 3), 0);

  ValidarDadosRetorno(rAgencia, rConta);
  with ACBrBanco.ACBrBoleto do
  begin
    if (not LeCedenteRetorno) and (rCodEmpresa <>
        PadLeft(Cedente.CodigoCedente, 14, '0')) then
      raise Exception.Create(ACBrStr('Código da Empresa do arquivo inválido'));

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

      NossoNumero := Copy(Linha, 63, 8);
      OcorrenciaOriginal.Tipo :=
        CodOcorrenciaToTipo(StrToIntDef(copy(Linha, 109, 2), 0));

      Carteira := copy(Linha, 108, 1);

      CodOcorrencia  := StrToIntDef(copy(Linha, 109, 2),0);
      DataOcorrencia := StringToDateTimeDef(copy(Linha, 111, 2) + '/' +
                                            copy(Linha, 113, 2) + '/' +
                                            copy(Linha, 115, 2), 0, 'DD/MM/YY');

      if CodOcorrencia = 03 then // entrada rejeitada
      begin
        MotivoRejeicaoComando.Add(copy(Linha, 105, 3));
        DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(
          toRetornoRegistroRecusado, StrToIntDef(copy(Linha, 105, 3),0)));
      end;
      SeuNumero := copy(Linha, 117, 10);

      if Copy(Linha, 147, 2) <> '00' then
        Vencimento := StringToDateTimeDef(copy(Linha, 147, 2) + '/' +
                                          copy(Linha, 149, 2) + '/' +
                                          copy(Linha, 151, 2), 0, 'DD/MM/YY');

      ValorDocumento       := StrToFloatDef(copy(Linha, 153, 13), 0) / 100;
      EspecieDoc           := copy(Linha, 174, 2);
      ValorDespesaCobranca := StrToFloatDef(Copy(Linha, 176, 13), 0) / 100;
      ValorOutrasDespesas  := StrToFloatDef(Copy(Linha, 189, 13), 0) / 100;
      ValorIOF             := StrToFloatDef(Copy(Linha, 215, 13), 0) / 100;
      ValorAbatimento      := StrToFloatDef(Copy(Linha, 228, 13), 0) / 100;
      ValorDesconto        := StrToFloatDef(Copy(Linha, 241, 13), 0) / 100;
      ValorRecebido        := StrToFloatDef(Copy(Linha, 254, 13), 0) / 100;
      ValorMoraJuros       := StrToFloatDef(Copy(Linha, 267, 13), 0) / 100;
      ValorOutrosCreditos  := StrToFloatDef(Copy(Linha, 280, 13), 0) / 100;

      if Copy(Linha, 296, 2) <> '00' then
        DataCredito := StringToDateTimeDef(copy(Linha, 296, 2) + '/' +
                                           copy(Linha, 298, 2) + '/' +
                                           copy(Linha, 300, 2), 0, 'DD/MM/YY');
    end;
  end;
end;

function TACBrBancoSafra.MontarCampoCodigoCedente(
  const ACBrTitulo: TACBrTitulo): string;
begin
  with ACBrTitulo.ACBrBoleto.Cedente do
  begin
    Result := PadLeft(Agencia, 4, '0') + PadLeft(AgenciaDigito, 1, '0') + '/' + PadLeft(ACBrBoleto.Cedente.Conta, 8, '0') + PadLeft(ACBrBoleto.Cedente.ContaDigito, 1, '0');
  end;
end;

function TACBrBancoSafra.MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): string;
begin
  with ACBrTitulo do
  begin
    Result := PadLeft(RightStr(NossoNumero,8),8,'0') + '-' + CalcularDigitoVerificador(ACBrTitulo);
  end;
end;

function TACBrBancoSafra.MontarCodigoBarras(const ACBrTitulo: TACBrTitulo): string;
var
  CodigoBarras, FatorVencimento, DigitoCodBarras: string;
begin
  with ACBrTitulo.ACBrBoleto do
  begin
    FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);

    CodigoBarras := IntToStr(Banco.Numero) + '9' + FatorVencimento +
                    IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10) +
                    '7' + Cedente.Agencia + Cedente.AgenciaDigito + Cedente.Conta + Cedente.ContaDigito +
                    PadLeft(RightStr(ACBrTitulo.NossoNumero,8),8,'0') + CalcularDigitoVerificador(ACBrTitulo) + '2';

    DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
  end;

  Result := IntToStr(Numero) + '9' + DigitoCodBarras + Copy(CodigoBarras, 5, 39);
end;

function TACBrBancoSafra.TipoOCorrenciaToCod(
  const TipoOcorrencia: TACBrTipoOcorrencia): string;
begin
  case TipoOcorrencia of
    toRetornoRegistroConfirmado                    : Result := '02';
    toRetornoRegistroRecusado                      : Result := '03';
    toRetornoTransferenciaCarteiraEntrada          : Result := '04';
    toRetornoTransferenciaCarteiraBaixa            : Result := '05';
    toRetornoLiquidado                             : Result := '06';
    toRetornoLiquidadoParcialmente                 : Result := '07';
    toRetornoBaixaAutomatica                       : Result := '09';
    toRetornoBaixadoInstAgencia                    : Result := '10';
    toRetornoTituloEmSer                           : Result := '11';
    toRetornoAbatimentoConcedido                   : Result := '12';
    toRetornoAbatimentoCancelado                   : Result := '13';
    toRetornoVencimentoAlterado                    : Result := '14';
    toRetornoLiquidadoEmCartorio                   : Result := '15';
    toRetornoBaixadoFrancoPagamento                : Result := '16';
    toRetornoEntradaBorderoManual                  : Result := '17';
    toRetornoAlteracaoUsoCedente                   : Result := '18';
    toRetornoRecebimentoInstrucaoProtestar         : Result := '19';
    toRetornoRecebimentoInstrucaoSustarProtesto    : Result := '20';
    toRetornoTransferenciaCedente                  : Result := '21';
    toRetornoEncaminhadoACartorio                  : Result := '23';
    toRetornoBaixaPorProtesto                      : Result := '40';
    toRetornoLiquidadoAposBaixaOuNaoRegistro       : Result := '41';
    toRetornoRetiradoDeCartorio                    : Result := '42';
    toRetornoDespesaCartorio                       : Result := '43';
    toRetornoDebitoDiretoAutorizado                : Result := '44';
    toRetornoDebitoDiretoNaoAutorizado             : Result := '45';
    toRetornoRecebimentoInstrucaoAlterarValorTitulo: Result := '51';
    toRetornoAlteracaoDataEmissao                  : Result := '52';
    toRetornoAlteracaoEspecie                      : Result := '53';
    toRetornoAlteracaoSeuNumero                    : Result := '54';
    toRetornoEqualizacaoVendor                     : Result := '60';
    toRetornoRecebimentoInstrucaoAlterarJuros      : Result := '77';
    else
      Result := '02';
  end;
end;

function TACBrBancoSafra.TipoOcorrenciaToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia): string;
var
  CodOcorrencia: integer;
begin
  CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia), 0);

  case CodOcorrencia of
    02: Result := '02-ENTRADA CONFIRMADA';
    03: Result := '03-ENTRADA REJEITADA';
    04: Result := '04-TRANSFERÊNCIA DE CARTEIRA (ENTRADA)';
    05: Result := '05-TRANSFERÊNCIA DE CARTEIRA (BAIXA)';
    06: Result := '06-LIQUIDAÇÃO NORMAL';
    07: Result := '07-LIQUIDAÇÃO PARCIAL';
    09: Result := '09-BAIXADO AUTOMATICAMENTE';
    10: Result := '10-BAIXADO CONFORME INSTRUÇÕES';
    11: Result := '11-TÍTULOS EM SER (PARA ARQUIVO MENSAL)';
    12: Result := '12-ABATIMENTO CONCEDIDO';
    13: Result := '13-ABATIMENTO CANCELADO';
    14: Result := '14-VENCIMENTO ALTERADO';
    15: Result := '15-LIQUIDAÇÃO EM CARTÓRIO';
    16: Result := '16-BAIXADO POR ENTREGA FRANCO DE PAGAMENTO';
    17: Result := '17-ENTRADA POR BORDERO MANUAL';
    18: Result := '18-ALTERACAO DE USO DO CEDENTE';
    19: Result := '19-CONFIRMAÇÃO DE INSTRUÇÃO DE PROTESTO';
    20: Result := '20-CONFIRMAÇÃO DE SUSTAR PROTESTO';
    21: Result := '21-TRANSFERÊNCIA DE CEDENTE';
    23: Result := '23-TÍTULO ENVIADO A CARTÓRIO';
    40: Result := '40-BAIXA DE TÍTULO PROTESTADO';
    41: Result := '41-LIQUIDAÇÃO DE TÍTULO BAIXADO';
    42: Result := '42-TÍTULO RETIRADO DO CARTÓRIO';
    43: Result := '43-DESPESA DE CARTÓRIO';
    44: Result := '44-ACEITE DO TÍTULO DDA PELO SACADO';
    45: Result := '45-NÃO ACEITE DO TÍTULO DDA PELO SACADO';
    51: Result := '51-VALOR DO TÍTULO ALTERADO';
    52: Result := '52-ACERTO DE DATA DE EMISSAO';
    53: Result := '53-ACERTO DE COD ESPECIE DOCTO';
    54: Result := '54-ALTERACAO DE SEU NUMERO';
    60: Result := '60-EQUALIZACAO VENDOR';
    77: Result := '77-ALT. INSTR. COBR. - JUROS';
  end;
end;

end.
