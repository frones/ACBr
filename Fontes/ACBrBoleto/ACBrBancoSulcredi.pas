{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:   Juliana Rodrigues Prado                       }
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

{$I ACBr.inc}

unit ACBrBancoSulcredi;

interface

uses
  Classes,
  SysUtils,
  ACBrBoleto,
  ACBrBoletoConversao,
  ACBrValidador,
  Contnrs;

type

  { TACBrBancoSulcredi }

  TACBrBancoSulcredi = class(TACBrBancoClass)
  private
  protected
  public
    constructor Create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCodigoBarras(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCampoNossoNumero(const ACBrTitulo:TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    function GerarRegistroHeader240(NumeroRemessa: Integer): String; override;
    function GerarRegistroTransacao240(ACBrTitulo: TACBrTitulo): String; override;
    function GerarRegistroTrailler240(ARemessa: TStringList): String; override;
    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;
    function TipoOcorrenciaToCodRemessa(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia: Integer): TACBrTipoOcorrencia; override;
    function TipoOCorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String; override;
    function DefineTipoCarteira(const ACBrTitulo: TACBrTitulo): String;
    function CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia; override;
    procedure LerRetorno240(ARetorno: TStringList); override;
  end;

implementation

uses
  {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5 {$ENDIF},
  StrUtils,
  ACBrUtil;

{ TACBrBancoSulcredi }

constructor TACBrBancoSulcredi.Create(AOwner: TACBrBanco);
begin
   inherited Create(AOwner);
   fpDigito := 0;
   fpNome := 'SULCREDI';
   fpNumero := 322;
   fpTamanhoMaximoNossoNum := 11;
   fpTamanhoAgencia := 4; 
   fpTamanhoConta := 5; 
   fpTamanhoCarteira := 2;
   fpDensidadeGravacao := '';
   fpLayoutVersaoArquivo := 103;
   fpLayoutVersaoLote := 060;
   fpQtdRegsLote := 0;
   fpQtdRegsCobranca := 0;
   fpVlrRegsCobranca := 0;
end;

function TACBrBancoSulcredi.DefineTipoCarteira(const ACBrTitulo: TACBrTitulo): String;
begin
  with ACBrTitulo do
  begin
    case ACBrBoleto.Cedente.TipoCarteira of
      tctSimples: Result := '2';
      tctRegistrada: Result := '1';
    else
      Result := '1';
    end;
  end;
end;

function TACBrBancoSulcredi.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo): String;
var
  LDocumento: String;

begin
  LDocumento := PadLeft(Trim(ACBrTitulo.ACBrBoleto.Cedente.Convenio), 7, '0') {Convênio: número do convênio de cobrança com sete dígitos}
    + '9'                                                                     {Origem: deve ser sempre 9, representa que o boleto foi gerado via arquivo CNAB}
    + PadLeft(ACBrTitulo.NossoNumero, fpTamanhoMaximoNossoNum, '0');          {NumeroSequencial: numero sequência do título dentro do convênio}

  //Digito: Cálculo em Mod10 das posições: {convenio}{origem}{numeroSequencial}
  Modulo.FormulaDigito := frModulo10;
  Modulo.MultiplicadorFinal := 1;
  Modulo.MultiplicadorInicial := 2;
  Modulo.Documento := LDocumento;
  Modulo.Calcular;

  Result := IntToStr(Modulo.DigitoFinal);
end;

function TACBrBancoSulcredi.MontarCodigoBarras(const ACBrTitulo: TACBrTitulo): String;
var
  CodigoBarras, FatorVencimento, DigitoCodBarras, CampoLivre: String;

begin
  with ACBrTitulo.ACBrBoleto do
  begin
    FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);

    CampoLivre := '01' + PadLeft(RemoveString('-', MontarCampoNossoNumero(ACBrTitulo)), 23, '0');

    CodigoBarras := IntToStr(Numero) + '9' + FatorVencimento +
                    IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10) +
                    CampoLivre;

    DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
  end;

  Result:= IntToStr(Numero) + '9' + DigitoCodBarras + Copy(CodigoBarras, 5, 39);
end;

function TACBrBancoSulcredi.MontarCampoNossoNumero(const ACBrTitulo: TACBrTitulo): String;
begin
  Result := PadLeft(Trim(ACBrTitulo.ACBrBoleto.Cedente.Convenio), 7, '0') +
            '9' +
            PadLeft(ACBrTitulo.NossoNumero, fpTamanhoMaximoNossoNum, '0') +
            '-' +
            CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrBancoSulcredi.MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String;
begin
  Result := ACBrTitulo.ACBrBoleto.Cedente.Agencia + '/' + ACBrTitulo.ACBrBoleto.Cedente.Conta;
end;

function TACBrBancoSulcredi.GerarRegistroHeader240(NumeroRemessa: Integer): String;
var
  ListHeader: TStringList;

begin
  Result := '';

  ListHeader:= TStringList.Create;
  try
    with ACBrBanco.ACBrBoleto.Cedente do
    begin
      { REGISTRO-HEADER DO ARQUIVO }

      ListHeader.Add(
        IntToStrZero(fpNumero, 3)                                         + // Código do Banco na Compensação
        '0000'                                                            + // Lote de Serviço
        '0'                                                               + // Tipo de Registro
        Space(9)                                                          + // Uso Exclusivo FEBRABAN / CNAB
        DefineTipoInscricao                                               + // Tipo de inscrição da empresa
        PadLeft(Trim(OnlyNumber(CNPJCPF)), 14, '0')                       + // Número de inscrição da empresa
        PadLeft(Convenio, 20, ' ')                                        + // Código do Convênio no Banco
        PadLeft(OnlyNumber(Agencia), 5, '0')                              + // Agência Mantenedora da Conta
        Space(1)                                                          + // Dígito Verificador da Agência: PadRight(AgenciaDigito, 1, ' ')
        PadLeft(OnlyNumber(Conta), 12, '0')                               + // Número da Conta Corrente
        PadRight(ContaDigito, 1, '0')                                     + // Dígito Verificador da Conta
        PadRight(DigitoVerificadorAgenciaConta, 1, ' ')                   + // Dígito Verificador da Ag/Conta
        PadRight(Nome, 30, ' ')                                           + // Nome da empresa
        PadRight(fpNome, 30, ' ')                                         + // Nome do banco
        Space(10)                                                         + // Uso Exclusivo FEBRABAN / CNAB
        '1'                                                               + // Código de Remessa (1) / Retorno (2)
        FormatDateTime('ddmmyyyy', Now)                                   + // Data de geração do arquivo
        FormatDateTime('hhmmss', Now)                                     + // Hora de geração do arquivo
        PadLeft(IntToStr(NumeroRemessa), 6, '0')                          + // Número seqüencial do arquivo
        PadLeft(IntToStr(fpLayoutVersaoArquivo), 3, '0')                  + // No da Versão do Layout do Arquivo
        PadLeft(fpDensidadeGravacao, 5, '0')                              + // Densidade de Gravação do Arquivo
        Space(20)                                                         + // Para Uso Reservado do Banco
        Space(20)                                                         + // Para Uso Reservado da Empresa
        Space(29)                                                           // Uso Exclusivo FEBRABAN / CNAB
      );

      { REGISTRO HEADER DO LOTE }

      ListHeader.Add(
        IntToStrZero(fpNumero, 3)                                         + // Código do Banco na Compensação
        '0001'                                                            + // Lote de Serviço
        '1'                                                               + // Tipo de Registro
        'R'                                                               + // Tipo de Operação
        '01'                                                              + // Tipo de Serviço
        Space(2)                                                          + // Uso Exclusivo FEBRABAN / CNAB
        PadLeft(IntToStr(fpLayoutVersaoLote), 3, '0')                     + // Nº da versão do layout do lote
        Space(1)                                                          + // Uso Exclusivo FEBRABAN / CNAB
        DefineTipoInscricao                                               + // Tipo de inscrição da empresa
        PadLeft(Trim(OnlyNumber(CNPJCPF)), 15, '0')                       + // Número de inscrição da empresa
        PadLeft(Convenio, 20, ' ')                                        + // Código do Convênio no Banco
        PadLeft(OnlyNumber(Agencia), 5, '0')                              + // Agência Mantenedora da Conta
        Space(1)                                                          + // Dígito Verificador da Agência: PadRight(AgenciaDigito, 1, ' ')
        PadLeft(OnlyNumber(Conta), 12, '0')                               + // Número da Conta Corrente
        PadRight(ContaDigito, 1, '0')                                     + // Dígito Verificador da Conta
        PadRight(DigitoVerificadorAgenciaConta, 1, ' ')                   + // Dígito Verificador da Ag/Conta
        PadRight(Nome, 30, ' ')                                           + // Nome da empresa
        Space(40)                                                         + // Mensagem 1
        Space(40)                                                         + // Mensagem 2
        PadLeft(IntToStr(NumeroRemessa), 8, '0')                          + // Número seqüencial do arquivo
        FormatDateTime('ddmmyyyy', Now)                                   + // Data de geração do arquivo
        IntToStrZero(0, 8)                                                + // Data do crédito
        Space(33)                                                           // Uso Exclusivo FEBRABAN / CNAB
      );
    end;

    Result := RemoverQuebraLinhaFinal(ListHeader.Text);

   finally
     ListHeader.Free;
   end;
end;

function TACBrBancoSulcredi.GerarRegistroTransacao240(ACBrTitulo: TACBrTitulo): String;
var
  ListTransacao: TStringList;
  sCodMovimento: String;
  sNossoNumero: String;
  sTipoCobranca: String;
  sTipoCarteira: String;
  sTipoDocto: String;
  sEspecie: String;
  sTipoJuros: String;
  sDataMoraJuros: String;
  sValorMoraDia: String;
  sTipoDesconto: String;
  sDataDesconto: String;
  sCodigoProtesto: String;
  sDiasProtesto: String;
  sCodigoBaixa: String;
  sDiasBaixa: String;
  sTipoInscricao: String;
  sEndereco: String;
  aTipoInscricao: String;
  sCodMulta: String;
  sDataMulta: String;

begin
  with ACBrTitulo do
  begin
    {Tipo de Ocorrencia}
    sCodMovimento := TipoOcorrenciaToCodRemessa(ACBrTitulo.OcorrenciaOriginal.Tipo);

    {Código da Carteira}
    sTipoCobranca := DefineCaracTitulo(ACBrTitulo);

    {Tipo Carteira}
    sTipoCarteira:= DefineTipoCarteira(ACBrTitulo);

    {Tipo Documento}
    sTipoDocto:= DefineTipoDocumento;

    {Especie Documento}
    sEspecie := DefineEspecieDoc(ACBrTitulo);

    {Código Mora}
    sTipoJuros := DefineCodigoMoraJuros(ACBrTitulo);

    {Data Mora}
    if (TruncTo(ValorMoraJuros, fpCasasDecimaisMoraJuros) > 0) then
      sDataMoraJuros := DefineDataMoraJuros(ACBrTitulo)
    else
    begin
      sDataMoraJuros := PadLeft('', 8, '0');
      sTipoJuros := '3'; // Isento
    end;

    {Valor Mora Dia}
    if (sTipoJuros = '2') then
      sValorMoraDia := FormatarMoraJurosRemessa(15, ACBrTitulo)
    else
      sValorMoraDia := IntToStrZero(Round(ValorMoraJuros * 100), 15);

    {Código Desconto}
    sTipoDesconto := DefineCodigoDesconto(ACBrTitulo);

    {Data Desconto}
    sDataDesconto := DefineDataDesconto(ACBrTitulo);

    {Código e Dias Protesto}
    if (DataProtesto <> 0) and (DiasDeProtesto > 0) then
    begin
      sCodigoProtesto := DefineTipoDiasProtesto(ACBrTitulo);
      sDiasProtesto := PadLeft(IntToStr(DiasDeProtesto), 2, '0');
    end
    else
    begin
      sCodigoProtesto := '3';
      sDiasProtesto := '00';
    end;

    {Código e Dias Baixa}
    if (DataBaixa <> 0) and (DataBaixa > Vencimento) then
    begin
      sCodigoBaixa := '1';
      sDiasBaixa := PadLeft(IntToStr(DaysBetween(DataBaixa, Vencimento)), 3, '0');
    end
    else
    begin
      sCodigoBaixa := Space(1);
      sDiasBaixa := Space(3);
    end;

    {Tipo de Inscrição}
    sTipoInscricao := Copy(DefineTipoSacado(ACBrTitulo), 2, 1);

    {Endereco do Sacado}
    sEndereco := PadRight(Sacado.Logradouro + ' ' + Sacado.Numero + ' ' + Sacado.Complemento , 40, ' ');

    {Tipo Inscrição do Avalista}
    aTipoInscricao := DefineTipoSacadoAvalista(ACBrTitulo);

    {Nosso Número Completo: (convenio)(origem)(numeroSequencial)(dv) }
    sNossoNumero := RemoveString('-', MontarCampoNossoNumero(ACBrTitulo));

    {Código Multa}
    sCodMulta := DefineCodigoMulta(ACBrTitulo);

    {Data Multa}
    sDataMulta := DefineDataMulta(ACBrTitulo);

    Inc(fpQtdRegsLote);
    Inc(fpQtdRegsCobranca);
    VlrRegsCobranca := VlrRegsCobranca + ValorDocumento;

    ListTransacao := TStringList.Create;
    try
      {SEGMENTO P}

      ListTransacao.Add(
        IntToStrZero(fpNumero, 3)                                                 + // Código do Banco na Compensação
        '0001'                                                                    + // Lote de Serviço
        '3'                                                                       + // Tipo de Registro
        IntToStrZero(fpQtdRegsLote, 5)                                            + // Nº Sequencial do Registro no Lote
        'P'                                                                       + // Cód. Segmento do Registro Detalhe
        Space(1)                                                                  + // Uso Exclusivo FEBRABAN/CNAB
        sCodMovimento                                                             + // Código de Movimento Remessa
        PadLeft(OnlyNumber(ACBrBoleto.Cedente.Agencia), 5, '0')                   + // Agência mantenedora da conta
        Space(1)                                                                  + // Digito agencia: PadLeft(ACBrBoleto.Cedente.AgenciaDigito, 1, ' ')
        PadLeft(OnlyNumber(ACBrBoleto.Cedente.Conta), 12, '0')                    + // Número da Conta Corrente
        PadLeft(ACBrBoleto.Cedente.ContaDigito, 1, '0')                           + // Dígito da Conta Corrente
        PadLeft(ACBrBoleto.Cedente.DigitoVerificadorAgenciaConta, 1, ' ')         + // Dígito Verificador da Ag/Conta
        PadLeft(sNossoNumero, 20, '0')                                            + // Identificação do título no Banco (Nosso Número)
        sTipoCobranca                                                             + // Código da carteira
        sTipoCarteira                                                             + // Forma de Cadastramento = 1 Registrada / 2 Sem Registro
        sTipoDocto                                                                + // Tipo de documento
        '2'                                                                       + // Identificação da Emissão do Boleto de Pagamento / 2 Cliente Emite
        '2'                                                                       + // Identificação da Distribuição / 2 Cliente distribui
        PadRight(Copy(NumeroDocumento, 1, 15), 15, ' ')                           + // Número do documento
        FormatDateTime('ddmmyyyy', Vencimento)                                    + // Data de vencimento do título
        IntToStrZero(Round(ValorDocumento * 100), 15)                             + // Valor nominal do título
        PadLeft('0', 5, '0')                                                      + // Agência encarregada da cobrança
        '0'                                                                       + // Dígito da Agência encarregada da cobrança
        sEspecie                                                                  + // Espécie do título
        IfThen(Aceite = atSim, 'A', 'N')                                          + // Identific. de Título Aceito/Não Aceito
        FormatDateTime('ddmmyyyy', DataDocumento)                                 + // Data da emissão do título
        sTipoJuros                                                                + // Código do juros de mora
        sDataMoraJuros                                                            + // Data do juros de mora
        sValorMoraDia                                                             + // Valor da mora/dia ou Taxa mensal
        sTipoDesconto                                                             + // Código do desconto 1
        sDataDesconto                                                             + // Data de desconto 1
        IntToStrZero(Round(ValorDesconto * 100), 15)                              + // Valor ou Percentual do desconto concedido
        IntToStrZero(Round(ValorIOF * 100), 15)                                   + // Valor do IOF a ser recolhido
        IntToStrZero(Round(ValorAbatimento * 100), 15)                            + // Valor do abatimento
        PadRight(SeuNumero, 25)                                                   + // Identificação do título na empresa
        sCodigoProtesto                                                           + // Código para protesto
        sDiasProtesto                                                             + // Número de dias para protesto
        sCodigoBaixa                                                              + // Código para Baixa/Devolução
        sDiasBaixa                                                                + // Número de dias para Baixa/Devolução
        '09'                                                                      + // Código da moeda
        IntToStrZero(0, 10)                                                       + // Nº do Contrato da Operação de Créd.
        '1'                                                                         // Uso livre banco/empresa / 1 – Não autoriza pagamento parcial 2 – Autoriza pagamentos parciais
      );

      Inc(fpQtdRegsLote);

      {SEGMENTO Q}

      ListTransacao.Add(
        IntToStrZero(fpNumero, 3)                                                 + // Código do Banco na Compensação
        '0001'                                                                    + // Lote de Serviço
        '3'                                                                       + // Tipo de Registro
        IntToStrZero(fpQtdRegsLote, 5)                                            + // Nº Sequencial do Registro no Lote
        'Q'                                                                       + // Cód. Segmento do Registro Detalhe
        Space(1)                                                                  + // Uso Exclusivo FEBRABAN/CNAB
        sCodMovimento                                                             + // Código de Movimento Remessa
        sTipoInscricao                                                            + // Tipo de inscrição
        PadLeft(trim(OnlyNumber(Sacado.CNPJCPF)), 15, '0')                        + // Número de inscrição do sacado
        PadRight(Trim(Sacado.NomeSacado), 40)                                     + // Nome sacado
        sEndereco                                                                 + // Endereço sacado
        PadRight(Trim(Sacado.Bairro), 15)                                         + // Bairro sacado
        PadLeft(Copy(OnlyNumber(Sacado.CEP), 1, 5), 5, '0')                       + // Cep sacado
        PadLeft(Copy(OnlyNumber(Sacado.CEP), 6, 3), 3, '0')                       + // Sufixo do Cep do sacado
        PadRight(Trim(Sacado.Cidade), 15)                                         + // Cidade do sacado
        PadRight(Sacado.UF, 2)                                                    + // Unidade da federação do
        aTipoInscricao                                                            + // Tipo de inscrição sacador/avalista
        PadLeft(Sacado.SacadoAvalista.CNPJCPF, 15, '0')                           + // Nº de inscrição sacador/avalista
        PadRight(Sacado.SacadoAvalista.NomeAvalista, 40, ' ')                     + // Nome do sacador/avalista
        PadRight('0', 3, '0')                                                     + // Cód. Bco. Corresp. na Compensação
        PadRight('', 20, ' ')                                                     + // Nosso Nº no Banco Correspondente
        PadRight('', 8, ' '));                                                      // FEBRABAN/CNAB 233 240 8 - Alfa Brancos G004

      Inc(fpQtdRegsLote);

      {SEGMENTO R}

      ListTransacao.Add(
        IntToStrZero(fpNumero, 3)                                                 + // Código do Banco na Compensação
        '0001'                                                                    + // Lote de Serviço
        '3'                                                                       + // Tipo de Registro
        IntToStrZero(fpQtdRegsLote, 5)                                            + // Nº Sequencial do Registro no Lote
        'R'                                                                       + // Cód. Segmento do Registro Detalhe
        Space(1)                                                                  + // Uso Exclusivo FEBRABAN/CNAB
        sCodMovimento                                                             + // Código de Movimento Remessa
        '0'                                                                       + // Código do Desconto 2
        IntToStrZero(0, 8)                                                        + // Data do Desconto 2
        IntToStrZero(0, 15)                                                       + // Valor/Percentual a ser concedido no Desconto 2
        '0'                                                                       + // Código do Desconto 3
        IntToStrZero(0, 8)                                                        + // Data do Desconto 3
        IntToStrZero(0, 15)                                                       + // Valor/Percentual a ser concedido no Desconto 3
        sCodMulta                                                                 + // Código da Multa
        sDataMulta                                                                + // Data da Multa
        IntToStrZero(Round(PercentualMulta * 100), 15)                            + // Valor/Percentual a ser aplicado na Multa
        Space(10)                                                                 + // Informação ao pagador
        Space(40)                                                                 + // Mensagem 3
        Space(40)                                                                 + // Mensagem 4
        Space(20)                                                                 + // Uso Exclusivo FEBRABAN/CNAB
        IntToStrZero(0, 8)                                                        + // Cód. Ocor. do Pagador
        IntToStrZero(0, 3)                                                        + // Cód. do Banco na Conta do Débito
        IntToStrZero(0, 5)                                                        + // Código da Agência do Débito
        Space(1)                                                                  + // Digito Verificador da Agência
        IntToStrZero(0, 12)                                                       + // Conta Corrente para Débito
        Space(1)                                                                  + // Digito Verificador da Conta
        Space(1)                                                                  + // Digito Verificador da Agência/Conta
        IntToStrZero(0, 1)                                                        + // Aviso para Débito Automático
        Space(9));                                                                  // Uso Exclusivo FEBRABAN/CNAB

      Result := RemoverQuebraLinhaFinal(ListTransacao.Text);

    finally
      ListTransacao.Free;
    end;
  end;
end;

function TACBrBancoSulcredi.GerarRegistroTrailler240(ARemessa: TStringList): String;
var
  ListTrailler: TStringList;

begin
  Result:= '';

  ListTrailler := TStringList.Create;
  try
    {REGISTRO TRAILER DO LOTE}

    ListTrailler.Add(
      IntToStrZero(fpNumero, 3)                                   + // Código do Banco na compensação
      '0001'                                                      + // Lote de serviço
      '5'                                                         + // Tipo de registro
      Space(9)                                                    + // Uso Exclusivo FEBRABAN / CNAB
      IntToStrZero(fpQtdRegsLote, 6)                              + // Quantidade de registros do lote
      IntToStrZero(fpQtdRegsCobranca, 6)                          + // Quantidade de Títulos em Cobrança
      IntToStrZero(Round(fpVlrRegsCobranca * 100), 17)            + // Valor Total dos Títulos em Carteiras
      IntToStrZero(0, 6)                                          + // Quantidade de Títulos em Cobrança
      IntToStrZero(0, 17)                                         + // Valor Total dos Títulos em Carteiras
      IntToStrZero(0, 6)                                          + // Quantidade de Títulos em Cobrança
      IntToStrZero(0, 17)                                         + // Valor Total dos Títulos em Carteiras
      IntToStrZero(0, 6)                                          + // Quantidade de Títulos em Cobrança
      IntToStrZero(0, 17)                                         + // Valor Total dos Títulos em Carteiras
      Space(8)                                                    + // Número do Aviso de Lançamento
      Space(117)                                                    // Uso Exclusivo FEBRABAN / CNAB
    );

    {REGISTRO TRAILER DO ARQUIVO}

    ListTrailler.Add(
      IntToStrZero(fpNumero, 3)                                   + // Código do Banco na compensação
      '9999'                                                      + // Lote de serviço
      '9'                                                         + // Tipo de registro
      Space(9)                                                    + // Uso Exclusivo FEBRABAN / CNAB
      '000001'                                                    + // Quantidade de lotes do arquivo
      IntToStrZero(fpQtdRegsLote + 4, 6)                          + // Quantidade de registros do arquivo
      '000000'                                                    + // Quantidade de contas
      Space(205)                                                    // Uso Exclusivo FEBRABAN / CNAB
    );

    Result := RemoverQuebraLinhaFinal(ListTrailler.Text);

  finally
    fpQtdRegsLote := 0;
    fpQtdRegsCobranca := 0;
    fpVlrRegsCobranca := 0;
    ListTrailler.Free;
  end;
end;

procedure TACBrBancoSulcredi.LerRetorno240(ARetorno: TStringList);
var
  Titulo: TACBrTitulo;
  TempData, Linha, rCedente, rCNPJCPF: String;
  ContLinha: Integer;
  idxMotivo: Integer;
  rConvenioCedente: String;

begin
  // Verifica se o arquivo pertence ao banco
  if StrToIntDef(copy(ARetorno.Strings[0], 1, 3), -1) <> Numero then
    raise Exception.create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno + 'não é um arquivo de retorno do ' + Nome));

  ACBrBanco.ACBrBoleto.DataArquivo := StringToDateTimeDef(Copy(ARetorno[0], 144, 2) + '/' +
                                                          Copy(ARetorno[0], 146, 2) + '/' +
                                                          Copy(ARetorno[0], 148, 4), 0, 'DD/MM/YYYY');

  ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0], 158, 6), 0);

  rCedente := Trim(Copy(ARetorno[0], 73, 30));
  rCNPJCPF := OnlyNumber(Copy(ARetorno[0], 19, 14));
  rConvenioCedente := Trim(Copy(ARetorno[0], 33, 20));

  with ACBrBanco.ACBrBoleto do
  begin
    if (not LeCedenteRetorno) and (rCNPJCPF <> OnlyNumber(Cedente.CNPJCPF)) then
       raise Exception.create(ACBrStr('CNPJ\CPF do arquivo inválido'));

    if LeCedenteRetorno then
    begin
      Cedente.Nome := rCedente;
      Cedente.CNPJCPF := rCNPJCPF;
      Cedente.Convenio := rConvenioCedente;
    end;

    case StrToIntDef(copy(ARetorno[0], 18, 1), 0) of
      01:
        Cedente.TipoInscricao := pFisica;
      else
        Cedente.TipoInscricao := pJuridica;
    end;

    ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
  end;

  for ContLinha := 1 to ARetorno.Count - 2 do
  begin
    Linha := ARetorno[ContLinha];

    if Copy(Linha, 8, 1) <> '3' then // Verifica se o registro (linha) é um registro detalhe (segmento J)
      Continue;

    if Copy(Linha, 14, 1) = 'T' then // Verifica se for segmento T cria um novo titulo
      Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

    with Titulo do
    begin
      if Copy(Linha, 14, 1) = 'T' then
      begin
        SeuNumero := Copy(Linha, 106, 25);
        NumeroDocumento := Copy(Linha, 59, 15);
        Carteira := Copy(Linha, 58, 1);

        TempData := Copy(Linha, 74, 2) + '/' + Copy(Linha, 76, 2) + '/' + Copy(Linha, 78, 4);
        if TempData <> '00/00/0000' then
          Vencimento := StringToDateTimeDef(TempData, 0, 'DDMMYYYY');

        ValorDocumento := StrToFloatDef(Copy(Linha, 82, 15), 0) / 100;

        NossoNumero := Copy(Linha, 42, 11);
        ValorDespesaCobranca := StrToFloatDef(Copy(Linha, 199, 15), 0) / 100;

        OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(StrToIntDef(Copy(Linha, 16, 2), 0));

        IdxMotivo := 214;

        while (IdxMotivo < 223) do
        begin
          if (Trim(Copy(Linha, IdxMotivo, 2)) <> '') then
          begin
            MotivoRejeicaoComando.Add(Copy(Linha, IdxMotivo, 2));
            DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo, StrToIntDef(Copy(Linha, IdxMotivo, 2), 0)));
          end;
          Inc(IdxMotivo, 2);
        end;

        // Quando o numero documento vier em branco
        if Trim(NumeroDocumento) = '' then
          NumeroDocumento := NossoNumero;
      end
      else // Segmento U
      begin
        ValorIOF            := StrToFloatDef(Copy(Linha, 63, 15), 0) / 100;
        ValorAbatimento     := StrToFloatDef(Copy(Linha, 48, 15), 0) / 100;
        ValorDesconto       := StrToFloatDef(Copy(Linha, 33, 15), 0) / 100;
        ValorMoraJuros      := StrToFloatDef(Copy(Linha, 18, 15), 0) / 100;
        ValorOutrosCreditos := StrToFloatDef(Copy(Linha, 123, 15), 0) / 100;
        ValorOutrasDespesas := StrToFloatDef(Copy(Linha, 108, 15), 0) / 100;
        ValorRecebido       := StrToFloatDef(Copy(Linha, 78, 15), 0) / 100;

        TempData := Copy(Linha, 138, 2) + '/' + Copy(Linha, 140, 2) + '/' + Copy(Linha, 142, 4);
        if TempData <> '00/00/0000' then
          DataOcorrencia := StringToDateTimeDef(TempData, 0, 'DDMMYYYY');

        TempData := Copy(Linha, 146, 2) + '/' + Copy(Linha, 148, 2) + '/' + Copy(Linha, 150, 4);
        if TempData <> '00/00/0000' then
          DataCredito := StringToDateTimeDef(TempData, 0, 'DDMMYYYY');
      end;
    end;
  end;
end;

function TACBrBancoSulcredi.TipoOcorrenciaToCodRemessa(const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  case TipoOcorrencia of
    toRemessaBaixar                         : Result := '02'; {Pedido de Baixa}
    toRemessaProtestoFinsFalimentares       : Result := '03';
    toRemessaConcederAbatimento             : Result := '04'; {Concessão de Abatimento}
    toRemessaCancelarAbatimento             : Result := '05'; {Cancelamento de Abatimento concedido}
    toRemessaAlterarVencimento              : Result := '06'; {Alteração de vencimento}
    toRemessaConcederDesconto               : Result := '07';
    toRemessaCancelarDesconto               : Result := '08';
    toRemessaProtestar                      : Result := '09'; {Pedido de protesto}
    toRemessaSustarProtestoBaixarTitulo     : Result := '10';
    toRemessaSustarProtestoManterCarteira   : Result := '11';
    toRemessaAlterarJurosMora               : Result := '12';
    toRemessaDispensarJuros                 : Result := '13';
    toRemessaAlterarMulta                   : Result := '14';
    toRemessaDispensarMulta                 : Result := '15';
    toRemessaAlterarDesconto                : Result := '16';
    toRemessaNaoConcederDesconto            : Result := '17';
    toRemessaAlterarValorAbatimento         : Result := '18';
    toRemessaAlterarPrazoLimiteRecebimento  : Result := '19';
    toRemessaDispensarPrazoLimiteRecebimento: Result := '20';
    toRemessaAlterarNumeroTituloBeneficiario: Result := '21';
    toRemessaAlterarNumeroControle          : Result := '22';
    toRemessaAlterarDadosPagador            : Result := '23';
    toRemessaAlterarDadosSacadorAvalista    : Result := '24';
    toRemessaRecusaAlegacaoPagador          : Result := '30';
    toRemessaAlterarOutrosDados             : Result := '31';
    toRemessaAlterarDadosRateioCredito      : Result := '33';
    toRemessaPedidoCancelamentoDadosRateioCredito : Result := '34';
    toRemessaPedidoDesagendamentoDebietoAutom     : Result := '35';
    toRemessaAlterarProtestoDevolucao       : Result := '40';
    toRemessaAlterarEspecieTitulo           : Result := '42';
    toRemessaTransferenciaCarteira          : Result := '43';
    toRemessaAlterarContratoCobran          : Result := '44';
    toRemessaNegativacaoSemProtesto         : Result := '45';
    toRemessaBaixaTituloNegativadoSemProtesto      : Result := '46';
    toRemessaAlterarValorTitulo             : Result := '47';
    toRemessaAlterarValorMinimo             : Result := '48';
    toRemessaAlterarValorMaximo             : Result := '49';
    toRemessaHibrido                        : Result := '61';
  else
    Result := '01';
  end;
end;

function TACBrBancoSulcredi.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
  CodOcorrencia: Integer;

begin
  CodOcorrencia := StrToIntDef(TipoOCorrenciaToCod(TipoOcorrencia),0);

  case CodOcorrencia of
    02: Result:='02-Entrada Confirmada' ;
    03: Result:='03-Entrada Rejeitada' ;
    06: Result:='06-Liquidação normal' ;
    09: Result:='09-Baixado Automaticamente via Arquivo' ;
    10: Result:='10-Baixado conforme instruções da Agência' ;
    11: Result:='11-Em Ser - Arquivo de Títulos pendentes' ;
    12: Result:='12-Abatimento Concedido' ;
    13: Result:='13-Abatimento Cancelado' ;
    14: Result:='14-Vencimento Alterado' ;
    15: Result:='15-Liquidação em Cartório' ;
    16: Result:= '16-Titulo Pago em Cheque - Vinculado';
    17: Result:='17-Liquidação após baixa ou Título não registrado' ;
    18: Result:='18-Acerto de Depositária' ;
    19: Result:='19-Confirmação Recebimento Instrução de Protesto' ;
    20: Result:='20-Confirmação Recebimento Instrução Sustação de Protesto' ;
    21: Result:='21-Acerto do Controle do Participante' ;
    22: Result:='22-Titulo com Pagamento Cancelado';
    23: Result:='23-Entrada do Título em Cartório' ;
    24: Result:='24-Entrada rejeitada por CEP Irregular' ;
    27: Result:='27-Baixa Rejeitada' ;
    28: Result:='28-Débito de tarifas/custas' ;
    29: Result:= '29-Ocorrências do Sacado';
    30: Result:='30-Alteração de Outros Dados Rejeitados' ;
    32: Result:='32-Instrução Rejeitada' ;
    33: Result:='33-Confirmação Pedido Alteração Outros Dados' ;
    34: Result:='34-Retirado de Cartório e Manutenção Carteira' ;
    35: Result:='35-Desagendamento do débito automático' ;
    40: Result:='40-Estorno de Pagamento';
    55: Result:='55-Sustado Judicial';
    68: Result:='68-Acerto dos dados do rateio de Crédito' ;
    69: Result:='69-Cancelamento dos dados do rateio' ;
  end;
end;

function TACBrBancoSulcredi.CodOcorrenciaToTipo(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02: Result := toRetornoRegistroConfirmado;
    03: Result := toRetornoRegistroRecusado;
    06: Result := toRetornoLiquidado;
    09: Result := toRetornoBaixadoViaArquivo;
    10: Result := toRetornoBaixadoInstAgencia;
    11: Result := toRetornoTituloEmSer;
    12: Result := toRetornoAbatimentoConcedido;
    13: Result := toRetornoAbatimentoCancelado;
    14: Result := toRetornoVencimentoAlterado;
    15: Result := toRetornoLiquidadoEmCartorio;
    16: Result := toRetornoTituloPagoEmCheque;
    17: Result := toRetornoLiquidadoAposBaixaouNaoRegistro;
    18: Result := toRetornoAcertoDepositaria;
    19: Result := toRetornoRecebimentoInstrucaoProtestar;
    20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
    21: Result := toRetornoAcertoControleParticipante;
    22: Result := toRetornoTituloPagamentoCancelado;
    23: Result := toRetornoEncaminhadoACartorio;
    24: Result := toRetornoEntradaRejeitaCEPIrregular;
    27: Result := toRetornoBaixaRejeitada;
    28: Result := toRetornoDebitoTarifas;
    29: Result := toRetornoOcorrenciasdoSacado;
    30: Result := toRetornoAlteracaoOutrosDadosRejeitada;
    32: Result := toRetornoComandoRecusado;
    33: Result := toRetornoRecebimentoInstrucaoAlterarDados;
    34: Result := toRetornoRetiradoDeCartorio;
    35: Result := toRetornoDesagendamentoDebitoAutomatico;
    99: Result := toRetornoRegistroRecusado;
  else
    Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoSulcredi.TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin
  case TipoOcorrencia of
    toRetornoRegistroConfirmado                : Result := '02';
    toRetornoRegistroRecusado                  : Result := '03';
    toRetornoLiquidado                         : Result := '06';
    toRetornoBaixadoViaArquivo                 : Result := '09';
    toRetornoBaixadoInstAgencia                : Result := '10';
    toRetornoTituloEmSer                       : Result := '11';
    toRetornoAbatimentoConcedido               : Result := '12';
    toRetornoAbatimentoCancelado               : Result := '13';
    toRetornoVencimentoAlterado                : Result := '14';
    toRetornoLiquidadoEmCartorio               : Result := '15';
    toRetornoTituloPagoEmCheque                : Result := '16';
    toRetornoLiquidadoAposBaixaouNaoRegistro   : Result := '17';
    toRetornoAcertoDepositaria                 : Result := '18';
    toRetornoRecebimentoInstrucaoProtestar     : Result := '19';
    toRetornoRecebimentoInstrucaoSustarProtesto: Result := '20';
    toRetornoAcertoControleParticipante        : Result := '21';
    toRetornoTituloPagamentoCancelado          : Result := '22';
    toRetornoEncaminhadoACartorio              : Result := '23';
    toRetornoEntradaRejeitaCEPIrregular        : Result := '24';
    toRetornoBaixaRejeitada                    : Result := '27';
    toRetornoDebitoTarifas                     : Result := '28';
    toRetornoOcorrenciasDoSacado               : Result := '29';
    toRetornoAlteracaoOutrosDadosRejeitada     : Result := '30';
    toRetornoComandoRecusado                   : Result := '32';
    { DONE -oJacinto -cAjuste : Acrescentar a ocorrência correta referente ao código. }
    toRetornoRecebimentoInstrucaoAlterarDados  : Result := '33';
    { DONE -oJacinto -cAjuste : Acrescentar a ocorrência correta referente ao código. }
    toRetornoRetiradoDeCartorio                : Result := '34';
    toRetornoDesagendamentoDebitoAutomatico    : Result := '35';
  else
    Result := '02';
  end;
end;

function TACBrBancoSulcredi.COdMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String;
begin
  case TipoOcorrencia of
    toRetornoRegistroConfirmado:
      case CodMotivo  of
        00: Result := '00-Ocorrencia aceita';
        01: Result := '01-Codigo de banco inválido';
        04: Result := '04-Cod. movimentacao nao permitido p/ a carteira';
        15: Result := '15-Caracteristicas de Cobranca Imcompativeis';
        17: Result := '17-Data de vencimento anterior a data de emissão';
        21: Result := '21-Espécie do Título inválido';
        24: Result := '24-Data da emissão inválida';
        38: Result := '38-Prazo para protesto inválido';
        39: Result := '39-Pedido para protesto não permitido para título';
        43: Result := '43-Prazo para baixa e devolução inválido';
        45: Result := '45-Nome do Sacado inválido';
        46: Result := '46-Tipo/num. de inscrição do Sacado inválidos';
        47: Result := '47-Endereço do Sacado não informado';
        48: Result := '48-CEP invalido';
        50: Result := '50-CEP referente a Banco correspondente';
        53: Result := '53-Nº de inscrição do Sacador/avalista inválidos (CPF/CNPJ)';
        54: Result := '54-Sacador/avalista não informado';
        67: Result := '67-Débito automático agendado';
        68: Result := '68-Débito não agendado - erro nos dados de remessa';
        69: Result := '69-Débito não agendado - Sacado não consta no cadastro de autorizante';
        70: Result := '70-Débito não agendado - Cedente não autorizado pelo Sacado';
        71: Result := '71-Débito não agendado - Cedente não participa da modalidade de débito automático';
        72: Result := '72-Débito não agendado - Código de moeda diferente de R$';
        73: Result := '73-Débito não agendado - Data de vencimento inválida';
        75: Result := '75-Débito não agendado - Tipo do número de inscrição do sacado debitado inválido';
        86: Result := '86-Seu número do documento inválido';
        89: Result := '89-Email sacado nao enviado - Titulo com debito automatico';
        90: Result := '90-Email sacado nao enviado - Titulo com cobranca sem registro';
      else
        Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

    toRetornoRegistroRecusado:
      case CodMotivo of
        02: Result:= '02-Codigo do registro detalhe invalido';
        03: Result:= '03-Codigo da Ocorrencia Invalida';
        04: Result:= '04-Codigo da Ocorrencia nao permitida para a carteira';
        05: Result:= '05-Codigo de Ocorrencia nao numerico';
        07: Result:= 'Agencia\Conta\Digito invalido';
        08: Result:= 'Nosso numero invalido';
        09: Result:= 'Nosso numero duplicado';
        10: Result:= 'Carteira invalida';
        13: Result:= 'Idetificacao da emissao do boleto invalida';
        16: Result:= 'Data de vencimento invalida';
        18: Result:= 'Vencimento fora do prazo de operacao';
        20: Result:= 'Valor do titulo invalido';
        21: Result:= 'Especie do titulo invalida';
        22: Result:= 'Especie nao permitida para a carteira';
        24: Result:= 'Data de emissao invalida';
        28: Result:= 'Codigo de desconto invalido';
        38: Result:= 'Prazo para protesto invalido';
        44: Result:= 'Agencia cedente nao prevista';
        45: Result:= 'Nome cedente nao informado';
        46: Result:= 'Tipo/numero inscricao sacado invalido';
        47: Result:= 'Endereco sacado nao informado';
        48: Result:= 'CEP invalido';
        50: Result:= 'CEP irregular - Banco correspondente';
        63: Result:= 'Entrada para titulo ja cadastrado';
        65: Result:= 'Limite excedido';
        66: Result:= 'Numero autorizacao inexistente';
        68: Result:= 'Debito nao agendado - Erro nos dados da remessa';
        69: Result:= 'Debito nao agendado - Sacado nao consta no cadastro de autorizante';
        70: Result:= 'Debito nao agendado - Cedente nao autorizado pelo sacado';
        71: Result:= 'Debito nao agendado - Cedente nao participa de debito automatico';
        72: Result:= 'Debito nao agendado - Codigo de moeda diferente de R$';
        73: Result:= 'Debito nao agendado - Data de vencimento invalida';
        74: Result:= 'Debito nao agendado - Conforme seu pedido titulo nao registrado';
        75: Result:= 'Debito nao agendado - Tipo de numero de inscricao de debitado invalido';
      else
        Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

    toRetornoLiquidado:
      case CodMotivo of
        00: Result:= '00-Titulo pago com dinheiro';
        15: Result:= '15-Titulo pago com cheque';
        42: Result:= '42-Rateio nao efetuado';
      else
        Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

    toRetornoBaixadoViaArquivo:
      case CodMotivo of
        00: Result:= '00-Ocorrencia aceita';
        10: Result:= '10=Baixa comandada pelo cliente';
      else
        Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

    toRetornoBaixadoInstAgencia:
      case CodMotivo of
        00: Result:= '00-Baixado conforme instrucoes na agencia';
        14: Result:= '14-Titulo protestado';
        15: Result:= '15-Titulo excluido';
        16: Result:= '16-Titulo baixado pelo banco por decurso de prazo';
        20: Result:= '20-Titulo baixado e transferido para desconto';
      else
        Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

    toRetornoLiquidadoAposBaixaouNaoRegistro:
      case CodMotivo of
        00: Result:= '00-Pago com dinheiro';
        15: Result:= '15-Pago com cheque';
      else
        Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

    toRetornoLiquidadoEmCartorio:
      case CodMotivo of
        00: Result:= '00-Pago com dinheiro';
        15: Result:= '15-Pago com cheque';
      else
        Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

    toRetornoEntradaRejeitaCEPIrregular:
      case CodMotivo of
        48: Result:= '48-CEP invalido';
      else
        Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

    toRetornoBaixaRejeitada:
      case CodMotivo of
        04: Result:= '04-Codigo de ocorrencia nao permitido para a carteira';
        07: Result:= '07-Agencia\Conta\Digito invalidos';
        08: Result:= '08-Nosso numero invalido';
        10: Result:= '10-Carteira invalida';
        15: Result:= '15-Carteira\Agencia\Conta\NossoNumero invalidos';
        40: Result:= '40-Titulo com ordem de protesto emitido';
        42: Result:= '42-Codigo para baixa/devolucao via Telebradesco invalido';
        60: Result:= '60-Movimento para titulo nao cadastrado';
        77: Result:= '70-Transferencia para desconto nao permitido para a carteira';
        85: Result:= '85-Titulo com pagamento vinculado';
      else
        Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

    toRetornoDebitoTarifas:
      case CodMotivo of
        02: Result:= '02-Tarifa de permanência título cadastrado';
        03: Result:= '03-Tarifa de sustação';
        04: Result:= '04-Tarifa de protesto';
        05: Result:= '05-Tarifa de outras instrucoes';
        06: Result:= '06-Tarifa de outras ocorrências';
        08: Result:= '08-Custas de protesto';
        12: Result:= '12-Tarifa de registro';
        13: Result:= '13-Tarifa titulo pago no Bradesco';
        14: Result:= '14-Tarifa titulo pago compensacao';
        15: Result:= '15-Tarifa título baixado não pago';
        16: Result:= '16-Tarifa alteracao de vencimento';
        17: Result:= '17-Tarifa concessão abatimento';
        18: Result:= '18-Tarifa cancelamento de abatimento';
        19: Result:= '19-Tarifa concessão desconto';
        20: Result:= '20-Tarifa cancelamento desconto';
        21: Result:= '21-Tarifa título pago cics';
        22: Result:= '22-Tarifa título pago Internet';
        23: Result:= '23-Tarifa título pago term. gerencial serviços';
        24: Result:= '24-Tarifa título pago Pág-Contas';
        25: Result:= '25-Tarifa título pago Fone Fácil';
        26: Result:= '26-Tarifa título Déb. Postagem';
        27: Result:= '27-Tarifa impressão de títulos pendentes';
        28: Result:= '28-Tarifa título pago BDN';
        29: Result:= '29-Tarifa título pago Term. Multi Funcao';
        30: Result:= '30-Impressão de títulos baixados';
        31: Result:= '31-Impressão de títulos pagos';
        32: Result:= '32-Tarifa título pago Pagfor';
        33: Result:= '33-Tarifa reg/pgto – guichê caixa';
        34: Result:= '34-Tarifa título pago retaguarda';
        35: Result:= '35-Tarifa título pago Subcentro';
        36: Result:= '36-Tarifa título pago Cartao de Credito';
        37: Result:= '37-Tarifa título pago Comp Eletrônica';
        38: Result:= '38-Tarifa título Baix. Pg. Cartorio';
        39: Result:='39-Tarifa título baixado acerto BCO';
        40: Result:='40-Baixa registro em duplicidade';
        41: Result:='41-Tarifa título baixado decurso prazo';
        42: Result:='42-Tarifa título baixado Judicialmente';
        43: Result:='43-Tarifa título baixado via remessa';
        44: Result:='44-Tarifa título baixado rastreamento';
        45: Result:='45-Tarifa título baixado conf. Pedido';
        46: Result:='46-Tarifa título baixado protestado';
        47: Result:='47-Tarifa título baixado p/ devolucao';
        48: Result:='48-Tarifa título baixado franco pagto';
        49: Result:='49-Tarifa título baixado SUST/RET/CARTÓRIO';
        50: Result:='50-Tarifa título baixado SUS/SEM/REM/CARTÓRIO';
        51: Result:='51-Tarifa título transferido desconto';
        52: Result:='52-Cobrado baixa manual';
        53: Result:='53-Baixa por acerto cliente';
        54: Result:='54-Tarifa baixa por contabilidade';
        55: Result:='55-BIFAX';
        56: Result:='56-Consulta informações via internet';
        57: Result:='57-Arquivo retorno via internet';
        58: Result:='58-Tarifa emissão Papeleta';
        59: Result:='59-Tarifa fornec papeleta semi preenchida';
        60: Result:='60-Acondicionador de papeletas (RPB)S';
        61: Result:='61-Acond. De papelatas (RPB)s PERSONAL';
        62: Result:='62-Papeleta formulário branco';
        63: Result:='63-Formulário A4 serrilhado';
        64: Result:='64-Fornecimento de softwares transmiss';
        65: Result:='65-Fornecimento de softwares consulta';
        66: Result:='66-Fornecimento Micro Completo';
        67: Result:='67-Fornecimento MODEN';
        68: Result:='68-Fornecimento de máquina FAX';
        69: Result:='69-Fornecimento de maquinas oticas';
        70: Result:='70-Fornecimento de Impressoras';
        71: Result:='71-Reativação de título';
        72: Result:='72-Alteração de produto negociado';
        73: Result:='73-Tarifa emissao de contra recibo';
        74: Result:='74-Tarifa emissao 2ª via papeleta';
        75: Result:='75-Tarifa regravação arquivo retorno';
        76: Result:='76-Arq. Títulos a vencer mensal';
        77: Result:='77-Listagem auxiliar de crédito';
        78: Result:='78-Tarifa cadastro cartela instrução permanente';
        79: Result:='79-Canalização de Crédito';
        80: Result:='80-Cadastro de Mensagem Fixa';
        81: Result:='81-Tarifa reapresentação automática título';
        82: Result:='82-Tarifa registro título déb. Automático';
        83: Result:='83-Tarifa Rateio de Crédito';
        84: Result:='84-Emissão papeleta sem valor';
        85: Result:='85-Sem uso';
        86: Result:='86-Cadastro de reembolso de diferença';
        87: Result:='87-Relatório fluxo de pagto';
        88: Result:='88-Emissão Extrato mov. Carteira';
        89: Result:='89-Mensagem campo local de pagto';
        90: Result:='90-Cadastro Concessionária serv. Publ.';
        91: Result:='91-Classif. Extrato Conta Corrente';
        92: Result:='92-Contabilidade especial';
        93: Result:='93-Realimentação pagto';
        94: Result:='94-Repasse de Créditos';
        95: Result:='95-Tarifa reg. pagto Banco Postal';
        96: Result:='96-Tarifa reg. Pagto outras mídias';
        97: Result:='97-Tarifa Reg/Pagto – Net Empresa';
        98: Result:='98-Tarifa título pago vencido';
        99: Result:='99-TR Tít. Baixado por decurso prazo';
        100: Result:='100-Arquivo Retorno Antecipado';
        101: Result:='101-Arq retorno Hora/Hora';
        102: Result:='102-TR. Agendamento Déb Aut';
        103: Result:='103-TR. Tentativa cons Déb Aut';
        104: Result:='104-TR Crédito on-line';
        105: Result:='105-TR. Agendamento rat. Crédito';
        106: Result:='106-TR Emissão aviso rateio';
        107: Result:='107-Extrato de protesto';
        110: Result:='110-Tarifa reg/pagto Bradesco Expresso';
      else
        Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

    toRetornoOcorrenciasdoSacado:
      case CodMotivo of
        78 : Result:= '78-Sacado alega que faturamento e indevido';
        116: Result:= '116-Sacado aceita/reconhece o faturamento';
      else
        Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

    toRetornoALteracaoOutrosDadosRejeitada:
      case CodMotivo of
        01: Result:= '01-Código do Banco inválido';
        04: Result:= '04-Código de ocorrência não permitido para a carteira';
        05: Result:= '05-Código da ocorrência não numérico';
        08: Result:= '08-Nosso número inválido';
        15: Result:= '15-Característica da cobrança incompatível';
        16: Result:= '16-Data de vencimento inválido';
        17: Result:= '17-Data de vencimento anterior a data de emissão';
        18: Result:= '18-Vencimento fora do prazo de operação';
        24: Result:= '24-Data de emissão Inválida';
        26: Result:= '26-Código de juros de mora inválido';
        27: Result:= '27-Valor/taxa de juros de mora inválido';
        28: Result:= '28-Código de desconto inválido';
        29: Result:= '29-Valor do desconto maior/igual ao valor do Título';
        30: Result:= '30-Desconto a conceder não confere';
        31: Result:= '31-Concessão de desconto já existente ( Desconto anterior )';
        32: Result:= '32-Valor do IOF inválido';
        33: Result:= '33-Valor do abatimento inválido';
        34: Result:= '34-Valor do abatimento maior/igual ao valor do Título';
        38: Result:= '38-Prazo para protesto inválido';
        39: Result:= '39-Pedido de protesto não permitido para o Título';
        40: Result:= '40-Título com ordem de protesto emitido';
        42: Result:= '42-Código para baixa/devolução inválido';
        46: Result:= '46-Tipo/número de inscrição do sacado inválidos';
        48: Result:= '48-Cep Inválido';
        53: Result:= '53-Tipo/Número de inscrição do sacador/avalista inválidos';
        54: Result:= '54-Sacador/avalista não informado';
        57: Result:= '57-Código da multa inválido';
        58: Result:= '58-Data da multa inválida';
        60: Result:= '60-Movimento para Título não cadastrado';
        79: Result:= '79-Data de Juros de mora Inválida';
        80: Result:= '80-Data do desconto inválida';
        85: Result:= '85-Título com Pagamento Vinculado.';
        88: Result:= '88-E-mail Sacado não lido no prazo 5 dias';
        91: Result:= '91-E-mail sacado não recebido';
      else
        Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

    toRetornoComandoRecusado:
      case CodMotivo of
        01 : Result:= '01-Código do Banco inválido';
        02 : Result:= '02-Código do registro detalhe inválido';
        04 : Result:= '04-Código de ocorrência não permitido para a carteira';
        05 : Result:= '05-Código de ocorrência não numérico';
        07 : Result:= '07-Agência/Conta/dígito inválidos';
        08 : Result:= '08-Nosso número inválido';
        10 : Result:= '10-Carteira inválida';
        15 : Result:= '15-Características da cobrança incompatíveis';
        16 : Result:= '16-Data de vencimento inválida';
        17 : Result:= '17-Data de vencimento anterior a data de emissão';
        18 : Result:= '18-Vencimento fora do prazo de operação';
        20 : Result:= '20-Valor do título inválido';
        21 : Result:= '21-Espécie do Título inválida';
        22 : Result:= '22-Espécie não permitida para a carteira';
        24 : Result:= '24-Data de emissão inválida';
        28 : Result:= '28-Código de desconto via Telebradesco inválido';
        29 : Result:= '29-Valor do desconto maior/igual ao valor do Título';
        30 : Result:= '30-Desconto a conceder não confere';
        31 : Result:= '31-Concessão de desconto - Já existe desconto anterior';
        33 : Result:= '33-Valor do abatimento inválido';
        34 : Result:= '34-Valor do abatimento maior/igual ao valor do Título';
        36 : Result:= '36-Concessão abatimento - Já existe abatimento anterior';
        38 : Result:= '38-Prazo para protesto inválido';
        39 : Result:= '39-Pedido de protesto não permitido para o Título';
        40 : Result:= '40-Título com ordem de protesto emitido';
        41 : Result:= '41-Pedido cancelamento/sustação para Título sem instrução de protesto';
        42 : Result:= '42-Código para baixa/devolução inválido';
        45 : Result:= '45-Nome do Sacado não informado';
        46 : Result:= '46-Tipo/número de inscrição do Sacado inválidos';
        47 : Result:= '47-Endereço do Sacado não informado';
        48 : Result:= '48-CEP Inválido';
        50 : Result:= '50-CEP referente a um Banco correspondente';
        53 : Result:= '53-Tipo de inscrição do sacador avalista inválidos';
        60 : Result:= '60-Movimento para Título não cadastrado';
        85 : Result:= '85-Título com pagamento vinculado';
        86 : Result:= '86-Seu número inválido';
        94 : Result:= '94-Título Penhorado – Instrução Não Liberada pela Agência';
      else
        Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;

    toRetornoDesagendamentoDebitoAutomatico:
      case CodMotivo of
        81 : Result:= '81-Tentativas esgotadas, baixado';
        82 : Result:= '82-Tentativas esgotadas, pendente';
        83 : Result:= '83-Cancelado pelo Sacado e Mantido Pendente, conforme negociação';
        84 : Result:= '84-Cancelado pelo sacado e baixado, conforme negociação';
      else
        Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
      end;
   else
      Result:= IntToStrZero(CodMotivo,2) +' - Outros Motivos';
   end;
end;

function TACBrBancoSulcredi.CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02 : Result:= toRemessaBaixar;                          {Pedido de Baixa}
    03 : Result:= toRemessaProtestoFinsFalimentares;        {Pedido de Protesto Falimentar}
    04 : Result:= toRemessaConcederAbatimento;              {Concessão de Abatimento}
    05 : Result:= toRemessaCancelarAbatimento;              {Cancelamento de Abatimento concedido}
    06 : Result:= toRemessaAlterarVencimento;               {Alteração de vencimento}
    07 : Result:= toRemessaConcederDesconto;                {Concessão de desconto}
    08 : Result:= toRemessaCancelarDesconto;                {Cancelamento de desconto}
    09 : Result:= toRemessaProtestar;                       {Pedido de protesto}
    10 : Result:= toRemessaSustarProtestoBaixarTitulo;      {Sustar Protesto e Baixar Título}
    11 : Result:= toRemessaSustarProtestoManterCarteira;    {Sustar Protesto e Manter em Carteira}
    12 : Result:= toRemessaAlterarJurosMora;                {Alteração de Juros de Mora}
    13 : Result:= toRemessaDispensarJuros;                  {Dispensar Cobrança de Juros de Mora}
    14 : Result:= toRemessaAlterarMulta;                    {Alteração de Valor/Percentual de Multa}
    15 : Result:= toRemessaDispensarMulta;                  {Dispensar Cobrança de Multa}
    16 : Result:= toRemessaAlterarDesconto;                 {Alteração de Valor/Data de Desconto}
    17 : Result:= toRemessaNaoConcederDesconto;             {Não conceder Desconto}
    18 : Result:= toRemessaAlterarValorAbatimento;          {Alteração do Valor de Abatimento}
    19 : Result:= toRemessaAlterarPrazoLimiteRecebimento;   {Prazo Limite de Recebimento - Alterar}
    20 : Result:= toRemessaDispensarPrazoLimiteRecebimento; {Prazo Limite de Recebimento - Dispensar}
    21 : Result:= toRemessaAlterarNumeroTituloBeneficiario; {Alterar número do título dado pelo Beneficiário}
    22 : Result:= toRemessaAlterarNumeroControle;           {Alterar número controle do Participante}
    23 : Result:= toRemessaAlterarDadosPagador;             {Alterar dados do Pagador}
    24 : Result:= toRemessaAlterarDadosSacadorAvalista;     {Alterar dados do Sacador/Avalista}
    30 : Result:= toRemessaRecusaAlegacaoPagador;           {Recusa da Alegação do Pagador}
    31 : Result:= toRemessaAlterarOutrosDados;              {Alteração de Outros Dados}
    33 : Result:= toRemessaAlterarDadosRateioCredito;       {Alteração dos Dados do Rateio de Crédito}
    34 : Result:= toRemessaPedidoCancelamentoDadosRateioCredito; {Pedido de Cancelamento dos Dados do Rateio de Crédito}
    35 : Result:= toRemessaPedidoDesagendamentoDebietoAutom;{Pedido de Desagendamento do Débito Automático}
    40 : Result:= toRemessaAlterarProtestoDevolucao;        {Alteração de Carteira "41" = Cancelar protesto}
    42 : Result:= toRemessaAlterarEspecieTitulo;            {Alteração de Espécie de Título}
    43 : Result:= toRemessaTransferenciaCarteira;           {Transferência de carteira/modalidade de cobrança}
    44 : Result:= toRemessaAlterarContratoCobran;           {Alteração de contrato de cobrança}
    45 : Result:= toRemessaNegativacaoSemProtesto;          {Negativação Sem Protesto}
    46 : Result:= toRemessaBaixaTituloNegativadoSemProtesto;{Solicitação de Baixa de Título Negativado Sem Protesto}
    47 : Result:= toRemessaAlterarValorTitulo;              {Alteração do Valor Nominal do Título}
    48 : Result:= toRemessaAlterarValorMinimo;              {Alteração do Valor Mínimo/ Percentual}
    49 : Result:= toRemessaAlterarValorMaximo;              {Alteração do Valor Máximo/Percentual}
    61 : Result:= toRemessaHibrido;                         {Alteração para inclusão/manutenção de QR Code Pix}
  else
     Result:= toRemessaRegistrar;                           {Remessa}
  end;
end;

end.


