{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: João Elson, Jéter Rabelo Ferreira               }
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

unit ACBrBancoCaixaSICOB;

interface

uses
  Classes, SysUtils, Contnrs, ACBrBoleto, ACBrBoletoConversao;

type

  { TACBrCaixaEconomicaSICOB}

  TACBrCaixaEconomicaSICOB = class(TACBrBancoClass)
   protected
    function GetLocalPagamento: String; override;
    procedure EhObrigatorioAgenciaDV; override;
   private
    function FormataNossoNumero(const ACBrTitulo :TACBrTitulo): String;
    function CalcularDVAgCD(Header: Boolean = False): string;
    function RetornaCodCarteira(const Carteira : string): integer;
   public
    Constructor create(AOwner: TACBrBanco);
    function CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String; override;
    function CalcularDVCedente(const ACBrTitulo: TACBrTitulo; UsaAgencia: boolean = false ): String;
    function MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String; override;
    function MontarCampoCodigoCedente(const ACBrTitulo: TACBrTitulo): String; override;
    function MontarCampoNossoNumero(const ACBrTitulo :TACBrTitulo): String; override;
    function GerarRegistroHeader240(NumeroRemessa : Integer): String; override;
    function GerarRegistroTransacao240(ACBrTitulo : TACBrTitulo): String; override;
    function GerarRegistroTrailler240(ARemessa : TStringList): String;  override;
    procedure LerRetorno240(ARetorno:TStringList); override;

    procedure GerarRegistroHeader400(NumeroRemessa : Integer; aRemessa:TStringList); override;
    procedure GerarRegistroTransacao400(ACBrTitulo : TACBrTitulo; aRemessa: TStringList); override;
    procedure GerarRegistroTrailler400(ARemessa : TStringList);  override;
    procedure LerRetorno400(ARetorno: TStringList); override;

    function TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia) : String; override;
    function CodOcorrenciaToTipo(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
    function TipoOcorrenciaToCod(const TipoOcorrencia: TACBrTipoOcorrencia):String; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia:TACBrTipoOcorrencia; CodMotivo:Integer): String; override;

    function CalcularTamMaximoNossoNumero(const Carteira : String; const NossoNumero : String = ''; const Convenio: String = ''): Integer; override;

    function CodOcorrenciaToTipoRemessa(const CodOcorrencia:Integer): TACBrTipoOcorrencia; override;
   end;

implementation

uses StrUtils, Variants, math,
     {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, FileCtrl {$ENDIF},
     ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrUtil.DateTime;

constructor TACBrCaixaEconomicaSICOB.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpDigito                := 0;
   fpNome                  := 'Caixa Economica Federal';
   fpNumero                := 104;
   fpTamanhoMaximoNossoNum := 0;
   fpTamanhoAgencia        := 5;
   fpTamanhoConta          := 8;

   fpOrientacoesBanco.Clear;
   fpOrientacoesBanco.Add(ACBrStr('SAC CAIXA: 0800 726 0101 (informações, reclamações, sugestões e elogios) ' + sLineBreak+
                          'Para pessoas com deficiência auditiva ou de fala: 0800 726 2492 ' + sLineBreak +
                          'Ouvidoria: 0800 725 7474') + sLineBreak+
                          '     caixa.gov.br      ');
end;

procedure TACBrCaixaEconomicaSICOB.EhObrigatorioAgenciaDV;
begin
  //sem validação
end;

function TACBrCaixaEconomicaSICOB.CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo ): String;
var
  Num, Res :String;
begin
   Result := '0';
   Num := OnlyNumber(FormataNossoNumero(ACBrTitulo));
   Modulo.CalculoPadrao;
   Modulo.MultiplicadorFinal   := 2;
   Modulo.MultiplicadorInicial := 9;
   Modulo.Documento := Num;
   Modulo.Calcular;

   Res:= IntToStr(Modulo.ModuloFinal);

   if Length(Res) > 1 then
      Result := '0'
   else
      Result := Res[1];
end;

function TACBrCaixaEconomicaSICOB.CalcularDVAgCD(Header: Boolean): string;
var
  Num, ACedente, Res :String;
begin
  Result := '0';
  with ACBrBanco.ACBrBoleto.Cedente do
  begin
    // Retirar o código da operácão do'código do cedetnet,
    // sempre com 3 digitos, ex: 870
    // Sem o DV
    if Header then
      ACedente := PadLeft(RightStr(CodigoCedente,8), 8, '0')
    else
      ACedente := PadLeft(RightStr(CodigoCedente,8), 12, '0');

    Num := Agencia + ACedente;
    Modulo.CalculoPadrao;
    Modulo.MultiplicadorFinal   := 9;
    Modulo.MultiplicadorInicial := 2;
    Modulo.Documento := Num;
    Modulo.Calcular;

    Res:= IntToStr(Modulo.DigitoFinal);
    if Length(Res) > 1 then
       Result := '0'
    else
       Result := Res[1];
  end;
end;

function TACBrCaixaEconomicaSICOB.CalcularDVCedente(const ACBrTitulo: TACBrTitulo;
   UsaAgencia: boolean = false): String;
var
  Num, Res: string;
begin
  if UsaAgencia then
     Num:=  RightStr(ACBrTitulo.ACBrBoleto.Cedente.Agencia,4) +
            Copy(ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente,
                 Length(ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente)-10,11)
  else
     Num := copy(ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente,
                 Length(ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente)-7,8);

  Modulo.CalculoPadrao;
  Modulo.MultiplicadorFinal   := 9;
  Modulo.MultiplicadorInicial := 2;
  Modulo.Documento := Num;
  Modulo.Calcular;
  Res := intTostr(Modulo.DigitoFinal);

  if Length(Res) > 1 then
     Result := '0'
  else
     Result := Res[1];
end;

function TACBrCaixaEconomicaSICOB.CodMotivoRejeicaoToDescricao(
  const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: Integer): String;
begin
   case TipoOcorrencia of
      toRetornoRegistroConfirmado,
      toRetornoRegistroRecusado,
      toRetornoInstrucaoRejeitada,
      toRetornoALteracaoOutrosDadosRejeitada:
      case CodMotivo of
        01: Result := '01-Código do banco inválido';
        02: Result := '02-Código do registro inválido.';
        03: Result := '03-Código do segmento inválido.';
        04: Result := '04-Transferência de Carteira/Entrada.';
        05: Result := '05-Código de movimento inválido.';
        06: Result := '06-Tipo/número de inscrição do cedente inválido.';
        07: Result := '07-Agência/Conta/DV inválido.';
        08: Result := '08-Nosso número inválido.';
        09: Result := '09-Nosso número duplicado.';
        10: Result := '10-Carteira inválida.';
        11: Result := '11-Forma de cadastramento do título inválido.';
        12: Result := '12-Tipo de documento inválido.';
        13: Result := '13-Identificação da emissão do bloqueto inválida.';
        14: Result := '14-Identificação da distribuição do bloqueto inválida.';
        15: Result := '15-Características da cobrança incompatíveis.';
        16: Result := '16-Data de vencimento inválida.';
        20: Result := '20-Valor do título inválido.';
        21: Result := '21-Espécie do título inválida.';
        23: Result := '23-Aceite inválido.';
        24: Result := '24-Data da emissão inválida.';
        26: Result := '26-Código de juros de mora inválido.';
        27: Result := '27-Valor/Taxa de juros de mora inválido.';
        28: Result := '28-Código do desconto inválido.';
        29: Result := '29-Valor do desconto maior ou igual ao valor do título.';
        30: Result := '30-Desconto a conceder não confere.';
        32: Result := '32-Valor do IOF inválido.';
        33: Result := '33-Valor do abatimento inválido.';
        37: Result := '37-Código para protesto inválido.';
        38: Result := '38-Prazo para protesto inválido.';
        40: Result := '40-Título com ordem de protesto emitida.';
        42: Result := '42-Código para baixa/devolução inválido.';
        43: Result := '43-Prazo para baixa/devolução inválido.';
        44: Result := '44-Código da moeda inválido.';
        45: Result := '45-Nome do sacado não informado.';
        46: Result := '46-Tipo/número de inscrição do sacado inválido.';
        47: Result := '47-Endereço do sacado não informado.';
        48: Result := '48-CEP inválido.';
        49: Result := '49-CEP sem praça de cobrança (não localizado).';
        52: Result := '52-Unidade da federação inválida.';
        53: Result := '53-Tipo/número de inscrição do sacador/avalista inválido.';
        57: Result := '57-Código da multa inválido.';
        58: Result := '58-Data da multa inválida.';
        59: Result := '59-Valor/Percentual da multa inválido.';
        60: Result := '60-Movimento para título não cadastrado. Erro genérico para as situações:' + #13#10 +
                          '“Cedente não cadastrado” ou' + #13#10 +
                          '“Agência Cedente não cadastrada ou desativada”.';
        61: Result := '61-Agência cobradora inválida.';
        62: Result := '62-Tipo de impressão inválido.';
        63: Result := '63-Entrada para título já cadastrado.';
        68: Result := '68-Movimentação inválida para o título.';
        69: Result := '69-Alteração de dados inválida.';
        70: Result := '70-Apelido do cliente não cadastrado.';
        71: Result := '71-Erro na composição do arquivo.';
        72: Result := '72-Lote de serviço inválido.';
        73: Result := '73-Código do cedente inválido.';
        74: Result := '74-Cedente não pertence a cobrança eletrônica/apelido não confere com cedente.';
        75: Result := '75-Nome da empresa inválido.';
        76: Result := '76-Nome do banco inválido.';
        77: Result := '77-Código da remessa inválido';
        78: Result := '78-Data/Hora de geração do arquivo inválida.';
        79: Result := '79-Número seqüencial do arquivo inválido.';
        80: Result := '80-Número da versão do Layout do arquivo/lote inválido.';
        81: Result := '81-Literal ‘REMESSA-TESTE’ válida somente para fase de testes.';
        82: Result := '82-Literal ‘REMESSA-TESTE’ obrigatório para fase de testes.';
        83: Result := '83-Tipo/número de inscrição da empresa inválido.';
        84: Result := '84-Tipo de operação inválido.';
        85: Result := '85-Tipo de serviço inválido.';
        86: Result := '86-Forma de lançamento inválido.';
        87: Result := '87-Número da remessa inválido.';
        88: Result := '88-Número da remessa menor/igual que da remessa anterior.';
        89: Result := '89-Lote de serviço divergente.';
        90: Result := '90-Número seqüencial do registro inválido.';
        91: Result := '91-Erro na seqüência de segmento do registro detalhe.';
        92: Result := '92-Código de movimento divergente entre grupo de segmentos.';
        93: Result := '93-Quantidade de registros no lote inválido.';
        94: Result := '94-Quantidade de registros no lote divergente.';
        95: Result := '95-Quantidade de lotes do arquivo inválido.';
        96: Result := '96-Quantidade de lotes no arquivo divergente.';
        97: Result := '97-Quantidade de registros no arquivo inválido.';
        98: Result := '98-Quantidade de registros no arquivo divergente.';
      end;
    toRetornoLiquidado,
    toRetornoBaixado,
    toRetornoLiquidadoAposBaixaOuNaoRegistro:
      case CodMotivo of
        02: Result := '02-Casas Lotéricas.';
        03: Result := '03-Liquidação no próprio Banco.';
        04: Result := '04-Compensação Eletrônica.';
        05: Result := '05-Compensação Convencional.';
        06: Result := '06-Outros Canais.';
        07: Result := '07-Correspondente Não Bancário.';
        08: Result := '08-Em Cartório.';
        09: Result := '09-Comandada Banco.';
        10: Result := '10-Comandada Cliente Arquivo.';
      end;
    toRetornoDebitoTarifas:
      case CodMotivo of
        01: Result := '01-Tarifa de Extrato de Posição';
        02: Result := '02-Tarifa de Manutenção de Título Vencido';
        03: Result := '03-Tarifa de Sustação';
        04: Result := '04-Tarifa de Protesto';
        05: Result := '05-Tarifa de Outras Instruções';
        06: Result := '06-Tarifa de Outras Ocorrências';
        07: Result := '07-Tarifa de Envio de Duplicata ao Sacado';
        08: Result := '08-Custas de Protesto';
        09: Result := '09-Custas de Sustação de Protesto';
        10: Result := '10-Custas de Cartório Distribuidor';
        11: Result := '11-Custas de Edital';
      end;
  end;

   Result := ACBrSTr(Result);
end;

function TACBrCaixaEconomicaSICOB.CalcularTamMaximoNossoNumero(
  const Carteira: String; const NossoNumero: String; const Convenio: String): Integer;
var
  wOperacao: Integer;
begin
   Result := 10;
   wOperacao := StrToIntDef(Copy(Convenio, 1, 3), 0);

   if (wOperacao = 870) and (Carteira = 'SR') then
     Result := 15;
end;

function TACBrCaixaEconomicaSICOB.CodOcorrenciaToTipo(
  const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  Result := toTipoOcorrenciaNenhum;
  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case CodOcorrencia of
      02: Result := toRetornoRegistroConfirmado;
      03: Result := toRetornoRegistroRecusado;
      04: Result := toRetornoTransferenciaCarteiraEntrada;
      05: Result := toRetornoTransferenciaCarteiraBaixa;
      06: Result := toRetornoLiquidado;
      09: Result := toRetornoBaixado;
      12: Result := toRetornoRecebimentoInstrucaoConcederAbatimento;
      13: Result := toRetornoRecebimentoInstrucaoCancelarAbatimento;
      14: Result := toRetornoRecebimentoInstrucaoAlterarVencimento;
      17: Result := toRetornoLiquidadoAposBaixaOuNaoRegistro;
      19: Result := toRetornoRecebimentoInstrucaoProtestar;
      20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
      23: Result := toRetornoEncaminhadoACartorio;
      24: Result := toRetornoRetiradoDeCartorio;
      25: Result := toRetornoBaixaPorProtesto;
      26: Result := toRetornoInstrucaoRejeitada;
      27: Result := toRetornoAlteracaoUsoCedente;
      28: Result := toRetornoDebitoTarifas;
      30: Result := toRetornoAlteracaoOutrosDadosRejeitada;
      36: Result := toRetornoConfirmacaoEmailSMS;
      37: Result := toRetornoEmailSMSRejeitado;
      43: Result := toRetornoEstornoProtesto;
      44: Result := toRetornoEstornoBaixaLiquidacao;
      45: Result := toRetornoRecebimentoInstrucaoAlterarDados;
      51: Result := toRetornoTituloDDAReconhecidoPagador;
      52: Result := toRetornoTituloDDANaoReconhecidoPagador;
      53: Result := toRetornoTituloDDARecusadoCIP;
    end;
  end
  else
  begin
    case CodOcorrencia of
      01: Result := toRetornoRegistroConfirmado;
      02: Result := toRetornoBaixado;
      03: Result := toRetornoAbatimentoConcedido;
      04: Result := toRetornoAbatimentoCancelado;
      05: Result := toRetornoVencimentoAlterado;
      06: Result := toRetornoAlteracaoUsoCedente;
      07: Result := toRetornoPrazoProtestoAlterado;
      08: Result := toRetornoPrazoDevolucaoAlterado;
      09: Result := toRetornoDadosAlterados;
      10: Result := toRetornoAlteracaoReemissaoBloquetoConfirmada;
      11: Result := toRetornoAlteracaoOpcaoProtestoParaDevolucaoConfirmada;
      12: Result := toRetornoAlteracaoOpcaoDevolucaoParaProtestoConfirmada;
      20: Result := toRetornoTituloEmSer;
      21: Result := toRetornoLiquidado;
      22: Result := toRetornoLiquidadoEmCartorio;
      23: Result := toRetornoBaixadoPorDevolucao;
      24: Result := toRetornoBaixadoFrancoPagamento;
      25: Result := toRetornoBaixaPorProtesto;
      26: Result := toRetornoEncaminhadoACartorio;
      27: Result := toRetornoProtestoSustado;
      28: Result := toRetornoEstornoProtesto;
      29: Result := toRetornoProtestoOuSustacaoEstornado;
      30: Result := toRetornoRecebimentoInstrucaoAlterarDados;
      31: Result := toRetornoTarifaDeManutencaoDeTitulosVencidos;
      32: Result := toRetornoOutrasTarifasAlteracao;
      33: Result := toRetornoEstornoBaixaLiquidacao;
      34: Result := toRetornoTransferenciaCarteiraEntrada;
      35: Result := toRetornoTransferenciaCarteiraBaixa;
      99: Result := toRetornoRegistroRecusado;
    end;
  end;
end;

function TACBrCaixaEconomicaSICOB.CodOcorrenciaToTipoRemessa(const CodOcorrencia: Integer): TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    02 : Result:= toRemessaBaixar;                          {Pedido de Baixa}
    04 : Result:= toRemessaConcederAbatimento;              {Concessão de Abatimento}
    05 : Result:= toRemessaCancelarAbatimento;              {Cancelamento de Abatimento concedido}
    06 : Result:= toRemessaAlterarVencimento;               {Alteração de vencimento}
    07 : Result:= toRemessaAlterarControleParticipante;     {Alteração do controle do participante}
    08 : Result:= toRemessaAlterarNumeroControle;           {Alteração de seu número}
    09 : Result:= toRemessaProtestar;                       {Pedido de protesto}
    10 : Result:= toRemessaCancelarInstrucaoProtestoBaixa;  {Sustar protesto e baixar}
    11 : Result:= toRemessaDispensarJuros;                  {Instrução para dispensar juros}
    12 : Result:= toRemessaAlterarNomeEnderecoSacado;       {Alteração de nome e endereço do Sacado}
    31 : Result:= toRemessaOutrasOcorrencias;               {Alteração de Outros Dados}
    32 : Result:= toRemessaCancelarDesconto;                {Não conceder desconto}
  else
     Result:= toRemessaRegistrar;                           {Remessa}
  end;
end;

function TACBrCaixaEconomicaSICOB.GetLocalPagamento: String;
begin
  Result := ACBrStr(CInstrucaoPagamentoLoterica);
end;

function TACBrCaixaEconomicaSICOB.FormataNossoNumero(const ACBrTitulo :TACBrTitulo): String;
var
  ANossoNumero: String;
  wOperacao: Integer;
begin
   with ACBrTitulo do
   begin
     ANossoNumero := OnlyNumber(NossoNumero);
     wOperacao    := StrToIntDef(Copy(ACBrBoleto.Cedente.Convenio, 1 , 3 ), 0);

     if (Carteira = 'SR') then
      begin
       if (wOperacao =  870) and (ACBrBoleto.Cedente.TipoCarteira = tctEletronica) then
         ANossoNumero:= '8'+ PadLeft(Copy(ANossoNumero,Length(ANossoNumero)-13,14),14)
       else
         ANossoNumero:= '82'+ PadLeft(Copy(ANossoNumero,Length(ANossoNumero)-7,8),8);
      end
     else if (Carteira = 'CS') then
      ANossoNumero := PadLeft(Copy(ANossoNumero,Length(ANossoNumero)-9,10),10,'0')
     else
       ANossoNumero:= '9' + PadLeft(Copy(ANossoNumero,Length(ANossoNumero)-8,9),9,'0');
   end;
   Result := ANossoNumero;
end;

function TACBrCaixaEconomicaSICOB.MontarCodigoBarras(const ACBrTitulo : TACBrTitulo): String;
var
  CodigoBarras, FatorVencimento, DigitoCodBarras :String;
  ANossoNumero, CampoLivre,aCodCedente :String;
begin
   FatorVencimento := CalcularFatorVencimento(ACBrTitulo.Vencimento);

   ANossoNumero := FormataNossoNumero(ACBrTitulo);
   aCodCedente:= PadLeft(RightStr(ACBrTitulo.ACBrBoleto.Cedente.CodigoCedente,11),11,'0');

   {Montando Campo Livre}
   if Length(ANossoNumero) > 11 then
   begin                                                                  //                     Tamanho
     CampoLivre := RightStr(aCodCedente, 5) +                             // Código do Cedende - 05
                   RightStr(ACBrTitulo.ACBrBoleto.Cedente.Agencia, 4) +   // Agência           - 04
                   '87' +                                                 // Constante         - 02
                   RightStr(ANossoNumero, 14);                            // Nosso Número      - 14
   end
   else
   begin
     CampoLivre := ANossoNumero +
                   RightStr(ACBrTitulo.ACBrBoleto.Cedente.Agencia, 4) +
                   aCodCedente;
   end;


   {Codigo de Barras}
   with ACBrTitulo.ACBrBoleto do
   begin
      CodigoBarras := IntToStrZero(Banco.Numero, 3) +
                      '9' +
                      FatorVencimento +
                      IntToStrZero(Round(ACBrTitulo.ValorDocumento * 100), 10) +
                      CampoLivre;
   end;

   DigitoCodBarras := CalcularDigitoCodigoBarras(CodigoBarras);
   Result:= copy( CodigoBarras, 1, 4) + DigitoCodBarras + copy( CodigoBarras, 5, 44);
end;

function TACBrCaixaEconomicaSICOB.RetornaCodCarteira(const Carteira: string): integer;
begin
if Carteira = 'CS' then
  Result := 11
else if Carteira = 'CR' then
  Result := 12
else if Carteira = 'SR' then
  Result := 14
else if Carteira = 'DE' then
  Result := 41
else
  Result := 11;

end;

function TACBrCaixaEconomicaSICOB.TipoOcorrenciaToCod(
  const TipoOcorrencia: TACBrTipoOcorrencia): String;
begin

  if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
  begin
    case TipoOcorrencia of
      toRetornoRegistroConfirmado                              : Result := '02';
      toRetornoRegistroRecusado                                : Result := '03';
      toRetornoTransferenciaCarteiraEntrada                    : Result := '04';
      toRetornoTransferenciaCarteiraBaixa                      : Result := '05';
      toRetornoLiquidado                                       : Result := '06';
      toRetornoBaixado                                         : Result := '09';
      toRetornoRecebimentoInstrucaoConcederAbatimento          : Result := '12';
      toRetornoRecebimentoInstrucaoCancelarAbatimento          : Result := '13';
      toRetornoRecebimentoInstrucaoAlterarVencimento           : Result := '14';
      toRetornoLiquidadoAposBaixaOuNaoRegistro                 : Result := '17';
      toRetornoRecebimentoInstrucaoProtestar                   : Result := '19';
      toRetornoRecebimentoInstrucaoSustarProtesto              : Result := '20';
      toRetornoEncaminhadoACartorio                            : Result := '23';
      toRetornoRetiradoDeCartorio                              : Result := '24';
      toRetornoBaixaPorProtesto                                : Result := '25';
      toRetornoInstrucaoRejeitada                              : Result := '26';
      toRetornoAlteracaoUsoCedente                             : Result := '27';
      toRetornoDebitoTarifas                                   : Result := '28';
      toRetornoAlteracaoOutrosDadosRejeitada                   : Result := '30';
      toRetornoConfirmacaoEmailSMS                             : Result := '36';
      toRetornoEmailSMSRejeitado                               : Result := '37';
      toRetornoEstornoProtesto                                 : Result := '43';
      toRetornoEstornoBaixaLiquidacao                          : Result := '44';
      toRetornoRecebimentoInstrucaoAlterarDados                : Result := '45';
      toRetornoTituloDDAReconhecidoPagador                     : Result := '51';
      toRetornoTituloDDANaoReconhecidoPagador                  : Result := '52';
      toRetornoTituloDDARecusadoCIP                            : Result := '53';
    end;
  end
  else
  begin
    case TipoOcorrencia of
      toRetornoRegistroConfirmado                              : Result := '01';
      toRetornoBaixado                                         : Result := '02';
      toRetornoAbatimentoConcedido                             : Result := '03';
      toRetornoAbatimentoCancelado                             : Result := '04';
      toRetornoVencimentoAlterado                              : Result := '05';
      toRetornoAlteracaoUsoCedente                             : Result := '06';
      toRetornoPrazoProtestoAlterado                           : Result := '07';
      toRetornoPrazoDevolucaoAlterado                          : Result := '08';
      toRetornoDadosAlterados                                  : Result := '09';
      toRetornoAlteracaoReemissaoBloquetoConfirmada            : Result := '10';
      toRetornoAlteracaoOpcaoProtestoParaDevolucaoConfirmada   : Result := '11';
      toRetornoAlteracaoOpcaoDevolucaoParaProtestoConfirmada   : Result := '12';
      toRetornoTituloEmSer                                     : Result := '20';
      toRetornoLiquidado                                       : Result := '21';
      toRetornoLiquidadoEmCartorio                             : Result := '22';
      toRetornoBaixadoPorDevolucao                             : Result := '23';
      toRetornoBaixadoFrancoPagamento                          : Result := '24';
      toRetornoBaixaPorProtesto                                : Result := '25';
      toRetornoEncaminhadoACartorio                            : Result := '26';
      toRetornoProtestoSustado                                 : Result := '27';
      toRetornoEstornoProtesto                                 : Result := '28';
      toRetornoProtestoOuSustacaoEstornado                     : Result := '29';
      toRetornoRecebimentoInstrucaoAlterarDados                : Result := '30';
      toRetornoTarifaDeManutencaoDeTitulosVencidos             : Result := '31';
      toRetornoOutrasTarifasAlteracao                          : Result := '32';
      toRetornoEstornoBaixaLiquidacao                          : Result := '33';
      toRetornoTransferenciaCarteiraEntrada                    : Result := '34';
      toRetornoTransferenciaCarteiraBaixa                      : Result := '35';
      toRetornoRegistroRecusado                                : Result := '99';
    end;
  end;
end;

function TACBrCaixaEconomicaSICOB.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
  CodOcorrencia: Integer;
begin
   CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia),0);

   if (ACBrBanco.ACBrBoleto.LayoutRemessa = c240) then
   begin
     case CodOcorrencia of
       02: Result := '02-Entrada Confirmada';
       03: Result := '03-Entrada Rejeitada';
       04: Result := '04-Transferência de Carteira/Entrada';
       05: Result := '05-Transferência de Carteira/Baixa';
       06: Result := '06-Liquidação';
       09: Result := '09-Baixa';
       12: Result := '12-Confirmação Recebimento Instrução de Abatimento';
       13: Result := '13-Confirmação Recebimento Instrução de Cancelamento Abatimento';
       14: Result := '14-Confirmação Recebimento Instrução Alteração de Vencimento';
       17: Result := '17-Liquidação Após Baixa ou Liquidação Título Não Registrado';
       19: Result := '19-Confirmação Recebimento Instrução de Protesto';
       20: Result := '20-Confirmação Recebimento Instrução de Sustação/Cancelamento de Protesto';
       23: Result := '23-Remessa a Cartório';
       24: Result := '24-Retirada de Cartório e Manutenção em Carteira';
       25: Result := '25-Protestado e Baixado';
       26: Result := '26-Instrução Rejeitada';
       27: Result := '27-Confirmação do Pedido de Alteração de Outros Dados';
       28: Result := '28-Débito de Tarifas/Custas';
       30: Result := '30-Alteração de Dados Rejeitada';
       36: Result := '36-Confirmação de envio de e-mail/SMS';
       37: Result := '37-Envio de e-mail/SMS rejeitado';
       43: Result := '43-Estorno de Protesto/Sustação';
       44: Result := '44-Estorno de Baixa/Liquidação';
       45: Result := '45-Alteração de Dados';
       51: Result := '51-Título DDA reconhecido pelo sacado';
       52: Result := '52-Título DDA não reconhecido pelo sacado';
       53: Result := '53-Título DDA recusado pela CIP';
     end;
   end
   else
   begin
     case CodOcorrencia of
       01: Result := '01-Entrada Confirmada';
       02: Result := '02-Baixa Confirmada';
       03: Result := '03-Abatimento Concedido';
       04: Result := '04-Abatimento Cancelado';
       05: Result := '05-Vencimento Alterado';
       06: Result := '06-Uso da Empresa Alterado';
       07: Result := '07-Prazo de Protesto Alterado';
       08: Result := '08-Prazo de Devolução Alterado';
       09: Result := '09-Alteração Confirmada';
       10: Result := '10-Alteração com Reemissão de Bloqueto Confirmada';
       11: Result := '11-Alteração da Opção de Protesto para Devolução';
       12: Result := '12-Alteração da Opção de Devolução para Protesto';
       20: Result := '20-Em Ser';
       21: Result := '21-Liquidação';
       22: Result := '22-Liquidação em Cartório';
       23: Result := '23-Baixa por Devolução';
       24: Result := '24-Baixa por Franco Pagamento';
       25: Result := '25-Baixa por Protesto';
       26: Result := '26-Título Enviado para Cartório';
       27: Result := '27-Sustação de Protesto';
       28: Result := '28-Estorno de Protesto';
       29: Result := '29-Estorno de Sustação de Protesto';
       30: Result := '30-Alteração de Título';
       31: Result := '31-Tarifa sobre Título Vencido';
       32: Result := '32-Outras Tarifas de Alteração';
       33: Result := '33-Estorno de Baixa / Liquidação';
       34: Result := '34-Transferência de Carteira/Entrada';
       35: Result := '35-Transferência de Carteira/Baixa';
       99: Result := '99-Rejeição do Título';
     end;
   end;

   Result := ACBrSTr(Result);
end;

function TACBrCaixaEconomicaSICOB.MontarCampoCodigoCedente (
   const ACBrTitulo: TACBrTitulo ) : String;
begin

  with ACBrTitulo.ACBrBoleto do
  begin
     Result := RightStr(Cedente.Agencia,4) + '.'+
               Copy(Cedente.CodigoCedente, Length(Cedente.CodigoCedente)-10,3) +
               '.'+ Copy(Cedente.CodigoCedente, Length(Cedente.CodigoCedente)-7,8) +
               '-' +CalcularDVCedente(ACBrTitulo,true);

  end;
end;

function TACBrCaixaEconomicaSICOB.MontarCampoNossoNumero (const ACBrTitulo: TACBrTitulo ) : String;
var ANossoNumero : string;
begin
  ANossoNumero := FormataNossoNumero(ACBrTitulo);
  Result := ANossoNumero + '-' + CalcularDigitoVerificador(ACBrTitulo);
end;

function TACBrCaixaEconomicaSICOB.GerarRegistroHeader240(NumeroRemessa : Integer): String;
var
  ATipoInscricao : String;
  ACodCedenteDV, aCodCedente, ACodCedenteDVAg: String;
  AMensagemReservada: String;
begin
   with ACBrBanco.ACBrBoleto.Cedente do
   begin
      case TipoInscricao of
        pFisica  : ATipoInscricao := '1';
        pJuridica: ATipoInscricao := '2';
      end;

      ACodCedenteDVAg := CalcularDVCedente(ACBrBanco.ACBrBoleto.ListadeBoletos[0],True);
      ACodCedenteDV   := CalcularDVCedente(ACBrBanco.ACBrBoleto.ListadeBoletos[0]);
      //ACodConvenio    := CodigoCedente + ACodCedenteDVAg;

      if ACBrBanco.ACBrBoleto.Homologacao then
        AMensagemReservada := 'REMESSA-TESTE'
      else
        AMensagemReservada := 'REMESSA-PRODUCAO';

      aCodCedente:= RightStr(CodigoCedente,8);


      { GERAR REGISTRO-HEADER DO ARQUIVO }
      Result:= IntToStrZero(ACBrBanco.Numero, 3)                         + //   1 a   3 - Código do banco
               '0000'                                                    + //   4 a   7 - Lote de serviço
               '0'                                                       + //   8 a   8 - Tipo de registro - Registro header de arquivo
               space(9)                                                  + //   9 a  17 - Uso exclusivo FEBRABAN/CNAB
               ATipoInscricao                                            + //  18 a  18 - Tipo de inscrição do cedente
               PadRight(OnlyNumber(CNPJCPF), 14, '0')                        + //  19 a  32 - Número de inscrição do cedente
               PadLeft(OnlyNumber(RightStr(Agencia, 4)), 4 , '0') +           //  33 a  48 - Código do convênio no banco - Cedente
               PadRight(CodigoCedente, 11, '0')+PadLeft(ACodCedenteDVAg,1,'0')  + //  33 a  48 - Código do convênio no banco - Cedente
               space(4)                                                  + //  49 a  52 - Uso Exclusivo CAIXA
               PadLeft(OnlyNumber(Agencia),5,'0')                           +
               PadLeft(AgenciaDigito,1,'0')                                 +
               PadLeft(aCodCedente,12, '0')                                 + //  59 a  70 - Código do Cedente
               PadLeft(ACodCedenteDV,1,'0')                                 + //  71 a  71 - DV Codigo Cedente
               CalcularDVAgCD(True)                                      + //  72 a  72 - Dig. Verif. Ag + Ced.
               PadRight(Nome, 30, ' ')                                       + //  73 a 102 - Nome da Empresa
               PadRight('CAIXA ECONOMICA FEDERAL', 40, ' ')                  + // 133 a 142 - Uso exclusivo FEBRABAN/CNAB
               '1'                                                       + // 143 a 143 - Código de Remessa (1) / Retorno (2)
               FormatDateTime('ddmmyyyy', Now)                           + // 144 a 151 - Data do de geração do arquivo
               FormatDateTime('hhmmss', Now)                             + // 152 a 157 - Hora de geração do arquivo
               PadLeft(IntToStr(NumeroRemessa), 6, '0')                     + // 158 a 163 - Número seqüencial do arquivo
               '030'                                                     + // 164 a 166 - Número da versão do layout do arquivo
               PadRight('',  5, '0')                                         + // 167 a 171 - Densidade de gravação do arquivo (BPI)
               space(20)                                                 + // 172 a 191 - Uso reservado do banco
               PadRight(AMensagemReservada, 20, ' ')  + // 192 a 211 - Uso reservado da empresa
               space(29);                                                  // 212 a 240 - Uso Exclusivo FEBRABAN / CNAB

      { GERAR REGISTRO HEADER DO LOTE }
      Result:= Result + #13#10 +
               IntToStrZero(ACBrBanco.Numero, 3)                         + //   1 a   3 - Código do banco
               '0001'                                                    + //   4 a   7 - Lote de serviço
               '1'                                                       + //   8 a   8 - Tipo de registro - Registro header de arquivo
               'R'                                                       + //   9 a   9 - Tipo de operação: R (Remessa) ou T (Retorno)
               '01'                                                      + //  10 a  11 - Tipo de serviço: 01 (Cobrança)
               '00'                                                      + //  12 a  13 - Forma de lançamento
               '020'                                                     + //  14 a  16 - Número da versão do layout do lote
               Space(1)                                                  + //  17 a  17 - Uso exclusivo FEBRABAN/CNAB
               ATipoInscricao                                            + //  18 a  18 - Tipo de inscrição da Empresa
               PadLeft(OnlyNumber(CNPJCPF), 15, '0')                        + //  19 a  33 - Número de inscrição da Empresa
               //PadLeft(CodigoCedente,15, '0') +PadLeft(ACodCedenteDVAg,1,'0')  + //  34 a  49 - Código do convênio no banco (código do cedente)
               PadLeft(OnlyNumber(RightStr(Agencia, 4)), 4 , '0') +
               PadRight(CodigoCedente, 11, '0')+PadLeft(ACodCedenteDVAg,1,'0')  + //  34 a  49 - Código do convênio no banco (código do cedente)
               space(4)                                                  + //  50 a  53 - Uso Exclusivo da CAIXA
               PadLeft(OnlyNumber(Agencia), 5 , '0')                        + //  54 a  58 - Agência Mantenedora da Conta
               PadLeft(AgenciaDigito, 1 , '0')                              + //  59 a  59 - Dígito Verificador da Agência
               PadLeft(aCodCedente,12, '0')                                 + //  60 a  71 - Cód. Cedente
               ACodCedenteDV                                             + //  72 a  72 - DV Codigo Cedente
               CalcularDVAgCD(True)                                      + //  73 a  73 - Dig. Verif. Ag + Ced.
               PadRight(Nome, 30, ' ')                                       + //  74 a 103 - Nome da Empresa
               space(40)                                                 + // 104 a 143 - Mensagem 1 para todos os boletos do lote
               space(40)                                                 + // 144 a 183 - Mensagem 2 para todos os boletos do lote
               PadLeft(IntToStr(NumeroRemessa), 8, '0')                     + // 184 a 191 - Número do arquivo
               FormatDateTime('ddmmyyyy', Now)                           + // 192 a 199 - Data de geração do arquivo
               '00000000'                                                + // 200 a 207 - Data do crédito - Só para arquivo retorno
               space(33);                                                  // 208 a 240 - Uso exclusivo FEBRABAN/CNAB
  end;
end;

procedure TACBrCaixaEconomicaSICOB.GerarRegistroHeader400(
  NumeroRemessa: Integer; aRemessa: TStringList);
var
  ACodCedente  :String;
  wLinha: String;
begin

   with ACBrBanco.ACBrBoleto.Cedente do
   begin

     ACodCedente := PadLeft(OnlyNumber(MontarCampoCodigoCedente(ACBrBanco.ACBrBoleto.ListadeBoletos[0])),16,'0');

      wLinha:= '0'                            + // 1 ate 1    - Código Identificador do tipo de Registro no Arquivo
               '1'                            + // 2 ate 2    - ID do Arquivo( 1 - Remessa)
               'REMESSA'                      + // 3 ate 9    - Literal de Remessa
               '01'                           + // 10 ate 11  - Código Identificador do Tipo de Serviço
               PadRight( 'COBRANCA', 15 )         + // 12 ate 26  - Literal correspondente ao códigode Serviço (COBRANÇA)
               ACodCedente+///  27 até 42  - Código Identificador da Empresa na CAIXA (CEDENTE)
               Space(4)                       + //43 Até 46   - brancos
               PadRight( Nome, 30)                + //47 até 76   - Nome da Empresa
               IntToStrZero( Numero, 3)       + //77 ate 79   - Código do Banco (104)
               PadRight('C ECON FEDERAL', 15)     + //80 até 94   - Nome do Banco(C ECON FEDERAL)
               FormatDateTime('ddmmyy',Now)   + //95 até 100  - Data de geração do arquivo
               space(289)                     + // 101 - 389 brancos
               IntToStrZero(NumeroRemessa,5)              + // 390 - 394 Número sequencial do Arquivo de Remessa
               IntToStrZero(1,6)              ; // 395 - 400 Número sequencial do Registrono Arquivo
      aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
   end;
end;

function TACBrCaixaEconomicaSICOB.GerarRegistroTransacao240(ACBrTitulo : TACBrTitulo): String;
var
   ATipoInscricao, ATipoOcorrencia, ATipoBoleto :String;
   ADataMoraJuros, ADataDesconto, ANossoNumero  :String;
   ATipoAceite, ACodCedenteDV, aCodCedente      :String;
   ADataMulta, aEspecieDoc: String;
   TipoInscricaoAvalista: Char;
begin
   TipoInscricaoAvalista := ' ';

   with ACBrTitulo do
   begin
      ANossoNumero := FormataNossoNumero(ACBrTitulo)+CalcularDigitoVerificador(ACBrTitulo);

      {SEGMENTO P}
      ACodCedenteDV := CalcularDVCedente(ACBrBanco.ACBrBoleto.ListadeBoletos[0]);

      {Pegando tipo de pessoa do Cendente}
      case Sacado.Pessoa of
         pFisica  : ATipoInscricao := '1';
         pJuridica: ATipoInscricao := '2';
         pOutras  : ATipoInscricao := '9';
      end;

      {Pegando tipo de pessoa do Avalista}
      case Sacado.Pessoa of
         pFisica  : TipoInscricaoAvalista := '1';
         pJuridica: TipoInscricaoAvalista := '2';
         pOutras  : TipoInscricaoAvalista := '9';
      end;

      {Pegando o Tipo de Ocorrencia}
      case OcorrenciaOriginal.Tipo of
         toRemessaBaixar                    : ATipoOcorrencia := '02';
         toRemessaConcederAbatimento        : ATipoOcorrencia := '04';
         toRemessaCancelarAbatimento        : ATipoOcorrencia := '05';
         toRemessaAlterarVencimento         : ATipoOcorrencia := '06';
         toRemessaConcederDesconto          : ATipoOcorrencia := '07';
         toRemessaCancelarDesconto          : ATipoOcorrencia := '08';
         toRemessaProtestar                 : ATipoOcorrencia := '09';
         toRemessaCancelarInstrucaoProtesto : ATipoOcorrencia := '10';
         toRemessaDispensarJuros            : ATipoOcorrencia := '31';
      else
         ATipoOcorrencia := '01';
      end;

      { Pegando o Aceite do Titulo }
      case Aceite of
         atSim :  ATipoAceite := 'A';
         atNao :  ATipoAceite := 'N';
      end;

      {Pegando a Espécie do Documento}
      if EspecieDoc = 'CH' then
        aEspecieDoc:= '01'
      else if EspecieDoc = 'DM' then
        aEspecieDoc:= '02'
      else if EspecieDoc = 'DMI' then
        aEspecieDoc:= '03'
      else if EspecieDoc = 'DS' then
        aEspecieDoc:= '04'
      else if EspecieDoc = 'DSI' then
        aEspecieDoc:= '05'
      else if EspecieDoc = 'DR' then
        aEspecieDoc:= '06'
      else if EspecieDoc = 'LC' then
        aEspecieDoc:= '07'
      else if EspecieDoc = 'NCC' then
        aEspecieDoc:= '08'
      else if EspecieDoc = 'NCE' then
        aEspecieDoc:= '09'
      else if EspecieDoc = 'NCI' then
        aEspecieDoc:= '10'
      else if EspecieDoc = 'NCR' then
        aEspecieDoc:= '11'
      else if EspecieDoc = 'NP' then
        aEspecieDoc:= '12'
      else if EspecieDoc = 'NPR' then
        aEspecieDoc:= '13'
      else if EspecieDoc = 'TM' then
        aEspecieDoc:= '14'
      else if EspecieDoc = 'TS' then
        aEspecieDoc:= '15'
      else if EspecieDoc = 'NS' then
        aEspecieDoc:= '16'
      else if EspecieDoc = 'RC' then
        aEspecieDoc:= '17'
      else if EspecieDoc = 'FAT' then
        aEspecieDoc:= '18'
      else if EspecieDoc = 'ND' then
        aEspecieDoc:= '19'
      else if EspecieDoc = 'AP' then
        aEspecieDoc:= '20'
      else if EspecieDoc = 'ME' then
        aEspecieDoc:= '21'
      else if EspecieDoc = 'PC' then
        aEspecieDoc:= '22'
      else
        aEspecieDoc:= '99';

      {Pegando Tipo de Boleto} //Quem emite e quem distribui o boleto?
      case ACBrBoleto.Cedente.ResponEmissao of
           tbBancoEmite      : ATipoBoleto := '1' + '1';
           tbCliEmite        : ATipoBoleto := '2' + '2';
           tbBancoReemite    : ATipoBoleto := '4' + '1';
           tbBancoNaoReemite : ATipoBoleto := '5' + '2';
      end;

      {Mora Juros}
      if (ValorMoraJuros > 0) then
        ADataMoraJuros := IfThen(DataMoraJuros > 0,
                                 FormatDateTime('ddmmyyyy', DataMoraJuros),
                                 FormatDateTime('ddmmyyyy', Vencimento + 1))
      else
        ADataMoraJuros := PadLeft('', 8, '0');

      {Multa}
      if (PercentualMulta > 0) then
        ADataMulta := IfThen(DataMoraJuros > 0,
                             FormatDateTime('ddmmyyyy', DataMoraJuros),
                             FormatDateTime('ddmmyyyy', Vencimento + 1))
      else
        ADataMulta := PadLeft('', 8, '0');



      {Descontos}
      if (ValorDesconto > 0) then
        ADataDesconto:= IfThen(DataDesconto > 0,
                               FormatDateTime('ddmmyyyy', DataDesconto),
                               PadLeft('', 8, '0'))
      else
        ADataDesconto:= PadLeft('', 8, '0');

      aCodCedente:= RightStr(ACBrBoleto.Cedente.CodigoCedente,8);

      Result:= IntToStrZero(ACBrBanco.Numero, 3)                                        + //   1 a   3 - Código do banco
               '0001'                                                                   + //   4 a   7 - Lote de Serviço
               '3'                                                                      + //   8 a   8 - Tipo do registro: Registro detalhe
               IntToStrZero((3 * ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo))+ 1 ,5)  + //   9 a  13 - Nº Sequencial do Registro no Lote
               'P'                                                                      + //  14 a  14 - Cód. Segmento do Registro Detalhe
               ' '                                                                      + //  15 a  15 - Uso Exclusivo FEBRABAN/CNAB
               ATipoOcorrencia                                                          + //  16 a  17 - Código de Movimento Remessa
               PadLeft(OnlyNumber(ACBrBoleto.Cedente.Agencia), 5, '0')                  + //  18 a  22 - Agência mantenedora da conta
               PadLeft(ACBrBoleto.Cedente.AgenciaDigito, 1 , '0')                       + //  23 a  23 - Dígito verificador da agência
               PadLeft(aCodCedente, 12, '0')                                            + //  24 a  35 - Código do Cedente
               ACodCedenteDV                                                            + //  36 a  36 - Digito Verificador do Cedente
               CalcularDVAgCD                                                           + //  37 a  37 - Digito Verificador da Ag. + Cedente
               space(9)                                                                 + //  38 a  46 - Uso Exclusivo da CAIXA
               PadLeft(ANossoNumero, 11, '0')                                           + //  47 a  57 - Nosso número - identificação do título no banco
               '1'                                                                      + //  58 a  58 - Código da Carteira: 10Cobrança Simples; 3-Cobrança Caucionada; 4-Cobrança Descontada
               '1'                                                                      + //  59 a  59 - Forma de cadastramento do título no banco:  1-cobrança Registrada | 2-Cobrança sem registro
               '2'                                                                      + //  60 a  60 - Tipo de documento: 1-Tradicional; 2-Escritural (Padrão 2)
               ATipoBoleto                                                              + //  61 a  62 - Identificação da Emissão do Bloqueto
               PadRight(NumeroDocumento, 11, ' ')                                       + //  63 a  73 - Número do Documento de Cobrança
               space(4)                                                                 + //  74 a  77 - Uso Exclusivo CAIXA
               FormatDateTime('ddmmyyyy', Vencimento)                                   + //  78 a  85 - Data de vencimento do título
               IntToStrZero(Round(ValorDocumento * 100), 15)                            + //  86 a 100 - Valor nominal do título
               '00000'                                                                  + // 101 a 105 - Agência cobradora. Se ficar em branco, a caixa determina automaticamente pelo CEP do sacado
               '0'                                                                      + // 106 a 106 - DV Agência cobradora
               PadRight(aEspecieDoc,2)                                                  + // 107 a 108 - Espécie do documento
               ATipoAceite                                                              + // 109 a 109 - Identificação de título Aceito / Não aceito
               FormatDateTime('ddmmyyyy', DataDocumento)                                + // 110 a 117 - Data da Emissão do Título
               IfThen(ValorMoraJuros > 0, '1', '0')                                     + // 118 a 118 - Código de juros de mora: Valor por dia
               ADataMoraJuros                                                           + // 119 a 126 - Data a partir da qual serão cobrados juros
               IntToStrZero( round(ValorMoraJuros * 100), 15)                           + // 127 a 141 - Valor de juros de mora por dia
               IfThen(ValorDesconto > 0, '1', '0')                                      + // 142 a 142 - Código de desconto: Valor fixo até a data informada
               ADataDesconto                                                            + // 143 a 150 - Data do desconto
               IntToStrZero(round(ValorDesconto * 100), 15)                             + // 151 a 165 - Valor do desconto por dia
               IntToStrZero( round(ValorIOF * 100), 15)                                 + // 166 a 180 - Valor do IOF a ser recolhido
               IntToStrZero( round(ValorAbatimento * 100), 15)                          + // 181 a 195 - Valor do abatimento
               PadRight(IfThen(Trim(SeuNumero) = '',NumeroDocumento,SeuNumero), 25, ' ')+ // 196 a 220 - Identificação do título na empresa
               IfThen((DataProtesto > 0) and (DataProtesto > Vencimento), '1', '3')     + // 221 a 221 - Código de protesto: Protestar em XX dias corridos
               IfThen((DataProtesto > 0) and (DataProtesto > Vencimento),
                      PadLeft(IntToStr(DaysBetween(DataProtesto,
                       Vencimento)), 2, '0'), '00')                                     + // 222 a 223 - Prazo para protesto (em dias corridos)
               IfThen((DataBaixa <> 0) and (DataBaixa > Vencimento), '1', '2') + //224 - Código para baixa/devolução: Não baixar/não devolver
               IfThen((DataBaixa <> 0) and (DataBaixa > Vencimento),
                      PadLeft(IntToStr(DaysBetween(DataBaixa, Vencimento)), 3, '0'), '000') + //225 a 227 - Prazo para baixa/devolução (em dias corridos)
               '09'                                                                     + // 228 a 229 - Código da moeda: Real
               Space(10)                                                                + // 230 a 239 - Uso Exclusivo FEBRABAN/CNAB
               Space(1);                                                                  // 240 a 240 - Uso exclusivo FEBRABAN/CNAB

      {SEGMENTO Q}
      Result:= Result + #13#10 +
               IntToStrZero(ACBrBanco.Numero, 3)                                       + //   1 a   3 - Código do banco
               '0001'                                                                  + // 4 a 7 - Lote de Serviço
               '3'                                                                     + //   8 a   8 - Tipo do registro: Registro detalhe
               IntToStrZero((3 * ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo))+ 2 ,5) + //   9 a  13 - Número do lote
               'Q'                                                                     + //  14 a  14 - Código do segmento do registro detalhe
               ' '                                                                     + //  15 a  15 - Uso exclusivo FEBRABAN/CNAB: Branco
               ATipoOcorrencia                                                         + //  16 a  17 - Código de movimento
               {Dados do sacado}
               ATipoInscricao                                                          + //  18 a  18 - Tipo inscricao
               PadLeft(OnlyNumber(Sacado.CNPJCPF), 15, '0')                            + //  19 a  33 - Número de Inscrição
               PadRight(Sacado.NomeSacado, 40, ' ')                                    + //  34 a  73 - Nome sacado
               PadRight(Sacado.Logradouro + ' ' +
                        Sacado.Numero + ' ' +
                        Sacado.Complemento , 40, ' ')                                  + //  74 a 113 - Endereço
               PadRight(Sacado.Bairro, 15, ' ')                                        + // 114 a 128 - bairro sacado
               PadLeft(OnlyNumber(Sacado.CEP), 8, '0')                                 + // 129 a 133 e 134 a 136- cep sacado prefixo e sufixo sem o traço"-" somente numeros
               PadRight(Sacado.Cidade, 15, ' ')                                        + // 137 a 151 - cidade sacado
               PadRight(Sacado.UF, 2, ' ')                                             + // 152 a 153 - UF sacado
               {Dados do sacador/avalista}
               TipoInscricaoAvalista                                                   + // 154 a 154  - Tipo de inscrição: Não informado {campo obrigatorio segunto manual da caixa}
               PadLeft(OnlyNumber(Sacado.SacadoAvalista.CNPJCPF), 15, '0')             + // 155 a 169 - Número de inscrição
               PadRight(Sacado.SacadoAvalista.NomeAvalista,40,' ')                     + // 170 a 209 - Nome do sacador/avalista
               space(3)                                                                + // 210 a 212 - Uso exclusivo FEBRABAN/CNAB
               space(20)                                                               + // 213 a 232 - Uso exclusivo FEBRABAN/CNAB
               space(8);                                                                 // 233 a 240 - Uso exclusivo FEBRABAN/CNAB

      {SEGMENTO R}
      Result:= Result + #13#10 +
               IntToStrZero(ACBrBanco.Numero, 3)                                       + //   1 a 3   - Código do banco
               '0001'                                                                  + //   4 a 7   - Número do lote
               '3'                                                                     + //   8 a 8   - Tipo do registro: Registro detalhe
               IntToStrZero((3 * ACBrBoleto.ListadeBoletos.IndexOf(ACBrTitulo))+ 3 ,5) + //   9 a 13  - Número seqüencial do registro no lote - Cada título tem 2 registros (P e Q)
               'R'                                                                     + //  14 a 14  - Código do segmento do registro detalhe
               ' '                                                                     + //  15 a 15  - Uso exclusivo FEBRABAN/CNAB: Branco
               ATipoOcorrencia                                                         + //  16 a 17  - Tipo Ocorrencia
               PadLeft('', 48, ' ')                                                    + //  18 a 65  - Brancos (Não definido pelo FEBRAN)
               IfThen(PercentualMulta > 0, '2', '0')                                   + //  66 a 66  - 1-Cobrar Multa / 0-Não cobrar multa
               ADataMulta                                                              + //  67 a 74  - Se cobrar informe a data para iniciar a cobrança ou informe zeros se não cobrar
               IntToStrZero(round(PercentualMulta * 100), 15)                          + //  75 a 89  - Percentual de multa. Informar zeros se não cobrar
               PadRight('', 10, ' ')                                                   + //  90 a 99  - Informação ao Sacado
               PadRight('', 40, ' ')                                                   + // 100 a 139 - Mensagem 3
               PadRight('', 40, ' ')                                                   + // 140 a 179 - Mensagem 4
               PadRight('', 61, ' ');                                                    // 180 a 240 - Uso Exclusivo Febraban/CNAB
    end;
end;



procedure TACBrCaixaEconomicaSICOB.GerarRegistroTransacao400(
  ACBrTitulo: TACBrTitulo; aRemessa: TStringList);
var
  ANossoNumero, ADigitoNossoNumero, ATipoOcorrencia, ATipoSacado, ATipoCendente,
  ATipoAceite, ATipoEspecieDoc, AMensagem, aDataDesconto, wLinha,
  ADataMoraJuros, ACodCedente: String;
  TamConvenioMaior6: Boolean;
  wCarteira: Integer;
begin
   with ACBrTitulo do
   begin
      wCarteira:= RetornaCodCarteira(Carteira);
      if ((wCarteira = 11) or (wCarteira= 31) or (wCarteira = 51)) or
         (((wCarteira = 12) or (wCarteira = 15) or (wCarteira = 17)) and
          (ACBrBoleto.Cedente.ResponEmissao <>  tbCliEmite)) then
       begin
         ANossoNumero       := '00000000000000000000';
         ADigitoNossoNumero := ' ';
       end
      else
       begin
         ANossoNumero := FormataNossoNumero(ACBrTitulo);
         ADigitoNossoNumero :=  CalcularDigitoVerificador(ACBrTitulo);
       end;



      {Mora Juros}
      if (ValorMoraJuros > 0) then
       begin
         if (DataMoraJuros > 0) and (DataMoraJuros >= 30/12/2000) then
            ADataMoraJuros := FormatDateTime('ddmmyy', DataMoraJuros)
         else
            ADataMoraJuros := PadLeft('', 6, '0');
       end
      else
         ADataMoraJuros := PadLeft('', 6, '0');


      TamConvenioMaior6 := Length(trim(ACBrBoleto.Cedente.Convenio)) > 6;

      {Pegando Código da Ocorrencia}
      case OcorrenciaOriginal.Tipo of
         toRemessaBaixar                         : ATipoOcorrencia := '02'; {Pedido de Baixa}
         toRemessaConcederAbatimento             : ATipoOcorrencia := '04'; {Concessão de Abatimento}
         toRemessaCancelarAbatimento             : ATipoOcorrencia := '05'; {Cancelamento de Abatimento concedido}
         toRemessaAlterarVencimento              : ATipoOcorrencia := '06'; {Alteração de vencimento}
         toRemessaAlterarControleParticipante    : ATipoOcorrencia := '07'; {Alteração do número de controle do participante}
         toRemessaAlterarNumeroControle          : ATipoOcorrencia := '08'; {Alteração de seu número}
         toRemessaProtestar                      : ATipoOcorrencia := '09'; {Pedido de protesto}
         toRemessaCancelarInstrucaoProtestoBaixa : ATipoOcorrencia := '10'; {Sustar protesto e baixar}
         toRemessaCancelarInstrucaoProtesto      : ATipoOcorrencia := '10'; {Sustar protesto e manter na carteira}
         toRemessaDispensarJuros                 : ATipoOcorrencia := '11'; {Instrução para dispensar juros}
         toRemessaAlterarNomeEnderecoSacado      : ATipoOcorrencia := '12'; {Alteração de nome e endereço do Sacado}
         toRemessaOutrasOcorrencias              : ATipoOcorrencia := '31'; {Alteração de Outros Dados}
         toRemessaCancelarDesconto               : ATipoOcorrencia := '32'; {Não conceder desconto}
      else
         ATipoOcorrencia := '01';                                      {Remessa}
      end;

      { Pegando o Aceite do Titulo }
      case Aceite of
         atSim :  ATipoAceite := 'A';
         atNao :  ATipoAceite := 'N';
      end;

      { Pegando o tipo de EspecieDoc }
      if EspecieDoc = 'DM' then
         ATipoEspecieDoc   := '01'
      else if EspecieDoc = 'NP' then
         ATipoEspecieDoc   := '02'
      else if EspecieDoc = 'NS' then
         ATipoEspecieDoc   := '03'
      else if EspecieDoc = 'RC' then
         ATipoEspecieDoc   := '05'
      else if EspecieDoc = 'LC' then
         ATipoEspecieDoc   := '08'
      else if EspecieDoc = 'DS' then
         ATipoEspecieDoc   := '12'
      else if EspecieDoc = 'ND' then
         ATipoEspecieDoc   := '13';

      if not ( (DataProtesto > 0) and (DataProtesto > Vencimento) ) then
        Instrucao1 := '07'; //Não Protestar

      aDataDesconto:= '000000';

      if ValorDesconto > 0 then
      begin
         if DataDesconto > EncodeDate(2000,01,01) then
            aDataDesconto := FormatDateTime('ddmmyy',DataDesconto)
         else
            aDataDesconto := '777777';
      end;


      {Pegando Tipo de Sacado}
      case Sacado.Pessoa of
         pFisica   : ATipoSacado := '01';
         pJuridica : ATipoSacado := '02';
      else
         ATipoSacado := '02';
      end;

      {Pegando Tipo de Cedente}
      case ACBrBoleto.Cedente.TipoInscricao of
         pFisica   : ATipoCendente := '01';
         pJuridica : ATipoCendente := '02';
      end;

      AMensagem   := '';
      if Mensagem.Text <> '' then
         AMensagem   := Mensagem.Strings[0];

     ACodCedente := PadLeft(OnlyNumber(MontarCampoCodigoCedente(ACBrTitulo)),16,'0');


      with ACBrBoleto do
      begin
         if TamConvenioMaior6 then
            wLinha:= '7'
         else
            wLinha:= '1';

         wLinha:= wLinha                                                + // 1 até 1  -  ID Registro
                  ATipoCendente                                         + // 2 até 3   -  Tipo de inscrição da empresa 01-CPF / 02-CNPJ
                  PadLeft(OnlyNumber(Cedente.CNPJCPF),14,'0')           + // 4  até 17 - Inscrição da empresa
                  ACodCedente                                           + // 18 até 33 - Identificação da Empresa na CAIXA - Cedente
                  Space(2)                                              + // 34 até 35 - Branco
                  '00'                                                  + //36 até 37  - Default '00' - Acata Comissão por Dia (recomendável)
                  PadRight( SeuNumero, 25 )                             + //38 até 62  - Controle da empresa
                  PadLeft( ANossoNumero,10)+ ADigitoNossoNumero         + //63 até 73  - Nosso Numero
                  Space(3)                                              + // 74 Até 76  - Brancos
                  PadRight( AMensagem, 30)                              + //77 até  106 - mensagem impressa
                  PadLeft(IntToStr(RetornaCodCarteira(Carteira)),2,'0') + //107 até 108 - Código Carteira
                  ATipoOcorrencia                                       + //109 até 110 - Código da ocorrencia
                  PadRight( NumeroDocumento, 10, ' ')                   + //111 ate 120 - Seu Numero - Nr. titulo dado pelo cedente
                  FormatDateTime( 'ddmmyy', Vencimento )                + //121 ate 126 -  Data de vencimento
                  IntToStrZero( Round( ValorDocumento * 100 ), 13)      + // 127 ate 139 - Valor do titulo
                  '104'                                                 + // 140 até 142 - Código de compensação da CAIXA
                  '00000'                                               + // 143 até 147 - Agencia Cobradora
                  PadLeft(ATipoEspecieDoc, 2, '0')                      + // 148 até 149 - Espécie
                  ATipoAceite                                           + // 150 até  150  A- aceito / N- não aceito
                  FormatDateTime( 'ddmmyy', DataDocumento )             + //151 até 156 - Data de Emissão
                  PadLeft(Instrucao1, 2, '0')                           + //157 até 158 - Primeira instrução de Cobrança
                  PadLeft(Instrucao2, 2, '0')                           + //159 até 160 - Primeira instrução de Cobrança
                  IntToStrZero( round(ValorMoraJuros * 100 ), 13)       + //161 até 173 Juros de mora por dia
                  aDataDesconto                                         + //174 até 179 Data limite para concessao de desconto
                  IntToStrZero( round( ValorDesconto * 100), 13)        + //180 até 192  Valor do desconto
                  IntToStrZero( round( ValorIOF * 100 ), 13)            + //193 até 205 Valor do IOF
                  IntToStrZero( round( ValorAbatimento * 100 ), 13)     + //206 até 218 Valor do abatimento permitido
                  ATipoSacado                                           + //219 até 220 "01" - CPF / "02"- CGC
                  PadLeft(OnlyNumber(Sacado.CNPJCPF),14,'0')            + // 221 até 234 CNPJ ou CPF do sacado
                  PadRight( Sacado.NomeSacado, 40)                      + // 235 até 274 Nome do sacado
                  PadRight(trim(Sacado.Logradouro) + ', ' +
                           trim(Sacado.Numero) + ' ' +
                           trim(Sacado.Complemento), 40) +                //275 até 314 - Endereço do sacado
                  PadRight( Sacado.Bairro, 12)                          + //315 até  326 - Bairro Sacado
                  PadLeft( OnlyNumber(Sacado.CEP), 8 )                  + // 327 até 334 - CEP do endereço do sacado
                  PadRight( trim(Sacado.Cidade), 15)                    + // 335 até 349  - Cidade do sacado
                  PadRight( Sacado.UF, 2 )                              +// 350 até 351  - UF da cidade do sacado
                  ADataMoraJuros                                        + //352 até 357  - Data Multa
                  IntToStrZero(round((ValorDocumento*PercentualMulta)/100), 10) + //358 até 367 - Valor nominal da multa
                  PadRight( Sacado.NomeSacado, 22)                      + // 368 até 389 - Nome do Sacador Avalista
                  '00'                                                  + // 390  391 - Terceira instrução de Cobrança Default '00'
                   IfThen((DataProtesto > 0) and
                      (DataProtesto > Vencimento),
                       PadLeft(IntToStr(DaysBetween(DataProtesto,
                       Vencimento)), 2, '0'), '00')                     + //392 até 393 - Quantidade de dias para início da ação de protesto ou devolução do Título
                   '1'                                                  + // 394 até 394 - Código da moeda: Real
                   IntToStrZero( aRemessa.Count + 1, 6 );                 // 395 até 400
         aRemessa.Text := aRemessa.Text + UpperCase(wLinha);
      end;
   end;
end;

function TACBrCaixaEconomicaSICOB.GerarRegistroTrailler240( ARemessa : TStringList ): String;
begin
   {REGISTRO TRAILER DO LOTE}
   Result:= IntToStrZero(ACBrBanco.Numero, 3)                          + //    1 a   3 - Código do banco
            '0001'                                                     + //    7 a   4 - Lote de Serviço
            '5'                                                        + //    8 a   8 - Tipo do registro: Registro trailer do lote
            Space(9)                                                   + //    9 a  17 - Uso exclusivo FEBRABAN/CNAB
            IntToStrZero((3 * ARemessa.Count-1 {3 * ARemessa.Count}), 6)                      + //   18 a  23 - Quantidade de Registro no Lote
            // Totalização Cobrança Simples
            PadLeft('', 6, '0')                                           + //   24 a  29 - Quantidade títulos em cobrança (Somente retorno)
            PadLeft('',17, '0')                                           + //   30 a  46 - Valor dos títulos em carteiras (Somente retorno)
            // CNAB
            Space(6)                                                   + //   47 a  52 - Uso Exclusivo FEBRABAN/CNAB
            space(17)                                                  + //   53 a  69 - Uso Exclusivo FEBRABAN/CNAB
            // Totalização Cobrança Caucionada
            PadLeft('', 6, '0')                                           + //   70 a  75 - Quantidade títulos em cobrança (Somente retorno)
            PadLeft('',17, '0')                                           + //   76 a  92 - Valor dos títulos em carteiras (Somente retorno)
            // Totalização Cobrança Descontada
            space(6)                                                   + //   93 a  98 - Quantidade títulos em cobrança (Somente retorno)
            space(17)                                                  + //   99 a 115 - Valor dos títulos em carteiras (Somente retorno)
            space(8)                                                   + //  116 a 123 - Uso exclusivo FEBRABAN/CNAB
            space(117);                                                  //  124 a 240 - Uso exclusivo FEBRABAN/CNAB

   {GERAR REGISTRO TRAILER DO ARQUIVO}
   Result:= Result + #13#10 +
            IntToStrZero(ACBrBanco.Numero, 3)                          + //    1 a   3 - Código do banco
            '9999'                                                     + //    4 a   7 - Lote de serviço
            '9'                                                        + //    8 a   8 - Tipo do registro: Registro trailer do arquivo
            space(9)                                                   + //    9 a  17 - Uso exclusivo FEBRABAN/CNAB}
            '000001'                                                   + //   18 a  23 - Quantidade de lotes do arquivo}
            IntToStrZero(((ARemessa.Count-1)* 3)+4, 6){2 * ARemessa.Count + 2}      + //   24 a  29 - Quantidade de registros do arquivo, inclusive este registro que está sendo criado agora}
            space(6)                                                   + //   30 a  35 - Uso exclusivo FEBRABAN/CNAB}
            space(205);                                                  //   36 a 240 - Uso exclusivo FEBRABAN/CNAB}
end;

procedure TACBrCaixaEconomicaSICOB.GerarRegistroTrailler400(
  ARemessa: TStringList);
var
  wLinha: String;
begin
   wLinha := '9' + Space(393)                     + // ID Registro
             IntToStrZero(ARemessa.Count + 1, 6);  // Contador de Registros
   ARemessa.Text:= ARemessa.Text + UpperCase(wLinha);
end;

procedure TACBrCaixaEconomicaSICOB.LerRetorno240(ARetorno: TStringList);
var
  ContLinha: Integer;
  Titulo   : TACBrTitulo;
  Linha, rCedente, rCNPJCPF: String;
  rAgencia, rConta,rDigitoConta: String;
begin

   if (copy(ARetorno.Strings[0],143,1) <> '2') then
      raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                             'não é um arquivo de retorno do '+ Nome));

   rAgencia     := trim(Copy(ARetorno[0],53,5));
   rConta       := trim(Copy(ARetorno[0],63,8));
   rDigitoConta := Copy(ARetorno[0],72,1);
   rCedente     := trim(Copy(ARetorno[0],73,30));

   ACBrBanco.ACBrBoleto.DataArquivo   := StringToDateTimeDef(Copy(ARetorno[0],144,2)+'/'+
                                                             Copy(ARetorno[0],146,2)+'/'+
                                                             Copy(ARetorno[0],148,4),0, 'DD/MM/YYYY' );
   ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0],158,6),0);                                                             

   if StrToIntDef(Copy(ARetorno[1],200,8),0) <> 0 then
      ACBrBanco.ACBrBoleto.DataCreditoLanc := StringToDateTimeDef(Copy(ARetorno[1],200,2)+'/'+
                                                                  Copy(ARetorno[1],202,2)+'/'+
                                                                  Copy(ARetorno[1],204,4),0, 'DD/MM/YYYY' );

   case StrToIntDef(Copy(ARetorno[1],18,1),0) of
     1: ACBrBanco.ACBrBoleto.Cedente.TipoInscricao:= pFisica;
     else
        ACBrBanco.ACBrBoleto.Cedente.TipoInscricao:= pJuridica;
   end;

   if ACBrBanco.ACBrBoleto.Cedente.TipoInscricao = pJuridica then
    begin
      rCNPJCPF := trim( Copy(ARetorno[1],19,15)) ;
      rCNPJCPF := RightStr(rCNPJCPF,14) ;
    end
   else
    begin
      rCNPJCPF := trim( Copy(ARetorno[1],23,11));
      rCNPJCPF := RightStr(rCNPJCPF,11) ;
    end;

   ValidarDadosRetorno(rAgencia, rConta, rCNPJCPF);
   with ACBrBanco.ACBrBoleto do
   begin
     if LeCedenteRetorno then
     begin
        Cedente.Nome    := rCedente;
        Cedente.CNPJCPF := rCNPJCPF;
        Cedente.Agencia := rAgencia;
        Cedente.AgenciaDigito:= '0';
        Cedente.Conta   := rConta;
        Cedente.ContaDigito:= rDigitoConta;

        case StrToIntDef(Copy(ARetorno[1],18,1),0) of
          1: Cedente.TipoInscricao:= pFisica;
        else
             Cedente.TipoInscricao:= pJuridica;
        end;
     end;
     ListadeBoletos.Clear;
   end;

   Linha := '';
   Titulo := nil;

   for ContLinha := 1 to ARetorno.Count - 2 do
   begin
      Linha := ARetorno[ContLinha] ;

      if Copy(Linha,14,1)= 'T' then //segmento T - Só cria após passar pelo seguimento T depois U
         Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

      if Assigned(Titulo) then
      with Titulo do
      begin
         if Copy(Linha,14,1)= 'T' then //segmento T
          begin
            ACBrBanco.TamanhoMaximoNossoNum :=
             CalcularTamMaximoNossoNumero(Carteira, '', ACBrBanco.ACBrBoleto.Cedente.Convenio);

            SeuNumero        := copy(Linha,59,11);
            NumeroDocumento  := copy(Linha,106,25);

            NossoNumero := Copy(Copy(Linha,47,10), // sem o DV
                                Length(Copy(Linha,47,10))-TamanhoMaximoNossoNum ,
                                TamanhoMaximoNossoNum);
            OcorrenciaOriginal.Tipo := CodOcorrenciaToTipo(StrToIntDef(copy(Linha,16,2),0));

            if (Trim(Copy(Linha,214,2)) <> '00') then
            begin
               MotivoRejeicaoComando.Add(copy(Linha,214,2));
               DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo,StrToIntDef(Copy(Linha,214,2),0)));
            end;
            
            Vencimento := StringToDateTimeDef( Copy(Linha,74,2)+'/'+
                                               Copy(Linha,76,2)+'/'+
                                               Copy(Linha,78,4),0, 'DD/MM/YYYY' );
            ValorDocumento       := StrToFloatDef(Copy(Linha,82,15),0)/100;

          
            ValorDespesaCobranca := StrToFloatDef(Copy(Linha,199,15),0)/100;
            // Carteira             := Copy(Linha,40,2);
            // No SICOB não retorna o numero da carteira. Retorna o seguinte:
            // 1 = Cobrança Simples
            // 3 = Cobrança Caucionada
            // 4 = Cobrança Descontada
         end
        {segmento U}
        else if Copy(Linha,14,1)= 'U' then
         begin
           if StrToIntDef(Copy(Linha,138,8),0) <> 0 then
              DataOcorrencia := StringToDateTimeDef( Copy(Linha,138,2)+'/'+
                                                     Copy(Linha,140,2)+'/'+
                                                     Copy(Linha,142,4),0, 'DD/MM/YYYY' );

           if StrToIntDef(Copy(Linha,146,8),0) <> 0 then
              DataCredito:= StringToDateTimeDef( Copy(Linha,146,2)+'/'+
                                                 Copy(Linha,148,2)+'/'+
                                                 Copy(Linha,150,4),0, 'DD/MM/YYYY' );

           ValorMoraJuros       := StrToFloatDef(Copy(Linha,18,15),0)/100;
           ValorDesconto        := StrToFloatDef(Copy(Linha,33,15),0)/100;
           ValorAbatimento      := StrToFloatDef(Copy(Linha,48,15),0)/100;
           ValorIOF             := StrToFloatDef(Copy(Linha,63,15),0)/100;
           ValorOutrasDespesas  := StrToFloatDef(Copy(Linha,108,15),0)/100;
           ValorOutrosCreditos  := StrToFloatDef(Copy(Linha,123,15),0)/100;

          
           ValorRecebido        := StrToFloatDef(Copy(Linha,78,15),0)/100;
        end;
      end;
   end;
end;



procedure TACBrCaixaEconomicaSICOB.LerRetorno400(ARetorno: TStringList);
var
  Titulo : TACBrTitulo;
  ContLinha, CodOcorrencia, CodMotivo, MotivoLinha : Integer;
  rAgencia, rDigitoAgencia, rConta :String;
  rDigitoConta, rCodigoCedente     :String;
  Linha, rCedente                  :String;
begin
   if StrToIntDef(copy(ARetorno.Strings[0],77,3),-1) <> Numero then
      raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                             'não é um arquivo de retorno do '+ Nome));

   rCedente      := trim(Copy(ARetorno[0],47,30));
   rAgencia      := trim(Copy(ARetorno[0],27,4));
   rDigitoAgencia:= Copy(ARetorno[0],31,1);
   rConta        := trim(Copy(ARetorno[0],32,8));
   rDigitoConta  := Copy(ARetorno[0],40,1);

   rCodigoCedente:= Copy(ARetorno[0],150,7);


   ACBrBanco.ACBrBoleto.NumeroArquivo := StrToIntDef(Copy(ARetorno[0],101,7),0);

   ACBrBanco.ACBrBoleto.DataArquivo   := StringToDateTimeDef(Copy(ARetorno[0],95,2)+'/'+
                                                             Copy(ARetorno[0],97,2)+'/'+
                                                             Copy(ARetorno[0],99,2),0, 'DD/MM/YY' );

   ValidarDadosRetorno(rAgencia, rConta);
   with ACBrBanco.ACBrBoleto do
   begin
      Cedente.Nome         := rCedente;
      Cedente.Agencia      := rAgencia;
      Cedente.AgenciaDigito:= rDigitoAgencia;
      Cedente.Conta        := rConta;
      Cedente.ContaDigito  := rDigitoConta;
      Cedente.CodigoCedente:= rCodigoCedente;

      ACBrBanco.ACBrBoleto.ListadeBoletos.Clear;
   end;

   for ContLinha := 1 to ARetorno.Count - 2 do
   begin
      Linha := ARetorno[ContLinha] ;

      if (Copy(Linha,1,1) <> '7') and (Copy(Linha,1,1) <> '1') then
         Continue;

      Titulo := ACBrBanco.ACBrBoleto.CriarTituloNaLista;

      with Titulo do
      begin
         Carteira := Copy(Linha,107,2);

         ACBrBanco.TamanhoMaximoNossoNum :=
           CalcularTamMaximoNossoNumero(Carteira, '', ACBrBanco.ACBrBoleto.Cedente.Convenio);

         SeuNumero                   := copy(Linha,39,25);
         NumeroDocumento             := copy(Linha,117,10);
         OcorrenciaOriginal.Tipo     := CodOcorrenciaToTipo(StrToIntDef(
                                        copy(Linha,109,2),0));

         CodOcorrencia := StrToIntDef(IfThen(copy(Linha,109,2) = '00','00',copy(Linha,109,2)),0);

         if(CodOcorrencia >= 2) and ((CodOcorrencia <= 10)) then
         begin
           MotivoLinha:= 87;
           CodMotivo:= StrToInt(IfThen(copy(Linha,MotivoLinha,2) = '00','00',copy(Linha,87,2)));
           MotivoRejeicaoComando.Add(copy(Linha,87,2));
           DescricaoMotivoRejeicaoComando.Add(CodMotivoRejeicaoToDescricao(OcorrenciaOriginal.Tipo,CodMotivo));
         end;

         DataOcorrencia := StringToDateTimeDef( Copy(Linha,111,2)+'/'+
                                                Copy(Linha,113,2)+'/'+
                                                Copy(Linha,115,2),0, 'DD/MM/YY' );

         Vencimento := StringToDateTimeDef( Copy(Linha,147,2)+'/'+
                                            Copy(Linha,149,2)+'/'+
                                            Copy(Linha,151,2),0, 'DD/MM/YY' );

         ValorDocumento       := StrToFloatDef(Copy(Linha,153,13),0)/100;
         ValorIOF             := StrToFloatDef(Copy(Linha,215,13),0)/100;
         ValorAbatimento      := StrToFloatDef(Copy(Linha,228,13),0)/100;
         ValorDesconto        := StrToFloatDef(Copy(Linha,241,13),0)/100;
         ValorRecebido        := StrToFloatDef(Copy(Linha,254,13),0)/100;
         ValorMoraJuros       := StrToFloatDef(Copy(Linha,267,13),0)/100;
         ValorOutrosCreditos  := StrToFloatDef(Copy(Linha,280,13),0)/100;
         NossoNumero          := Copy(Linha,63,11);
         Carteira             := Copy(Linha,107,2);
         ValorDespesaCobranca := StrToFloatDef(Copy(Linha,182,07),0)/100; //--Anderson: Valor tarifa
         ValorOutrasDespesas  := StrToFloatDef(Copy(Linha,189,13),0)/100;

         if StrToIntDef(Copy(Linha,176,6),0) <> 0 then
            DataCredito:= StringToDateTimeDef( Copy(Linha,294,2)+'/'+
                                               Copy(Linha,296,2)+'/'+
                                               Copy(Linha,298,2),0, 'DD/MM/YY' );
      end;
   end;

   fpTamanhoMaximoNossoNum := 10;
end;

end.
