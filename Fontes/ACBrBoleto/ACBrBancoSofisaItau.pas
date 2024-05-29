{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
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
//incluido em 23/04/2023
{$I ACBr.inc}

unit ACBrBancoSofisaItau;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  ACBrBoleto,
  ACBrUtil.Strings, 
  ACBrUtil.DateTime, 
  ACBrValidador,
  ACBrBoletoConversao,
  {$ifdef COMPILER6_UP} DateUtils {$else} ACBrD5 {$endif},  
  ACBrUtil.Base,
  ACBrBancoItau;

type

  { TACBrBancoSofisaItau }

  TACBrBancoSofisaItau = class(TACBrBancoItau)
  private

  protected
  public
    Constructor create(AOwner: TACBrBanco);
    Procedure LerRetorno400 ( ARetorno: TStringList ); override;
    function CodOcorrenciaToTipo(const CodOcorrencia: Integer): TACBrTipoOcorrencia;override;
    function TipoOcorrenciaToDescricao( const TipoOcorrencia: TACBrTipoOcorrencia): String;override;
    function GerarRegistroHeader240(NumeroRemessa: Integer): String; override;
    procedure GerarRegistroHeader400( NumeroRemessa: Integer; aRemessa: TStringList); override;
    function CodMotivoRejeicaoToDescricaoSofisa(const TipoOcorrencia: TACBrTipoOcorrencia; CodMotivo: String): String;
    procedure GerarRegistroTransacao400(ACBrTitulo: TACBrTitulo; aRemessa: TStringList);override;
  end;

implementation

{ TACBrBancoSofisaItau }

constructor TACBrBancoSofisaItau.create(AOwner: TACBrBanco);
begin
   inherited create(AOwner);
   fpNome                   := 'BANCO SOFISA SA';
   fpNumeroCorrespondente   := 637;
end;

function TACBrBancoSofisaItau.GerarRegistroHeader240(NumeroRemessa: Integer): String;
begin
  raise Exception.Create( ACBrStr('Não permitido para o layout deste banco.') );
end;


procedure TACBrBancoSofisaItau.GerarRegistroHeader400( NumeroRemessa: Integer; aRemessa: TStringList);
var
  wLinha: String;
begin
   with ACBrBanco.ACBrBoleto.Cedente do
   begin
      wLinha:= '0'                                        + // 001-001 ID do Registro
               '1'                                        + // 002-002 ID do Arquivo( 1 - Remessa)
               'REMESSA'                                  + // 003-009 Literal de Remessa
               '01'                                       + // 010-011 Código do Tipo de Serviço
               PadRight( 'COBRANCA', 15 )                 + // 012-026 Descrição do tipo de serviço
               PadRight( CodigoTransmissao, 20)           + // 027-046 Codigo da Empresa no Banco
               PadRight( Nome, 30)                        + // 047-076 Nome da Empresa
               '637'                                      + // 077-079 Código
               PadRight('BANCO SOFISA SA', 15)            + // 080-094 Nome do Banco
               FormatDateTime('ddmmyy',Now)               + // 095-100 Data de geração do arquivo
               Space(294)                                 + // 101-394 Brancos
               IntToStrZero(1,6);                           // 395-400 Nr. Sequencial de Remessa

      aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
   end;
end;

procedure TACBrBancoSofisaItau.LerRetorno400(ARetorno: TStringList);
var LCodBanco : Integer;
begin
  try
    LCodBanco := fpNumero;
    fpNumero  := fpNumeroCorrespondente;
    LCodBanco := StrToIntDef(copy(ARetorno.Strings[0],77,3),-1);
    if (LCodBanco <> Numero) and (LCodBanco <> fpNumeroCorrespondente) then
      raise Exception.Create(ACBrStr(ACBrBanco.ACBrBoleto.NomeArqRetorno +
                                     'não é um arquivo de retorno do '+ Nome));

    ACBrBanco.ACBrBoleto.Cedente.CodigoCedente := Trim(Copy(ARetorno[1],18,20));
    inherited;
  finally
    fpNumero := LCodBanco;
  end;

end;
function TACBrBancoSofisaItau.TipoOcorrenciaToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia): String;
var
 CodOcorrencia: Integer;
begin
  Result := '';
  CodOcorrencia := StrToIntDef(TipoOcorrenciaToCod(TipoOcorrencia),0);

  case CodOcorrencia of
    01: Result := '01-Confirma Entrada Título na CIP';
    02: Result := '02-Entrada Confirmada';
    03: Result := '03-Entrada Rejeitada';
    05: Result := '05-Campo Livre Alterado';
    06: Result := '06-Liquidação Normal';
    08: Result := '08-Liquidação em Cartório';
    09: Result := '09-Baixa Automática';
    10: Result := '10-Baixa por ter sido liquidado';
    12: Result := '12-Confirma Abatimento';
    13: Result := '13-Abatimento Cancelado';
    14: Result := '14-Vencimento Alterado';
    15: Result := '15-Baixa Rejeitada';
    16: Result := '16-Instrução Rejeitada';
    19: Result := '19-Confirma Recebimento de Ordem de Protesto';
    20: Result := '20-Confirma Recebimento de Ordem de Sustação';
    22: Result := '22-Seu número alterado';
    23: Result := '23-Título enviado para cartório';
    24: Result := '24-Confirma recebimento de ordem de não protestar';
    28: Result := '28-Débito de Tarifas/Custas – Correspondentes';
    40: Result := '40-Tarifa de Entrada (debitada na Liquidação)';
    43: Result := '43-Baixado por ter sido protestado';
    96: Result := '96-Tarifa Sobre Instruções – Mês anterior';
    97: Result := '97-Tarifa Sobre Baixas – Mês Anterior';
    98: Result := '98-Tarifa Sobre Entradas – Mês Anterior';
    99: Result := '99-Tarifa Sobre Instruções de Protesto/Sustação – Mês Anterior';
  end;
  Result := ACBrSTr(Result);
end;

function TACBrBancoSofisaItau.CodOcorrenciaToTipo(const CodOcorrencia:Integer ) : TACBrTipoOcorrencia;
begin
  case CodOcorrencia of
    01: Result := toRetornoEntradaConfirmadaNaCip;
    02: Result := toRetornoRegistroConfirmado;
    03: Result := toRetornoRegistroRecusado;
    05: Result := toRetornoDadosAlterados;
    06: Result := toRetornoLiquidado;
    08: Result := toRetornoLiquidadoEmCartorio;
    09: Result := toRetornoBaixaAutomatica;
    10: Result := toRetornoBaixaPorTerSidoLiquidado;
    12: Result := toRetornoAbatimentoConcedido;
    13: Result := toRetornoAbatimentoCancelado;
    14: Result := toRetornoVencimentoAlterado;
    15: Result := toRetornoBaixaRejeitada;
    16: Result := toRetornoInstrucaoRejeitada;
    19: Result := toRetornoRecebimentoInstrucaoProtestar;
    20: Result := toRetornoRecebimentoInstrucaoSustarProtesto;
    22: Result := toRetornoDadosAlterados;
    23: Result := toRetornoEncaminhadoACartorio;
    24: Result := toRetornoRecebimentoInstrucaoNaoProtestar;
    28: Result := toRetornoDebitoTarifas;
    40: Result := toRetornoTarifaDeRelacaoDasLiquidacoes;
    43: Result := toRetornoBaixaPorProtesto;
    96: Result := toRetornoDebitoMensalTarifasOutrasInstrucoes;
    97: Result := toRetornoTarifaMensalBaixasCarteira;
    98: Result := toRetornoTarifaMensalRefEntradasBancosCorrespCarteira;
    99: Result := toRetornoDebitoMensalTarifasSustacaoProtestos;
  else
    Result := toRetornoOutrasOcorrencias;
  end;
end;

function TACBrBancoSofisaItau.CodMotivoRejeicaoToDescricaoSofisa( const TipoOcorrencia:TACBrTipoOcorrencia; CodMotivo: String) : String;
begin
   case TipoOcorrencia of
    toRetornoComandoRecusado, toRetornoRegistroRecusado: //03 (Entrada rejeitada)
      begin
         if CodMotivo = '03' then
            Result:='CEP inválido – Não temos cobrador – Cobrador não Localizado'
         else if CodMotivo = '04' then
            Result:='Sigla do Estado inválida'
         else if CodMotivo = '05' then
            Result:='Data de Vencimento inválida ou fora do prazo mínimo'
         else if CodMotivo = '06' then
            Result:='Código do Banco inválido'
         else if CodMotivo = '08' then
            Result:='Nome do sacado não informado'
         else if CodMotivo = '10' then
            Result:='Logradouro não informado'
         else if CodMotivo = '14' then
            Result:='Registro em duplicidade'
         else if CodMotivo = '19' then
            Result:='Data de desconto inválida ou maior que a data de vencimento'
         else if CodMotivo = '20' then
            Result:='Valor de IOF não numérico'
         else if CodMotivo = '21' then
            Result:='Movimento para título não cadastrado no sistema'
         else if CodMotivo = '22' then
            Result:='Valor de desconto + abatimento maior que o valor do título'
         else if CodMotivo = '25' then
            Result:='CNPJ ou CPF do sacado inválido (aceito com restrições)'
         else if CodMotivo = '26' then
            Result:='Espécie de documento inválida'
         else if CodMotivo = '27' then
            Result:='Data de emissão do título inválida'
         else if CodMotivo = '28' then
            Result:='Seu número não informado'
         else if CodMotivo = '29' then
            Result:='CEP é igual a espaço ou zeros ou não numérico'
         else if CodMotivo = '30' then
            Result:='Valor do título não numérico ou inválido'
         else if CodMotivo = '36' then
            Result:='Valor de permanência (mora) não numérico'
         else if CodMotivo = '37' then
            Result:='Valor de permanência inconsistente, pois, dentro de um mês, será maior que o valor do título'
         else if CodMotivo = '38' then
            Result:='Valor de desconto/abatimento não numérico ou inválido'
         else if CodMotivo = '39' then
            Result:='Valor de abatimento não numérico'
         else if CodMotivo = '42' then
            Result:='Título já existente em nossos registros. Nosso número não aceito'
         else if CodMotivo = '43' then
            Result:='Título enviado em duplicidade nesse movimento'
         else if CodMotivo = '44' then
            Result:='Título zerado ou em branco ou não numérico na remessa'
         else if CodMotivo = '46' then
            Result:='Título enviado fora da faixa de Nosso Número, estipulada para o cliente.'
         else if CodMotivo = '51' then
            Result:='Tipo/Número de Inscrição Sacador/Avalista Inválido'
         else if CodMotivo = '52' then
            Result:='Sacador/Avalista não informado'
         else if CodMotivo = '53' then
            Result:='Prazo de vencimento do título excede ao da contratação'
         else if CodMotivo = '54' then
            Result:='Banco informado não é nosso correspondente 140-142'
         else if CodMotivo = '55' then
            Result:='Banco correspondente informado não cobra este CEP ou não possui faixas de CEP cadastradas'
         else if CodMotivo = '56' then
            Result:='Nosso número no correspondente não foi informado'
         else if CodMotivo = '57' then
            Result:='Remessa contendo duas instruções incompatíveis – não protestar e dias de protesto ou prazo para protesto inválido.'
         else if CodMotivo = '58' then
            Result:='Entradas Rejeitadas – Reprovado no Represamento para Análise'
         else if CodMotivo = '60' then
            Result:='CNPJ/CPF do sacado inválido – título recusado'
         else if CodMotivo = '87' then
            Result:='Excede Prazo máximo entre emissão e vencimento'
         else if CodMotivo = 'AA' then
            Result:='Serviço de cobrança inválido'
         else if CodMotivo = 'AB' then
            Result:='Serviço de "0" ou "5" e banco cobrador <> zeros'
         else if CodMotivo = 'AE' then
            Result:='Título não possui abatimento'
         else if CodMotivo = 'AG' then
            Result:='Movto não permitido para título À Vista/Contra Apresentação'
         else if CodMotivo = 'AH' then
            Result:='Cancelamento de Valores Inválidos'
         else if CodMotivo = 'AI' then
            Result:='Nossa carteira inválida'
         else if CodMotivo = 'AJ' then
            Result:='Modalidade com bancos correspondentes inválida'
         else if CodMotivo = 'AK' then
            Result:='Título pertence a outro cliente'
         else if CodMotivo = 'AL' then
            Result:='Sacado impedido de entrar nesta cobrança'
         else if CodMotivo = 'AT' then
            Result:='Valor Pago Inválido'
         else if CodMotivo = 'AU' then
            Result:='Data da ocorrência inválida'
         else if CodMotivo = 'AV' then
            Result:='Valor da tarifa de cobrança inválida'
         else if CodMotivo = 'AX' then
            Result:='Título em pagamento parcial'
         else if CodMotivo = 'AY' then
            Result:='Título em Aberto e Vencido para acatar protestol'
         else if CodMotivo = 'BA' then
            Result:='Banco Correspondente Recebedor não é o Cobrador Atual'
         else if CodMotivo = 'BB' then
            Result:='Título deve estar em cartório para baixar'
         else if CodMotivo = 'BC' then
            Result:='Análise gerencial-sacado inválido p/operação crédito'
         else if CodMotivo = 'BD' then
            Result:='Análise gerencial-sacado inadimplente'
         else if CodMotivo = 'BE' then
            Result:='Análise gerencial-sacado difere do exigido'
         else if CodMotivo = 'BF' then
            Result:='Análise gerencial-vencto excede vencto da operação de crédito'
         else if CodMotivo = 'BG' then
            Result:='Análise gerencial-sacado com baixa liquidez'
         else if CodMotivo = 'BH' then
            Result:='Análise gerencial-sacado excede concentração'
         else if CodMotivo = 'CC' then
            Result:='Valor de iof incompatível com a espécie documento'
         else if CodMotivo = 'CD' then
            Result:='Efetivação de protesto sem agenda válida'
         else if CodMotivo = 'CE' then
            Result:='Título não aceito - pessoa física'
         else if CodMotivo = 'CF' then
            Result:='Excede prazo máximo da entrada ao vencimento'
         else if CodMotivo = 'CG' then
            Result:='Título não aceito – por análise gerencial'
         else if CodMotivo = 'CH' then
            Result:='Título em espera – em análise pelo banco'
         else if CodMotivo = 'CJ' then
            Result:='Análise gerencial-vencto do titulo abaixo przcurto'
         else if CodMotivo = 'CK' then
            Result:='Análise gerencial-vencto do titulo abaixo przlongo CS Título rejeitado pela checagem de duplicatas'
         else if CodMotivo = 'DA' then
            Result:='Análise gerencial – Entrada de Título Descontado com limite cancelado'
         else if CodMotivo = 'DB' then
            Result:='Análise gerencial – Entrada de Título Descontado com limite vencido'
         else if CodMotivo = 'DC' then
            Result:='Análise gerencial - cedente com limite cancelado'
         else if CodMotivo = 'DD' then
            Result:='Análise gerencial – cedente é sacado e teve seu limite cancelado'
         else if CodMotivo = 'DE' then
            Result:='Análise gerencial - apontamento no Serasa'
         else if CodMotivo = 'DG' then
            Result:='Endereço sacador/avalista não informado'
         else if CodMotivo = 'DH' then
            Result:='Cep do sacador/avalista não informado'
         else if CodMotivo = 'DI' then
            Result:='Cidade do sacador/avalista não informado'
         else if CodMotivo = 'DJ' then
            Result:='Estado do sacador/avalista inválido ou n informado'
         else if CodMotivo = 'DM' then
            Result:='Cliente sem Código de Flash cadastrado no cobrador'
         else if CodMotivo = 'DN' then
            Result:='Título Descontado com Prazo ZERO – Recusado'
         else if CodMotivo = 'DP' then
            Result:='Data de Referência menor que a Data de Emissão do Título'
         else if CodMotivo = 'DT' then
            Result:='Nosso Número do Correspondente não deve ser informado'
         else if CodMotivo = 'EB' then
            Result:='HSBC não aceita endereço de sacado com mais de 38 caracteres'
         else if CodMotivo = 'G1' then
            Result:='Endereço do sacador incompleto ( lei 12.039)'
         else if CodMotivo = 'G2' then
            Result:='Sacador impedido de movimentar'
         else if CodMotivo = 'G3' then
            Result:='Concentração de cep não permitida'
         else if CodMotivo = 'G4' then
            Result:='Valor do título não permitido'
         else if CodMotivo = 'HA' then
            Result:='Serviço e Modalidade Incompatíveis'
         else if CodMotivo = 'HB' then
            Result:='Inconsistências entre Registros Título e Sacador'
         else if CodMotivo = 'HC' then
            Result:='Ocorrência não disponível'
         else if CodMotivo = 'HD' then
            Result:='Título com Aceite'
         else if CodMotivo = 'HF' then
            Result:='Baixa Liquidez do Sacado'
         else if CodMotivo = 'HG' then
            Result:='Sacado Informou que não paga Boletos'
         else if CodMotivo = 'HH' then
            Result:='Sacado não confirmou a Nota Fiscal'
         else if CodMotivo = 'HI' then
            Result:='Checagem Prévia não Efetuada'
         else if CodMotivo = 'HJ' then
            Result:='Sacado desconhece compra e Nota Fiscal'
         else if CodMotivo = 'HK' then
            Result:='Compra e Nota Fiscal canceladas pelo sacado'
         else if CodMotivo = 'HL' then
            Result:='Concentração além do permitido pela área de Crédito'
         else if CodMotivo = 'HM' then
            Result:='Vencimento acima do permitido pelo área de Crédito'
         else if CodMotivo = 'HN' then
            Result:='Excede o prazo limite da operação'
         else if CodMotivo = 'IX' then
            Result:='Título de Cartão de Crédito não aceita instruções'
         else if CodMotivo = 'JB' then
            Result:='Título de Cartão de Crédito inválido para o Produto'
         else if CodMotivo = 'JC' then
            Result:='Produto somente para Cartão de Crédito'
         else if CodMotivo = 'JH' then
            Result:='CB Direta com operação de Desconto Automático'
         else if CodMotivo = 'JI' then
            Result:='Espécie de Documento incompatível para produto de Cartão de Crédito';
      end;
    toRetornoBaixaRejeitada:
      begin
         if CodMotivo = '05' then
            Result:='Solicitação de baixa para título já baixado ou liquidado'
         else if CodMotivo = '06' then
            Result:='Solicitação de baixa para título não registrado no sistema'
         else if CodMotivo = '08' then
            Result:='Solicitação de baixa para título em float';
      end;
    toRetornoInstrucaoRejeitada:
      begin
         if CodMotivo = '04' then
            Result:='Data de vencimento não numérica ou inválida'
         else if CodMotivo = '05' then
            Result:='Data de Vencimento inválida ou fora do prazo mínimo'
         else if CodMotivo = '14' then
            Result:='Registro em duplicidade'
         else if CodMotivo = '19' then
            Result:='Data de desconto inválida ou maior que a data de vencimento'
         else if CodMotivo = '20' then
            Result:='Campo livre não informado'
         else if CodMotivo = '21' then
            Result:='Título não registrado no sistema'
         else if CodMotivo = '22' then
            Result:='Título baixado ou liquidado'
         else if CodMotivo = '26' then
            Result:='Espécie de documento inválida'
         else if CodMotivo = '27' then
            Result:='Instrução não aceita, por não ter sido emitida ordem de protesto ao cartório'
         else if CodMotivo = '28' then
            Result:='Título tem instrução de cartório ativa'
         else if CodMotivo = '29' then
            Result:='Título não tem instrução de carteira ativa'
         else if CodMotivo = '30' then
            Result:='Existe instrução de não protestar, ativa para o título'
         else if CodMotivo = '36' then
            Result:='Valor de permanência (mora) não numérico'
         else if CodMotivo = '37' then
            Result:='Título Descontado – Instrução não permitida para a carteira'
         else if CodMotivo = '38' then
            Result:='Valor do abatimento não numérico ou maior que a soma do valor do título + permanência + multa'
         else if CodMotivo = '39' then
            Result:='Título em cartório'
         else if CodMotivo = '40' then
            Result:='Instrução recusada – Reprovado no Represamento para Análise'
         else if CodMotivo = '44' then
            Result:='Título zerado ou em branco ou não numérico na remessa'
         else if CodMotivo = '51' then
            Result:='Tipo/Número de Inscrição Sacador/Avalista Inválido'
         else if CodMotivo = '53' then
            Result:='Prazo de vencimento do título excede ao da contratação'
         else if CodMotivo = '57' then
            Result:='Remessa contendo duas instruções incompatíveis – não protestar e dias de protesto ou prazo para protesto inválido.'
         else if CodMotivo = 'AA' then
            Result:='Serviço de cobrança inválido'
         else if CodMotivo = 'AE' then
            Result:='Título não possui abatimento'
         else if CodMotivo = 'AG' then
            Result:='Movimento não permitido – Título à vista ou contra apresentação'
         else if CodMotivo = 'AH' then
            Result:='Cancelamento de valores inválidos'
         else if CodMotivo = 'AI' then
            Result:='Nossa carteira inválida'
         else if CodMotivo = 'AK' then
            Result:='Título pertence a outro cliente'
         else if CodMotivo = 'AU' then
            Result:='Data da ocorrência inválida'
         else if CodMotivo = 'AY' then
            Result:='Título deve estar em aberto e vencido para acatar protesto'
         else if CodMotivo = 'CB' then
            Result:='Título possui protesto efetivado/a efetivar hoje'
         else if CodMotivo = 'CT' then
            Result:='Título já baixado'
         else if CodMotivo = 'CW' then
            Result:='Título já transferido'
         else if CodMotivo = 'DO' then
            Result:='Título em Prejuízo'
         else if CodMotivo = 'JK' then
            Result:='Produto não permite alteração de valor de título'
         else if CodMotivo = 'JQ' then
            Result:='Título em Correspondente – Não alterar Valor'
         else if CodMotivo = 'JS' then
            Result:='Título possui Descontos/Abto/Mora/Multa'
         else if CodMotivo = 'JT' then
            Result:='Título possui Agenda de Protesto/Devolução'
         else if CodMotivo = '99' then
            Result:='Ocorrência desconhecida na remessa';
      end;
    else
      Result := CodMotivo + ' - Outros Motivos';
    end; //case TipoOcorrencia
end;

procedure TACBrBancoSofisaItau.GerarRegistroTransacao400(ACBrTitulo :TACBrTitulo; aRemessa: TStringList);
var
  Ocorrencia,aEspecie :String;
  Protesto, Multa, valorMulta, aAgencia, TipoSacado, wLinha, ChaveNFe :String;
  aCarteira, I, mensagemBranco, multiplicadorMulta: Integer;
begin

  aCarteira := StrToIntDef(ACBrTitulo.Carteira, 0 );

  if aCarteira = 109  then
    aCarteira := 4
  else
     raise Exception.Create( ACBrStr('Carteira não permitida, aceita por enquanto apenas carteira 109(Código 4).') );

   aAgencia:= '0000'{agencia} + '0'{Digito};

  with ACBrTitulo do
  begin
    {Pegando Código da Ocorrencia}
    case OcorrenciaOriginal.Tipo of
      toRemessaBaixar                        : Ocorrencia := '02'; {Pedido de Baixa}
      toRemessaConcederAbatimento            : Ocorrencia := '04'; {Concessão de Abatimento}
      toRemessaCancelarAbatimento            : Ocorrencia := '05'; {Cancelamento de Abatimento concedido}
      toRemessaAlterarVencimento             : Ocorrencia := '06'; {Alteração de vencimento}
      toRemessaProtestar                     : Ocorrencia := '09'; {Pedido de protesto}
      toRemessaNaoProtestar                  : Ocorrencia := '10'; {Sustar protesto antes do início do ciclo de protesto}
      toRemessaCancelarInstrucaoProtesto     : Ocorrencia := '18'; {Sustar protesto e manter na carteira}
    else
      Ocorrencia := '01';                                          {Remessa}
    end;

    {Pegando Especie}
    if trim(EspecieDoc) = 'DM' then
      aEspecie:= '01'
    else if trim(EspecieDoc) = 'NP' then
      aEspecie:= '02'
    else if trim(EspecieDoc) = 'CH' then
      aEspecie:= '03'
    else if trim(EspecieDoc) = 'LC' then
      aEspecie:= '04'
    else if trim(EspecieDoc) = 'RC' then
      aEspecie:= '05'
    else if trim(EspecieDoc) = 'AP' then
      aEspecie:= '08'
    else if trim(EspecieDoc) = 'DS' then
      aEspecie:= '12'
    else if trim(EspecieDoc) = '99' then
      aEspecie:= '99'
    else
      aEspecie := EspecieDoc;

    {Pegando campo Intruções}
    if (DataProtesto > 0) and (DataProtesto > Vencimento) then //and (Instrucao1 = '06') then
    begin
      Protesto := IntToStrZero(DaysBetween(DataProtesto,Vencimento),2);
      if (trim(Instrucao1) <> '06' )  and (trim(Instrucao2) <> '06' ) then
        If Trim(Instrucao1) = '' then
          Instrucao1 := '06'
        else
          Instrucao2 := '06';
    end
    else
      Protesto:= '00';

    {Pegando Dias Multa}
    if (DataMulta > 0) and (DataMulta > Vencimento) then
    begin
      Multa := IntToStrZero(DaysBetween(DataMulta, Vencimento), 2);
    end
    else
      Multa := '00';

    {Define Valor Multa}
    if MultaValorFixo then
      multiplicadorMulta:= 100
    else
      multiplicadorMulta:= 10000;
    valorMulta:= IntToStrZero( round( PercentualMulta * multiplicadorMulta ), 13);

    {Pegando Tipo de Sacado}
    case Sacado.Pessoa of
      pFisica   : TipoSacado := '01';
      pJuridica : TipoSacado := '02';
    else
      TipoSacado := '99'; //TODO: CHECAR OQ FAZER PARA CEDENTE SEM TIPO
    end;


    {Chave da NFe}
    if ListaDadosNFe.Count>0 then
      ChaveNFe := ListaDadosNFe[0].ChaveNFe
    else
      ChaveNFe := StringOfChar('0', 44);

    with ACBrBoleto do
    begin
      wLinha:= '1'                                                  +  // 1- ID Registro
        IfThen(Cedente.TipoInscricao = pJuridica,'02','01')         +  // 2 a 3
        PadLeft(trim(OnlyNumber(Cedente.CNPJCPF)),14,'0')           +  // 4 a 17
        PadRight(trim(Cedente.CodigoTransmissao),20)                +  // 18 a 37
        PadRight( SeuNumero ,25,' ')                                +  // 38 a 62
        '00000000000'                                               +  // 63 a 73
        //quando é sofisa itaú, o nosso numero nao tem DV, nota 3 item 4.
        PadLeft(RightStr(NossoNumero,13),13,'0')                    +  // 74 a 86 Cobrança direta Título Correspondente
        Space(3)                                                    +  // 87 a 89 Modalidade de Cobrança com bancos correspondentes.
        IfThen(PercentualMulta > 0,'2','0')                         +  // 90 a 90
        valorMulta                                                  +  // 91 a 103
        Multa                                                       +  // 104 a 105 Número de dias após o vencimento para aplicar a multa
        Space(2)                                                    +  // 106 a 107 Identificação da Operação no Banco
        IntToStr(aCarteira)                                         +  // 108 a 108 Código da Carteira
        Ocorrencia                                                  +  // 109 a 110 Identificação da Ocorrência
        PadRight( NumeroDocumento,10,' ')                           +  // 111 a 120
        FormatDateTime( 'ddmmyy', Vencimento)                       +  // 121 a 126
        IntToStrZero( round( ValorDocumento * 100), 13)             +  // 127 a 139
        '341' + aAgencia                                            +  // 140 a 147
        PadLeft(aEspecie, 2) + 'N'                                  +  // 148 a 150
        FormatDateTime( 'ddmmyy', DataDocumento )                   +  // 151 a 156
        PadLeft(trim(Instrucao1),2,'0')                             +  // 157 a 158
        PadLeft(trim(Instrucao2),2,'0')                             +  // 159 a 160
        IntToStrZero( round(ValorMoraJuros * 100 ), 13)             +  // 161 a 173
        IfThen(DataDesconto < EncodeDate(2000,01,01),
               '000000',
               FormatDateTime( 'ddmmyy', DataDesconto))             +  // 174 a 179
        IntToStrZero( round( ValorDesconto * 100), 13)              +  // 180 a 192
        IntToStrZero( round( ValorIOF * 100 ), 13)                  +  // 193 a 205
        IntToStrZero( round( ValorAbatimento * 100 ), 13)           +  // 206 a 218
        TipoSacado + PadLeft(OnlyNumber(Sacado.CNPJCPF),14,'0')     +  // 219 a 234
        PadRight( Sacado.NomeSacado, 40, ' ')                       +  // 235 a 274
        PadRight( Sacado.Logradouro + ' '+ Sacado.Numero, 40, ' ')  +  // 275 a 314
        PadRight( Sacado.Bairro,12,' ')                             +  // 315 a 326
        PadRight( OnlyNumber(Sacado.CEP) , 8, ' ' )                 +  // 327 a 334
        PadRight( Sacado.Cidade, 15, ' ')                           +
        PadRight( Sacado.UF, 2 )                                    +  // 335 a 351
        PadRight( Sacado.NomeSacado, 30, ' ')                       +  // 352 a 381
        Space(4)                                                    +  // 382 a 385
        Space(6)                                                    +  // 386 a 391
        Protesto + '0'                                              +  // 392 a 394
        IntToStrZero( aRemessa.Count + 1, 6 )                       +  // 395 a 400
        ChaveNFe;                                                      // 401 a 444


      wLinha:= UpperCase(wLinha);
      if Mensagem.Count > 0 then
      begin
        wLinha:= wLinha + #13#10                         +
                 '2' + '0';
        for I := 0 to Mensagem.Count - 1 do
        begin
          if i = 5  then
            Break;

          wLinha := wLinha +
            PadRight(Mensagem[I],69);

        end;

        mensagemBranco := (5 - i) * 69;

        wLinha := wLinha + Space(mensagemBranco) + Space(47);
        wLinha := wLinha +  IntToStrZero(aRemessa.Count  + 2, 6 );
      end;

      aRemessa.Text:= aRemessa.Text + UpperCase(wLinha);
    end;
  end;
end;

end.


