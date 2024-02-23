{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit ACBrBPeConversao;

interface

uses
  SysUtils, StrUtils, Classes,
  pcnConversao;

type
  TStatusACBrBPe = (stBPeIdle, stBPeRecepcao, stBPeRetRecepcao, stBPeConsulta,
                    stBPeStatusServico, stBPeEvento, stBPeEmail,
                    stDistDFeInt, stEnvioWebService);

type
  TVersaoBPe = (ve100);

const
  TVersaoBPeArrayStrings: array[TVersaoBPe] of string = ('1.00');
  TVersaoBPeArrayDouble: array[TVersaoBPe] of Double = (1.00);

type
  TSchemaBPe = (schErroBPe, schBPe, schBPeTM, schconsSitBPe, schconsStatServ,
                schEventoBPe, schdistDFeInt, schevCancBPe, schevNaoEmbBPe,
                schevAlteracaoPoltrona, schevExcessoBagagem);

const
  TSchemaBPeArrayStrings: array[TSchemaBPe] of string = ('', '', '', '',
    '', '', '', 'evCancBPe', 'evNaoEmbBPe', 'evAlteracaoPoltrona',
    'evExcessoBagagem');

type
  TLayOutBPe = (LayBPeRecepcao, LayBPeRecepcaoTM, LayBPeRetRecepcao, LayBPeConsulta,
                LayBPeStatusServico, LayBPeEvento, LayBPeEventoAN, LayDistDFeInt,
                LayBPeURLQRCode, LayURLConsultaBPe);

const
  TLayOutBPeArrayStrings: array[TLayOutBPe] of string = ('BPeRecepcao',
    'BPeRecepcaoTM', 'BPeRetRecepcao', 'BPeConsultaProtocolo', 'BPeStatusServico',
    'BPeRecepcaoEvento', 'BPeRecepcaoEvento', 'BPeDistribuicaoDFe', 'URL-QRCode',
    'URL-ConsultaBPe');

type
  TModeloBPe = (moBPe, moBPeTM);

const
  TModeloBPeArrayStrings: array[TModeloBPe] of string = ('63', '63');

type
  TTipoBPe = (tbNormal, tbSubstituicao, tbBPeTM);

const
  TTipoBPeArrayStrings: array[TTipoBPe] of string = ('0', '3', '4');

type
  TModalBPe = (moRodoviario, moAquaviario, moFerroviario);

const
  TModalBPeArrayStrings: array[TModalBPe] of string = ('1', '3', '4');

type
  TTipoSubstituicao = (tsRemarcacao, tsTransferencia, tsTransfRemarcacao);

const
  TTipoSubstituicaoArrayStrings: array[TTipoSubstituicao] of string = ('1', '2',
    '3');

type
  TTipoDocumento = (tdRG, tdTituloEleitor, tdPassaporte, tdCNH, tdOutros);

const
  TTipoDocumentoArrayStrings: array[TTipoDocumento] of string = ('1', '2', '3',
    '4', '5');
  TTipoDocumentoDescArrayStrings: array[TTipoDocumento] of string = ('RG',
    'Titulo de Eleitor', 'Passaporte', 'CNH', 'Outros');

type
  TTipoViagem = (tvRegular, tvExtra);

const
  TTipoViagemArrayStrings: array[TTipoViagem] of string = ('00', '01');

type
  TTipoServico = (tsConvencionalComSanitario, tsConvencionalSemSanitario,
                  tsSemileito, tsLeitoComAr, tsLeitoSemAr, tsExecutivo,
                  tsSemiurbano, tsLongitudinal, tsTravessia, tsCama,
                  tsMicroOnibus);

const
  TTipoServicoArrayStrings: array[TTipoServico] of string = ('1', '2', '3', '4',
    '5', '6', '7', '8', '9', '10', '11');
  TTipoServicoDescArrayStrings: array[TTipoServico] of string = (
    'Convencional com Sanitario', 'Convencional sem Sanitario', 'Semileito',
    'Leito com Ar', 'Leito sem Ar', 'Executivo', 'Semiurbano', 'Longitudinal',
    'Travessia', 'Cama', 'Micro-Onibus');

type
  TTipoAcomodacao = (taAssento, taRede, taRedeComAr, taCabine, taOutros);

const
  TTipoAcomodacaoArrayStrings: array[TTipoAcomodacao] of string = ('1', '2', '3',
    '4', '5');

type
  TTipoTrecho = (ttNormal, ttTrechoInicial, ttConexao);

const
  TTipoTrechoArrayStrings: array[TTipoTrecho] of string = ('1', '2', '3');

type
  TTipoVeiculo = (tvNenhum, tvMotocicleta, tvAutomovel, tvAtomovelComReboque,
                  tvCaminhonete, tvCaminhoneteComReboque, tvMicroOnibus,
                  tvVan, tvOnibus2ou3Eixos, tvOnibus4Eixos, tvCaminhao34,
                  tvCaminhaoToco, tvCaminhaoTruck, tvCarreta, tvBiTrem,
                  tvRodoTrem, tvRomeuJulieta, tvJamanta6Eixos, tvJamanta5Eixos,
                  tvJamanta4Eixos, tvTratorEsteira, tvPaMecanica, tvPatrol,
                  tvTratorPneuGrande, tvTratorPneuComReboque, tvPneuSemReboque,
                  tvCarroca, tvMobilete, tvBicicleta, tvPassageiro, tvOutros);

const
  TTipoVeiculoArrayStrings: array[TTipoVeiculo] of string = ('00', '01', '02',
    '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15',
    '16', '17', '18', '19', '20', '21', '22', '23', '24', '25', '26', '27', '28',
    '29', '99');

type
  TSitVeiculo = (svVazio, svCarregado, svNaoSeAplica);

const
  TSitVeiculoArrayStrings: array[TSitVeiculo] of string = ('1', '2', '3');

type
  TTipoDesconto = (tdNenhum, tdTarifaPromocional, tdIdoso, tdCrianca, tdDeficiente,
                   tdEstudante, tdAnimalDomestico, tdAcordoColetivo,
                   tdProfissionalemDeslocamento, tdProfissionaldaEmpresa,
                   tdJovem, tdOutrosDesc);

const
  TTipoDescontoArrayStrings: array[TTipoDesconto] of string = ('', '01', '02',
    '03', '04', '05', '06', '07', '08', '09', '10', '99');
  TTipoDescontoDescArrayStrings: array[TTipoDesconto] of string = ('',
    'Tarifa Promocional', 'Idoso', 'Criança', 'Deficiente', 'Estudante',
    'Animal Domestico', 'Acordo Coletivo', 'Profissional em Deslocamento',
    'Profissional da Empresa', 'Jovem', 'Outros');

type
  TTipoComponente = (tcTarifa, tcPedagio, tcTaxaEmbarque, tcSeguro, tcTRM,
                     tcSVI, tcOutros);

const
  TTipoComponenteArrayStrings: array[TTipoComponente] of string = ('01', '02',
    '03', '04', '05', '06', '99');
  TTipoComponenteDescArrayStrings: array[TTipoComponente] of string = ('Tarifa',
    'Pedagio', 'Taxa Embarque', 'Seguro', 'TRM', 'SVI', 'Outros');

type
  TBandeiraCard = (bcVisa, bcMasterCard, bcAmericanExpress, bcSorocred, bcElo,
                   bcDinersClub, bcHipercard, bcAura, bcCabal, bcOutros);

const
  TBandeiraCardArrayStrings: array[TBandeiraCard] of string = ('01', '02', '03',
    '04', '05', '06', '07', '08', '09', '99');
  TBandeiraCardDescArrayStrings: array[TBandeiraCard] of string = ('Visa',
    'MasterCard', 'AmericanExpress', 'Sorocred', 'Elo', 'Diners Club',
    'Hipercard', 'Aura', 'Cabal', 'Outros');

type
  TFormaPagamento = (fpDinheiro, fpCheque, fpCartaoCredito, fpCartaoDebito,
                     fpValeTransporte, fpPIX, fpOutro);

const
  TFormaPagamentoArrayStrings: array[TFormaPagamento] of string = ('01', '02',
    '03', '04', '05', '06', '99');
  TFormaPagamentoDescArrayStrings: array[TFormaPagamento] of string = ('Dinheiro',
    'Cheque', 'Cartão de Crédito', 'Cartão de Débito', 'Vale Transporte', 'PIX',
    'Outro');

type
  TTipoEmissao = (teNormal, teOffLine);

const
  TTipoEmissaoArrayStrings: array[TTipoEmissao] of string = ('1', '2');

type
  // Futuramente deve ir para a unit ACBrDFeConversao
  TCRT = (crtSimplesNacional, crtSimplesExcessoReceita, crtRegimeNormal);

const
  // Futuramente deve ir para a unit ACBrDFeConversao
  TCRTArrayStrings: array[TCRT] of string = ('1', '2', '3');

type
  TPresencaComprador = (pcNao, pcPresencial, pcInternet, pcTeleatendimento,
                        pcEntregaDomicilio, pcPresencialForaEstabelecimento,
                        pcOutros);
const
  TPresencaCompradorArrayStrings: array[TPresencaComprador] of string = ('0',
    '1', '2', '3', '4', '5', '9');

type
  TCSTIcms = (cst00, cst20, cst40, cst41, cst51, cst90, cstSN);

const
  TCSTIcmsArrayStrings: array[TCSTIcms] of string = ('00', '20', '40', '41',
    '51', '90', 'SN');

type
  TtpIntegra = (tiNaoInformado, tiPagIntegrado, tiPagNaoIntegrado);

const
  TtpIntegraArrayStrings: array[TtpIntegra] of string = ('', '1', '2');

{
  Declaração das funções de conversão
}
function StrToTpEventoBPe(out ok: boolean; const s: string): TpcnTpEvento;

function StrToVersaoBPe(const s: string): TVersaoBPe;
function VersaoBPeToStr(const t: TVersaoBPe): string;

function tpBPeToStr(const t: TTipoBPe): string;
function StrToTpBPe(const s: string): TTipoBPe;

function ModalBPeToStr(const t: TModalBPe): string;
function StrToModalBPe(const s: string): TModalBPe;

function DblToVersaoBPe(const d: Double): TVersaoBPe;
function VersaoBPeToDbl(const t: TVersaoBPe): Double;

function LayOutToSchema(const t: TLayOutBPe): TSchemaBPe;

function SchemaBPeToStr(const t: TSchemaBPe): string;
function StrToSchemaBPe(const s: string): TSchemaBPe;

function LayOutBPeToServico(const t: TLayOutBPe): string;
function ServicoToLayOutBPe(const s: string): TLayOutBPe;

function tpSubstituicaoToStr(const t: TTipoSubstituicao): string;
function StrTotpSubstituicao(const s: string): TTipoSubstituicao;

function tpDocumentoToStr(const t: TTipoDocumento): string;
function StrTotpDocumento(const s: string): TTipoDocumento;
function tpDocumentoToDesc(const t: TTipoDocumento): string;

function tpViagemToStr(const t: TTipoViagem): string;
function StrTotpViagem(const s: string): TTipoViagem;

function tpServicoToStr(const t: TTipoServico): string;
function StrTotpServico(const s: string): TTipoServico;
function tpServicoToDesc(const t: TTipoServico): string;

function tpAcomodacaoToStr(const t: TTipoAcomodacao): string;
function StrTotpAcomodacao(const s: string): TTipoAcomodacao;

function tpTrechoToStr(const t: TTipoTrecho): string;
function StrTotpTrecho(const s: string): TTipoTrecho;

function tpVeiculoToStr(const t: TTipoVeiculo): string;
function StrTotpVeiculo(const s: string): TTipoVeiculo;

function SitVeiculoToStr(const t: TSitVeiculo): string;
function StrToSitVeiculo(const s: string): TSitVeiculo;

function tpDescontoToStr(const t: TTipoDesconto): string;
function StrTotpDesconto(const s: string): TTipoDesconto;
function tpDescontoToDesc(const t: TTipoDesconto): string;

function tpComponenteToStr(const t: TTipoComponente): string;
function StrTotpComponente(const s: string): TTipoComponente;
function tpComponenteToDesc(const t: TTipoComponente): string;

function BandeiraCardToStr(const t: TBandeiraCard): string;
function BandeiraCardToDescStr(const t: TBandeiraCard): string;
function StrToBandeiraCard(const s: string): TBandeiraCard;

function FormaPagamentoBPeToStr(const t: TFormaPagamento): string;
function FormaPagamentoBPeToDescricao(const t: TFormaPagamento): string;
function StrToFormaPagamentoBPe(const s: string): TFormaPagamento;

function ModeloBPeToStr(const t: TModeloBPe): string;
function StrToModeloBPe(const s: string): TModeloBPe;
function ModeloBPeToPrefixo(const t: TModeloBPe): string;

function TpEmisBPeToStr(const t: TTipoEmissao): string;
function StrToTpEmisBPe(const s: string): TTipoEmissao;

function CRTToStr(const t: TCRT): string;
function StrToCRT(const s: string): TCRT;

function PresencaCompradorToStr(const t: TPresencaComprador): string;
function StrToPresencaComprador(const s: string): TPresencaComprador;

function CSTICMSToStr(const t: TCSTIcms): string;
function StrToCSTICMS(const s: string): TCSTIcms;

function tpIntegraToStr(const t: TtpIntegra): string;
function StrTotpIntegra(const s: string): TtpIntegra;

implementation

uses
  typinfo,
  ACBrBase;

function StrToTpEventoBPe(out ok: boolean; const s: string): TpcnTpEvento;
begin
  Result := StrToEnumerado(ok, s,
            ['-99999', '110111', '110115', '110116', '110117'],
            [teNaoMapeado, teCancelamento, teNaoEmbarque, teAlteracaoPoltrona,
             teExcessoBagagem]);
end;

function StrToVersaoBPe(const s: string): TVersaoBPe;
var
  idx: TVersaoBPe;
begin
  for idx := Low(TVersaoBPeArrayStrings) to High(TVersaoBPeArrayStrings) do
  begin
    if (TVersaoBPeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TVersaoBPe: %s', [s]);
end;

function VersaoBPeToStr(const t: TVersaoBPe): string;
begin
  result := TVersaoBPeArrayStrings[t];
end;

function DblToVersaoBPe(const d: Double): TVersaoBPe;
var
  idx: TVersaoBPe;
begin
  for idx := Low(TVersaoBPeArrayDouble) to High(TVersaoBPeArrayDouble) do
  begin
    if (TVersaoBPeArrayDouble[idx] = d) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TVersaoBPe: %s',
    [FormatFloat('0.00', d)]);
end;

function VersaoBPeToDbl(const t: TVersaoBPe): Double;
begin
  result := TVersaoBPeArrayDouble[t];
end;

function tpBPeToStr(const t: TTipoBPe): string;
begin
  result := TTipoBPeArrayStrings[t];
end;

function StrToTpBPe(const s: string): TTipoBPe;
var
  idx: TTipoBPe;
begin
  for idx := Low(TTipoBPeArrayStrings) to High(TTipoBPeArrayStrings) do
  begin
    if (TTipoBPeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TTipoBPe: %s', [s]);
end;

function ModalBPeToStr(const t: TModalBPe): string;
begin
  result := TModalBPeArrayStrings[t];
end;

function StrToModalBPe(const s: string): TModalBPe;
var
  idx: TModalBPe;
begin
  for idx := Low(TModalBPeArrayStrings) to High(TModalBPeArrayStrings) do
  begin
    if (TModalBPeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TModalBPe: %s', [s]);
end;

function LayOutToSchema(const t: TLayOutBPe): TSchemaBPe;
begin
  case t of
    LayBPeRecepcao:      Result := schBPe;
    LayBPeRecepcaoTM:    Result := schBPeTM;
    LayBPeConsulta:      Result := schconsSitBPe;
    LayBPeStatusServico: Result := schconsStatServ;
    LayBPeEvento,
    LayBPeEventoAN:      Result := schEventoBPe;
    LayDistDFeInt:       Result := schdistDFeInt;
  else
    Result := schErroBPe;
  end;
end;

function SchemaBPeToStr(const t: TSchemaBPe): string;
begin
  Result := GetEnumName(TypeInfo(TSchemaBPe), Integer( t ) );
  Result := copy(Result, 4, Length(Result)); // Remove prefixo "sch"
end;

function StrToSchemaBPe(const s: string): TSchemaBPe;
var
  P: Integer;
  SchemaStr: string;
  CodSchema: Integer;
begin
  P := pos('_', s);
  if p > 0 then
    SchemaStr := copy(s, 1, P-1)
  else
    SchemaStr := s;

  if LeftStr(SchemaStr, 3) <> 'sch' then
    SchemaStr := 'sch' + SchemaStr;

  CodSchema := GetEnumValue(TypeInfo(TSchemaBPe), SchemaStr );

  if CodSchema = -1 then
  begin
    raise Exception.Create(Format('"%s" não é um valor TSchemaANe válido.',[SchemaStr]));
  end;

  Result := TSchemaBPe( CodSchema );
end;

function LayOutBPeToServico(const t: TLayOutBPe): string;
begin
  result := TLayOutBPeArrayStrings[t];
end;

function ServicoToLayOutBPe(const s: string): TLayOutBPe;
var
  idx: TLayOutBPe;
begin
  for idx := Low(TLayOutBPeArrayStrings) to High(TLayOutBPeArrayStrings) do
  begin
    if (TLayOutBPeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TLayOutBPe: %s', [s]);
end;

function tpSubstituicaoToStr(const t: TTipoSubstituicao): string;
begin
  result := TTipoSubstituicaoArrayStrings[t];
end;

function StrTotpSubstituicao(const s: string): TTipoSubstituicao;
var
  idx: TTipoSubstituicao;
begin
  for idx := Low(TTipoSubstituicaoArrayStrings) to High(TTipoSubstituicaoArrayStrings) do
  begin
    if (TTipoSubstituicaoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TTipoSubstituicao: %s', [s]);
end;

function tpDocumentoToStr(const t: TTipoDocumento): string;
begin
  result := TTipoDocumentoArrayStrings[t];
end;

function StrTotpDocumento(const s: string): TTipoDocumento;
var
  idx: TTipoDocumento;
begin
  for idx := Low(TTipoDocumentoArrayStrings) to High(TTipoDocumentoArrayStrings) do
  begin
    if (TTipoDocumentoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TTipoDocumento: %s', [s]);
end;

function tpDocumentoToDesc(const t: TTipoDocumento): string;
begin
  result := TTipoDocumentoDescArrayStrings[t];
end;

function tpViagemToStr(const t: TTipoViagem): string;
begin
  result := TTipoViagemArrayStrings[t];
end;

function StrTotpViagem(const s: string): TTipoViagem;
var
  idx: TTipoViagem;
begin
  for idx := Low(TTipoViagemArrayStrings) to High(TTipoViagemArrayStrings) do
  begin
    if (TTipoViagemArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TTipoViagem: %s', [s]);
end;

function tpServicoToStr(const t: TTipoServico): string;
begin
  result := TTipoServicoArrayStrings[t];
end;

function StrTotpServico(const s: string): TTipoServico;
var
  idx: TTipoServico;
begin
  for idx := Low(TTipoServicoArrayStrings) to High(TTipoServicoArrayStrings) do
  begin
    if (TTipoServicoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TTipoServico: %s', [s]);
end;

function tpServicoToDesc(const t: TTipoServico): string;
begin
  result := TTipoServicoDescArrayStrings[t];
end;

function tpAcomodacaoToStr(const t: TTipoAcomodacao): string;
begin
  result := TTipoAcomodacaoArrayStrings[t];
end;

function StrTotpAcomodacao(const s: string): TTipoAcomodacao;
var
  idx: TTipoAcomodacao;
begin
  for idx := Low(TTipoAcomodacaoArrayStrings) to High(TTipoAcomodacaoArrayStrings) do
  begin
    if (TTipoAcomodacaoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TTipoAcomodacao: %s', [s]);
end;

function tpTrechoToStr(const t: TTipoTrecho): string;
begin
  result := TTipoTrechoArrayStrings[t];
end;

function StrTotpTrecho(const s: string): TTipoTrecho;
var
  idx: TTipoTrecho;
begin
  for idx := Low(TTipoTrechoArrayStrings) to High(TTipoTrechoArrayStrings) do
  begin
    if (TTipoTrechoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TTipoTrecho: %s', [s]);
end;

function tpVeiculoToStr(const t: TTipoVeiculo): string;
begin
  result := TTipoVeiculoArrayStrings[t];
end;

function StrTotpVeiculo(const s: string): TTipoVeiculo;
var
  idx: TTipoVeiculo;
begin
  for idx := Low(TTipoVeiculoArrayStrings) to High(TTipoVeiculoArrayStrings) do
  begin
    if (TTipoVeiculoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TTipoVeiculo: %s', [s]);
end;

function SitVeiculoToStr(const t: TSitVeiculo): string;
begin
  result := TSitVeiculoArrayStrings[t];
end;

function StrToSitVeiculo(const s: string): TSitVeiculo;
var
  idx: TSitVeiculo;
begin
  for idx := Low(TSitVeiculoArrayStrings) to High(TSitVeiculoArrayStrings) do
  begin
    if (TSitVeiculoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TSitVeiculo: %s', [s]);
end;

function tpDescontoToStr(const t: TTipoDesconto): string;
begin
  result := TTipoDescontoArrayStrings[t];
end;

function StrTotpDesconto(const s: string): TTipoDesconto;
var
  idx: TTipoDesconto;
begin
  for idx := Low(TTipoDescontoArrayStrings) to High(TTipoDescontoArrayStrings) do
  begin
    if (TTipoDescontoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TTipoDesconto: %s', [s]);
end;

function tpDescontoToDesc(const t: TTipoDesconto): string;
begin
  result := TTipoDescontoDescArrayStrings[t];
end;

function tpComponenteToStr(const t: TTipoComponente): string;
begin
  result := TTipoComponenteArrayStrings[t];
end;

function StrTotpComponente(const s: string): TTipoComponente;
var
  idx: TTipoComponente;
begin
  for idx := Low(TTipoComponenteArrayStrings) to High(TTipoComponenteArrayStrings) do
  begin
    if (TTipoComponenteArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TTipoComponente: %s', [s]);
end;

function tpComponenteToDesc(const t: TTipoComponente): string;
begin
  result := TTipoComponenteDescArrayStrings[t];
end;

function BandeiraCardToStr(const t: TBandeiraCard): string;
begin
  result := TBandeiraCardArrayStrings[t];
end;

function BandeiraCardToDescStr(const t: TBandeiraCard): string;
begin
  result := TBandeiraCardDescArrayStrings[t];
end;

function StrToBandeiraCard(const s: string): TBandeiraCard;
var
  idx: TBandeiraCard;
begin
  for idx := Low(TBandeiraCardArrayStrings) to High(TBandeiraCardArrayStrings) do
  begin
    if (TBandeiraCardArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TBandeiraCard: %s', [s]);
end;

function FormaPagamentoBPeToStr(const t: TFormaPagamento): string;
begin
  result := TFormaPagamentoArrayStrings[t];
end;

function FormaPagamentoBPeToDescricao(const t: TFormaPagamento): string;
begin
  result := TFormaPagamentoDescArrayStrings[t];
end;

function StrToFormaPagamentoBPe(const s: string): TFormaPagamento;
var
  idx: TFormaPagamento;
begin
  for idx := Low(TFormaPagamentoArrayStrings) to High(TFormaPagamentoArrayStrings) do
  begin
    if (TFormaPagamentoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TFormaPagamento: %s', [s]);
end;

function ModeloBPeToStr(const t: TModeloBPe): string;
begin
  result := TModeloBPeArrayStrings[t];
end;

function StrToModeloBPe(const s: string): TModeloBPe;
var
  idx: TModeloBPe;
begin
  for idx := Low(TModeloBPeArrayStrings) to High(TModeloBPeArrayStrings) do
  begin
    if (TModeloBPeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TModeloBPe: %s', [s]);
end;

function ModeloBPeToPrefixo(const t: TModeloBPe): string;
begin
  Case t of
    moBPeTM: Result := 'BPeTM';
  else
    Result := 'BPe';
  end;
end;

function TpEmisBPeToStr(const t: TTipoEmissao): string;
begin
  result := TTipoEmissaoArrayStrings[t];
end;

function StrToTpEmisBPe(const s: string): TTipoEmissao;
var
  idx: TTipoEmissao;
begin
  for idx := Low(TTipoEmissaoArrayStrings) to High(TTipoEmissaoArrayStrings) do
  begin
    if (TTipoEmissaoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TTipoEmissao: %s', [s]);
end;

function CRTToStr(const t: TCRT): string;
begin
  result := TCRTArrayStrings[t];
end;

function StrToCRT(const s: string): TCRT;
var
  idx: TCRT;
begin
  for idx := Low(TCRTArrayStrings) to High(TCRTArrayStrings) do
  begin
    if (TCRTArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TCRT: %s', [s]);
end;

function PresencaCompradorToStr(const t: TPresencaComprador): string;
begin
  result := TPresencaCompradorArrayStrings[t];
end;

function StrToPresencaComprador(const s: string): TPresencaComprador;
var
  idx: TPresencaComprador;
begin
  for idx := Low(TPresencaCompradorArrayStrings) to High(TPresencaCompradorArrayStrings) do
  begin
    if (TPresencaCompradorArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TPresencaComprador: %s', [s]);
end;

function CSTICMSToStr(const t: TCSTIcms): string;
begin
  result := TCSTIcmsArrayStrings[t];
end;

function StrToCSTICMS(const s: string): TCSTIcms;
var
  idx: TCSTIcms;
begin
  for idx := Low(TCSTIcmsArrayStrings) to High(TCSTIcmsArrayStrings) do
  begin
    if (TCSTIcmsArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TCSTIcms: %s', [s]);
end;

function tpIntegraToStr(const t: TtpIntegra): string;
begin
  result := TtpIntegraArrayStrings[t];
end;

function StrTotpIntegra(const s: string): TtpIntegra;
var
  idx: TtpIntegra;
begin
  for idx := Low(TtpIntegraArrayStrings) to High(TtpIntegraArrayStrings) do
  begin
    if (TtpIntegraArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpIntegra: %s', [s]);
end;

initialization
  RegisterStrToTpEventoDFe(StrToTpEventoBPe, 'BPe');

end.

