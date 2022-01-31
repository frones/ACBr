{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrPagForConversao;

interface

uses
  SysUtils, StrUtils,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  Classes;

type
  TVersaoLayout = (ve084);

  TBanco = (pagNenhum, pagBancoDoBrasil, pagSantander, pagCaixaEconomica,
            pagCaixaSicob, pagBradesco, pagItau, pagBancoMercantil, pagSicred,
            pagBancoob, pagBanrisul, pagBanestes, pagHSBC, pagBancoDoNordeste,
            pagBRB, pagBicBanco, pagBradescoSICOOB, pagBancoSafra,
            pagSafraBradesco, pagBancoCECRED, pagUnibanco);

  TTipoInscricao = (tiIsento, tiCPF, tiCNPJ, tiPISPASEP, tiOutros);

  TTipoArquivo = (taRemessa, taRetorno);

  TTipoServico = (tsCobranca, tsBloquetoEletronico, tsConciliacaoBancaria,
                  tsDebitos, tsCustodiaCheques, tsGestaoCaixa,
                  tsConsultaMargem, tsAverbacaoConsignacao,
                  tsPagamentoDividendos, tsManutencaoConsignacao,
                  tsConsignacaoParcelas, tsGlosaConsignacao,
                  tsConsultaTributosaPagar, tsDebentures, tsPagamentoFornecedor,
                  tsPagamentoContas, tsCompror, tsComprorRotativo,
                  tsAlegacaoSacado, tsPagamentoSalarios,
                  tsPagamentoHonorarios, tsPagamentoBolsaAuxilio,
                  tsPagamentoPrebenda, tsVendor, tsVendoraTermo,
                  tsPagamentoSinistrosSegurado, tsPagamentoDespesaViagem,
                  tsPagamentoAutorizado, tsPagamentoCredenciados,
                  tsPagamentoRemuneracao, tsPagamentoRepresentantes,
                  tsPagamentoBeneficios, tsPagamentosDiversos, tsNenhum);

  TFormaLancamento = (flCreditoContaCorrente, flChequePagamento, flDocTed,
                      flCartaoSalario, flCreditoContaPoupanca, flCreditoContaCorrenteMesmaTitularidade, flDocMesmaTitularidade, flOPDisposicao,
                      flPagamentoContas, flPagamentoConcessionarias, flTributoDARFNormal, flTributoGPS,
                      flTributoDARFSimples, flTributoIPTU,
                      flPagamentoAutenticacao, flTributoDARJ,
                      flTributoGARESPICMS, flTributoGARESPDR,
                      flTributoGARESPITCMD, flTributoIPVA,
                      flTributoLicenciamento, flTributoDPVAT,
                      flLiquidacaoTitulosProprioBanco,
                      flLiquidacaoTitulosOutrosBancos, flLiberacaoTitulosNotaFiscalEletronica,
                      flLiquidacaoParcelasNaoRegistrada, flFGTSGFIP,
                      flExtratoContaCorrente, flTEDOutraTitularidade,
                      flTEDMesmaTitularidade, flTEDTransferencia,
                      flDebitoContaCorrente, flExtratoGestaoCaixa,
                      flDepositoJudicialContaCorrente, flCartaoSalarioItau,
                      flDepositoJudicialPoupanca, flExtratoContaInvestimento,
                      flTributoGNRe, flNenhum);

  TTipoMovimento = (tmInclusao, tmConsulta, tmEstorno, tmAlteracao,
                    tmLiquidacao, tmExclusao);

  TInstrucaoMovimento = (imInclusaoRegistroDetalheLiberado,
                         imInclusaoRegistroDetalheBloqueado,
                         imAltecacaoPagamentoLiberadoparaBloqueio,
                         imAlteracaoPagamentoBloqueadoparaLiberado,
                         imAlteracaoValorTitulo,
                         imAlteracaoDataPagamento,
                         imPagamentoDiretoFornecedor,
                         imManutencaoemCarteira,
                         imRetiradadeCarteira,
                         imEstornoDevolucaoCamaraCentralizadora,
                         imAlegacaoSacado,
                         imExclusaoRegistro);

  TTipoMoeda = (tmBonusTesouroNacional, tmReal, tmDolarAmericano,
                tmEscudoPortugues, tmFrancoFrances, tmFrancoSuico,
                tmIenJapones, tmIndiceGeralPrecos, tmIndiceGeralPrecosMercado,
                tmLibraEsterlina, tmMarcoAlemao, tmTaxaReferencialDiaria,
                tmUnidadePadraoCapital, tmUnidadePadraoFinanciamento,
                tmUnidadeFiscalReferencia, tmUnidadeMonetariaEuropeia);

  TTipoTributo = (ttGPS, ttDARFNormal, ttDARFSimples, ttDARJ, ttGareICMS,
                  ttIPVA, ttDPVAT, ttLicenciamento, ttFGTS);

  TIndTributo = (itNenhum, itDANFNormal, itDARFSimples, itGPS, itDARJ,
                 itIPVA, itLicenciamento, itDPVAT);

  TTipoOperacao = (toCredito, toDebito, toExtrato, toGestao,
                   toInformacao, toRemessa, toRetorno);

  TTipoMovimentoPagto = (tmpCredito,
                         tmpDebito,
                         tmpAcumulado,
                         tmpRendimentoTributavelDeducaoIRRF,
                         tmpRendimentoIsentoNaoTributavel,
                         tmpRendimentoSujeitoTributacaoExclusiva,
                         tmpRendimentoRecebidoAcumuladamente,
                         tmpInformacaoComplementar);

  TCodigoPagamentoGps = (cpgContribuinteIndividualRecolhimentoMensal,     //NIT PIS PASEP
                         cpgContribuinteIndividualRecolhimentoTrimestral, //NIT PIS PASEP
                         cpgSecuradoFacultativoRecolhimentoMensal,        //NIT PIS PASEP
                         cpgSecuradoFacultativoRecolhimentoTrimestral,    //NIT PIS PASEP
                         cpgSecuradoEspecialRecolhimentoMensal,           //NIT PIS PASEP
                         cpgSecuradoEspecialRecolhimentoTrimestral,       //NIT PIS PASEP
                         cpgEmpresasOptantesSimplesCNPJ,
                         cpgEmpresasGeralCNPJ,
                         cpgEmpresasGeralCEI,
                         cpgContribuicaoRetidaSobreNfFaturaEmpresaPrestadoraServicoCNPJ,
                         ReclamatoriaTrabalhistaCNPJ);

function StrToEnumerado(var ok: boolean; const s: string; const AString: array of string; const AEnumerados: array of variant): variant;
function EnumeradoToStr(const t: variant; const AString: array of string; const AEnumerados: array of variant): variant;

function BancoToStr(const t: TBanco): String;
function BancoToDesc(const t: TBanco): String;
function StrToBanco(var ok: boolean; const s: String): TBanco;

function TpInscricaoToStr(const t: TTipoInscricao): String;
function StrToTpInscricao(var ok: boolean; const s: string): TTipoInscricao;

function TpArquivoToStr(const t: TTipoArquivo): String;
function StrToTpArquivo(var ok: boolean; const s: String): TTipoArquivo;

function TpServicoToStr(const t: TTipoServico): String;
function StrToTpServico(var ok: boolean; const s: String): TTipoServico;

function FmLancamentoToStr(const t: TFormaLancamento): String;
function StrToFmLancamento(var ok: boolean; const s: String): TFormaLancamento;

function TpMovimentoToStr(const t: TTipoMovimento): String;
function StrToTpMovimento(var ok:boolean; const s: string): TTipoMovimento;

function InMovimentoToStr(const t: TInstrucaoMovimento): String;
function StrToInMovimento(var ok: boolean; const s: string): TInstrucaoMovimento;

function TpMoedaToStr(const t: TTipoMoeda): String;

function TpIndTributoToStr(const t: TIndTributo): String;

function TpOperacaoToStr(const t: TTipoOperacao): String;
function StrToTpOperacao(var ok: boolean; const s:string): TTipoOperacao;

function TpMovimentoPagtoToStr(const t: TTipoMovimentoPagto): String;
function StrToTpMovimentoPagto(var ok:boolean; const s:string): TTipoMovimentoPagto;

function CodigoPagamentoGpsToStr(const t: TCodigoPagamentoGps): String;
function StrToCodigoPagamentoGps(var ok: boolean; const s: string): TCodigoPagamentoGps;

function TpTributoToStr(const t: TTipoTributo): String;

function DescricaoRetornoItau(const ADesc: string): string;

function DescricaoRetornoSantander(const ADesc: string): string;

function DescricaoRetornoBancoDoBrasil(const ADesc: string): string;

implementation

function StrToEnumerado(var ok: boolean; const s: string; const AString:
  array of string; const AEnumerados: array of variant): variant;
var
  i: integer;
begin
  result := -1;
  for i := Low(AString) to High(AString) do
    if AnsiSameText(s, AString[i]) then
      result := AEnumerados[i];
  ok := result <> -1;
  if not ok then
    result := AEnumerados[0];
end;

function EnumeradoToStr(const t: variant; const AString:
  array of string; const AEnumerados: array of variant): variant;
var
  i: integer;
begin
  result := '';
  for i := Low(AEnumerados) to High(AEnumerados) do
    if t = AEnumerados[i] then
      result := AString[i];
end;

///////////////////////////////////////////////////////////////////////////////
(*
 cbBancos.Items.Add('246-D - Banco ABC Brasil S.A.');
 cbBancos.Items.Add('025-D - Banco Alfa S.A.');
 cbBancos.Items.Add('641-D - Banco Alvorada S.A.');
 cbBancos.Items.Add('029-D - Banco Banerj S.A.');
 cbBancos.Items.Add('000-D - Banco Bankpar S.A.');
 cbBancos.Items.Add('740-D - Banco Barclays S.A.');
 cbBancos.Items.Add('107-D - Banco BBM S.A.');
 cbBancos.Items.Add('031-D - Banco Beg S.A.');
 cbBancos.Items.Add('739-D - Banco BGN S.A.');
 cbBancos.Items.Add('096-D - Banco BM&F de Serviços de Liquidação e Custódia S.A');
 cbBancos.Items.Add('318-D - Banco BMG S.A.');
 cbBancos.Items.Add('752-D - Banco BNP Paribas Brasil S.A.');
 cbBancos.Items.Add('248-D - Banco Boavista Interatlântico S.A.');
 cbBancos.Items.Add('218-D - Banco Bonsucesso S.A.');
 cbBancos.Items.Add('036-D - Banco Bradesco BBI S.A.');
 cbBancos.Items.Add('204-D - Banco Bradesco Cartões S.A.');
 cbBancos.Items.Add('394-D - Banco Bradesco Financiamentos S.A.');
 cbBancos.Items.Add('225-D - Banco Brascan S.A.');
 cbBancos.Items.Add('208-D - Banco BTG Pactual S.A.');
 cbBancos.Items.Add('044-D - Banco BVA S.A.');
 cbBancos.Items.Add('263-D - Banco Cacique S.A.');
 cbBancos.Items.Add('473-D - Banco Caixa Geral - Brasil S.A.');
 cbBancos.Items.Add('040-D - Banco Cargill S.A.');
 cbBancos.Items.Add('745-D - Banco Citibank S.A.');
 cbBancos.Items.Add('215-D - Banco Comercial e de Investimento Sudameris S.A.');
 cbBancos.Items.Add('748-D - Banco Cooperativo Sicredi S.A.');
 cbBancos.Items.Add('222-D - Banco Credit Agricole Brasil S.A.');
 cbBancos.Items.Add('505-D - Banco Credit Suisse (Brasil) S.A.');
 cbBancos.Items.Add('229-D - Banco Cruzeiro do Sul S.A.');
 cbBancos.Items.Add('003-D - Banco da Amazônia S.A.');
 cbBancos.Items.Add('083-3 - Banco da China Brasil S.A.');
 cbBancos.Items.Add('707-D - Banco Daycoval S.A.');
 cbBancos.Items.Add('024-D - Banco de Pernambuco S.A. - BANDEPE');
 cbBancos.Items.Add('456-D - Banco de Tokyo-Mitsubishi UFJ Brasil S.A.');
 cbBancos.Items.Add('214-D - Banco Dibens S.A.');
 cbBancos.Items.Add('047-D - Banco do Estado de Sergipe S.A.');
 cbBancos.Items.Add('037-D - Banco do Estado do Pará S.A.');
 cbBancos.Items.Add('041-D - Banco do Estado do Rio Grande do Sul S.A.');
 cbBancos.Items.Add('004-D - Banco do Nordeste do Brasil S.A.');
 cbBancos.Items.Add('265-D - Banco Fator S.A.');
 cbBancos.Items.Add('224-D - Banco Fibra S.A.');
 cbBancos.Items.Add('626-D - Banco Ficsa S.A.');
 cbBancos.Items.Add('233-D - Banco GE Capital S.A.');
 cbBancos.Items.Add('612-D - Banco Guanabara S.A.');
 cbBancos.Items.Add('063-D - Banco Ibi S.A. Banco Múltiplo');
 cbBancos.Items.Add('604-D - Banco Industrial do Brasil S.A.');
 cbBancos.Items.Add('320-D - Banco Industrial e Comercial S.A.');
 cbBancos.Items.Add('653-D - Banco Indusval S.A.');
 cbBancos.Items.Add('630-D - Banco Intercap S.A.');
 cbBancos.Items.Add('249-D - Banco Investcred Unibanco S.A.');
 cbBancos.Items.Add('184-D - Banco Itaú BBA S.A.');
 cbBancos.Items.Add('479-D - Banco ItaúBank S.A');
 cbBancos.Items.Add('376-D - Banco J. P. Morgan S.A.');
 cbBancos.Items.Add('074-D - Banco J. Safra S.A.');
 cbBancos.Items.Add('217-D - Banco John Deere S.A.');
 cbBancos.Items.Add('065-D - Banco Lemon S.A.');
 cbBancos.Items.Add('600-D - Banco Luso Brasileiro S.A.');
 cbBancos.Items.Add('755-D - Banco Merrill Lynch de Investimentos S.A.');
 cbBancos.Items.Add('746-D - Banco Modal S.A.');
 cbBancos.Items.Add('045-D - Banco Opportunity S.A.');
 cbBancos.Items.Add('623-D - Banco Panamericano S.A.');
 cbBancos.Items.Add('611-D - Banco Paulista S.A.');
 cbBancos.Items.Add('643-D - Banco Pine S.A.');
 cbBancos.Items.Add('638-D - Banco Prosper S.A.');
 cbBancos.Items.Add('747-D - Banco Rabobank International Brasil S.A.');
 cbBancos.Items.Add('633-D - Banco Rendimento S.A.');
 cbBancos.Items.Add('072-D - Banco Rural Mais S.A.');
 cbBancos.Items.Add('453-D - Banco Rural S.A.');
 cbBancos.Items.Add('422-D - Banco Safra S.A.');
 cbBancos.Items.Add('250-D - Banco Schahin S.A.');
 cbBancos.Items.Add('749-D - Banco Simples S.A.');
 cbBancos.Items.Add('366-D - Banco Société Générale Brasil S.A.');
 cbBancos.Items.Add('637-D - Banco Sofisa S.A.');
 cbBancos.Items.Add('012-D - Banco Standard de Investimentos S.A.');
 cbBancos.Items.Add('464-D - Banco Sumitomo Mitsui Brasileiro S.A.');
 cbBancos.Items.Add('082-5 - Banco Topázio S.A.');
 cbBancos.Items.Add('634-D - Banco Triângulo S.A.');
 cbBancos.Items.Add('655-D - Banco Votorantim S.A.');
 cbBancos.Items.Add('610-D - Banco VR S.A.');
 cbBancos.Items.Add('370-D - Banco WestLB do Brasil S.A.');
 cbBancos.Items.Add('021-D - BANESTES S.A. Banco do Estado do Espírito Santo');
 cbBancos.Items.Add('719-D - Banif-Banco Internacional do Funchal (Brasil)S.A.');
 cbBancos.Items.Add('073-D - BB Banco Popular do Brasil S.A.');
 cbBancos.Items.Add('078-D - BES Investimento do Brasil S.A.-Banco de Investimento');
 cbBancos.Items.Add('069-D - BPN Brasil Banco Múltiplo S.A.');
 cbBancos.Items.Add('070-D - BRB - Banco de Brasília S.A.');
 cbBancos.Items.Add('477-D - Citibank N.A.');
 cbBancos.Items.Add('081-7 - Concórdia Banco S.A.');
 cbBancos.Items.Add('487-D - Deutsche Bank S.A. - Banco Alemão');
 cbBancos.Items.Add('751-D - Dresdner Bank Brasil S.A. - Banco Múltiplo');
 cbBancos.Items.Add('064-D - Goldman Sachs do Brasil Banco Múltiplo S.A.');
 cbBancos.Items.Add('062-D - Hipercard Banco Múltiplo S.A.');
 cbBancos.Items.Add('399-D - HSBC Bank Brasil S.A. - Banco Múltiplo');
 cbBancos.Items.Add('492-D - ING Bank N.V.');
 cbBancos.Items.Add('652-D - Itaú Unibanco Holding S.A.');
 cbBancos.Items.Add('341-D - Itaú Unibanco S.A.');
 cbBancos.Items.Add('488-D - JPMorgan Chase Bank');
 cbBancos.Items.Add('409-D - UNIBANCO - União de Bancos Brasileiros S.A.');
 cbBancos.Items.Add('230-D - Unicard Banco Múltiplo S.A.');
*)
function BancoToStr(const t: TBanco): String;
begin
  result := EnumeradoToStr(t, ['000', '001', '033', '104',
                               '000', '237', '341', '389', '748',
                               '756', '000', '000', '399', '000',
                               '000', '000', '000', '422',
                               '000', '085', '409'],
           [pagNenhum, pagBancoDoBrasil, pagSantander, pagCaixaEconomica,
            pagCaixaSicob, pagBradesco, pagItau, pagBancoMercantil, pagSicred,
            pagBancoob, pagBanrisul, pagBanestes, pagHSBC, pagBancoDoNordeste,
            pagBRB, pagBicBanco, pagBradescoSICOOB, pagBancoSafra,
            pagSafraBradesco, pagBancoCECRED, pagUnibanco]);
end;

function BancoToDesc(const t: TBanco): String;
begin
  result := EnumeradoToStr(t, ['Nenhum', 'Banco do Brasil', 'Santander',
                               'Caixa Economica Federal', 'Caixa Sicob',
                               'Bradesco', 'Itau', 'Banco Mercantil',
                               'Sicred', 'Bancoob', 'Banrisul', 'Banestes',
                               'HSBC', 'Banco do Nordeste', 'BRB',
                               'Bic Banco', 'Bradesco Sicob', 'Banco Safra',
                               'Safra Bradesco', 'Ailos', 'Unibanco'],
           [pagNenhum, pagBancoDoBrasil, pagSantander, pagCaixaEconomica,
            pagCaixaSicob, pagBradesco, pagItau, pagBancoMercantil, pagSicred,
            pagBancoob, pagBanrisul, pagBanestes, pagHSBC, pagBancoDoNordeste,
            pagBRB, pagBicBanco, pagBradescoSICOOB, pagBancoSafra,
            pagSafraBradesco, pagBancoCECRED, pagUnibanco]);
end;

function StrToBanco(var ok: boolean; const s: String): TBanco;
begin
  Result := StrToEnumerado(ok, s,
                           ['000', '001', '033', '104', '000', '237', '341',
                            '389', '748', '756', '000', '000', '399', '000',
                            '000', '000', '000', '422', '000', '085', '409'],
                           [pagNenhum, pagBancoDoBrasil, pagSantander, pagCaixaEconomica,
                            pagCaixaSicob, pagBradesco, pagItau, pagBancoMercantil, pagSicred,
                            pagBancoob, pagBanrisul, pagBanestes, pagHSBC, pagBancoDoNordeste,
                            pagBRB, pagBicBanco, pagBradescoSICOOB, pagBancoSafra,
                            pagSafraBradesco, pagBancoCECRED, pagUnibanco]);
end;

function TpInscricaoToStr(const t: TTipoInscricao): String;
begin
 result := EnumeradoToStr(t, ['0', '1', '2', '3', '9'],
                             [tiIsento, tiCPF, tiCNPJ, tiPISPASEP, tiOutros]);
end;

function StrToTpInscricao(var ok: boolean; const s: string): TTipoInscricao;
begin
 result := StrToEnumerado(ok, s, ['0', '1', '2', '3', '9'],
                                 [tiIsento, tiCPF, tiCNPJ, tiPISPASEP, tiOutros]);
end;

function TpArquivoToStr(const t: TTipoArquivo): String;
begin
 result := EnumeradoToStr(t, ['1', '2'], [taRemessa, taRetorno]);
end;

function StrToTpArquivo(var ok: boolean; const s: String): TTipoArquivo;
begin
 result := StrToEnumerado(ok, s, ['1', '2'], [taRemessa, taRetorno]);
end;

function TpServicoToStr(const t: TTipoServico): String;
begin
 result := EnumeradoToStr(t, ['01', '03', '04', '05', '06', '07', '08',
                           '09', '10', '11', '12', '13', '14', '15', '20', '22', '25',
                           '26', '29', '30', '32', '33', '34', '40', '41', '50',
                           '60', '70', '75', '77', '80', '90', '98', '  '],
                       [tsCobranca, tsBloquetoEletronico, tsConciliacaoBancaria,
                        tsDebitos, tsCustodiaCheques, tsGestaoCaixa,
                        tsConsultaMargem, tsAverbacaoConsignacao,
                        tsPagamentoDividendos, tsManutencaoConsignacao,
                        tsConsignacaoParcelas, tsGlosaConsignacao,
                        tsConsultaTributosaPagar, tsDebentures,
                        tsPagamentoFornecedor,
                        tsPagamentoContas, tsCompror, tsComprorRotativo,
                        tsAlegacaoSacado, tsPagamentoSalarios,
                        tsPagamentoHonorarios, tsPagamentoBolsaAuxilio,
                        tsPagamentoPrebenda, tsVendor, tsVendoraTermo,
                        tsPagamentoSinistrosSegurado, tsPagamentoDespesaViagem,
                        tsPagamentoAutorizado, tsPagamentoCredenciados,
                        tsPagamentoRemuneracao, tsPagamentoRepresentantes,
                        tsPagamentoBeneficios, tsPagamentosDiversos,
                        tsNenhum]);
end;

function StrToTpServico(var ok: boolean; const s: String): TTipoServico;
begin
  result := StrToEnumerado(ok, s,
                           ['01', '03', '04', '05', '06', '07', '08',
                           '09', '10', '11', '12', '13', '14', '15', '20', '22', '25',
                           '26', '29', '30', '32', '33', '34', '40', '41', '50',
                           '60', '70', '75', '77', '80', '90', '98', '  '],
                       [tsCobranca, tsBloquetoEletronico, tsConciliacaoBancaria,
                        tsDebitos, tsCustodiaCheques, tsGestaoCaixa,
                        tsConsultaMargem, tsAverbacaoConsignacao,
                        tsPagamentoDividendos, tsManutencaoConsignacao,
                        tsConsignacaoParcelas, tsGlosaConsignacao,
                        tsConsultaTributosaPagar, tsDebentures,
                        tsPagamentoFornecedor,
                        tsPagamentoContas, tsCompror, tsComprorRotativo,
                        tsAlegacaoSacado, tsPagamentoSalarios,
                        tsPagamentoHonorarios, tsPagamentoBolsaAuxilio,
                        tsPagamentoPrebenda, tsVendor, tsVendoraTermo,
                        tsPagamentoSinistrosSegurado, tsPagamentoDespesaViagem,
                        tsPagamentoAutorizado, tsPagamentoCredenciados,
                        tsPagamentoRemuneracao, tsPagamentoRepresentantes,
                        tsPagamentoBeneficios, tsPagamentosDiversos,
                        tsNenhum]);
end;

function StrToFmLancamento(var ok: boolean; const s: String): TFormaLancamento;
begin
 result := StrToEnumerado(ok, s,
                          ['01', '02', '03', '04', '05', '06', '07', '10', '11', '13',
                           '16', '17', '18', '19', '20', '21', '22', '23', '24', '25',
                           '26', '27', '30', '31', '32', '33', '35', '40', '41', '43',
                           '44', '50', '60', '70', '71', '72', '73', '91', '  '],
                         [flCreditoContaCorrente, flChequePagamento, flDocTed,
                          flCartaoSalario, flCreditoContaPoupanca, flCreditoContaCorrenteMesmaTitularidade, flDocMesmaTitularidade, flOPDisposicao,
                          flPagamentoContas, flPagamentoConcessionarias, flTributoDARFNormal, flTributoGPS,
                          flTributoDARFSimples, flTributoIPTU,
                          flPagamentoAutenticacao, flTributoDARJ,
                          flTributoGARESPICMS, flTributoGARESPDR,
                          flTributoGARESPITCMD, flTributoIPVA,
                          flTributoLicenciamento, flTributoDPVAT,
                          flLiquidacaoTitulosProprioBanco,
                          flLiquidacaoTitulosOutrosBancos, flLiberacaoTitulosNotaFiscalEletronica,
                          flLiquidacaoParcelasNaoRegistrada, flFGTSGFIP,
                          flExtratoContaCorrente, flTEDOutraTitularidade,
                          flTEDMesmaTitularidade, flTEDTransferencia,
                          flDebitoContaCorrente, flCartaoSalarioItau,
                          flExtratoGestaoCaixa, flDepositoJudicialContaCorrente,
                          flDepositoJudicialPoupanca, flExtratoContaInvestimento,
                          flTributoGNRe, flNenhum]);
end;

function FmLancamentoToStr(const t: TFormaLancamento): String;
begin
 result := EnumeradoToStr(t,
                          ['01', '02', '03', '04', '05', '06', '07', '10', '11', '13',
                           '16', '17', '18', '19', '20', '21', '22', '23', '24', '25',
                           '26', '27', '30', '31', '32', '33', '35', '40', '41', '43',
                           '44', '50', '60', '70', '71', '72', '73', '91', '  '],
                         [flCreditoContaCorrente, flChequePagamento, flDocTed,
                          flCartaoSalario, flCreditoContaPoupanca, flCreditoContaCorrenteMesmaTitularidade, flDocMesmaTitularidade, flOPDisposicao,
                          flPagamentoContas, flPagamentoConcessionarias, flTributoDARFNormal, flTributoGPS,
                          flTributoDARFSimples, flTributoIPTU,
                          flPagamentoAutenticacao, flTributoDARJ,
                          flTributoGARESPICMS, flTributoGARESPDR,
                          flTributoGARESPITCMD, flTributoIPVA,
                          flTributoLicenciamento, flTributoDPVAT,
                          flLiquidacaoTitulosProprioBanco,
                          flLiquidacaoTitulosOutrosBancos, flLiberacaoTitulosNotaFiscalEletronica,
                          flLiquidacaoParcelasNaoRegistrada, flFGTSGFIP,
                          flExtratoContaCorrente, flTEDOutraTitularidade,
                          flTEDMesmaTitularidade, flTEDTransferencia,
                          flDebitoContaCorrente, flCartaoSalarioItau,
                          flExtratoGestaoCaixa, flDepositoJudicialContaCorrente,
                          flDepositoJudicialPoupanca, flExtratoContaInvestimento,
                          flTributoGNRe, flNenhum]);
end;

function TpMovimentoToStr(const t: TTipoMovimento): String;
begin
 result := EnumeradoToStr(t, ['0', '1', '2', '3', '5', '7', '9'],
                              [tmInclusao, tmConsulta, tmEstorno, tmAlteracao,
                               tmLiquidacao, tmExclusao]);
end;

function StrToTpMovimento(var ok:boolean; const s: string): TTipoMovimento;
begin
 result := StrToEnumerado(ok, s,
                              ['0', '1', '2', '3', '5', '7', '9'],
                              [tmInclusao, tmConsulta, tmEstorno, tmAlteracao,
                               tmLiquidacao, tmExclusao]);
end;

function InMovimentoToStr(const t: TInstrucaoMovimento): String;
begin
 result := EnumeradoToStr(t, ['00', '09', '10', '11', '17', '19', '23',
                                       '25', '27', '33', '40', '99'],
                              [imInclusaoRegistroDetalheLiberado,
                               imInclusaoRegistroDetalheBloqueado,
                               imAltecacaoPagamentoLiberadoparaBloqueio,
                               imAlteracaoPagamentoBloqueadoparaLiberado,
                               imAlteracaoValorTitulo,
                               imAlteracaoDataPagamento,
                               imPagamentoDiretoFornecedor,
                               imManutencaoemCarteira,
                               imRetiradadeCarteira,
                               imEstornoDevolucaoCamaraCentralizadora,
                               imAlegacaoSacado,
                               imExclusaoRegistro]);
end;

function StrToInMovimento(var ok: boolean; const s: string): TInstrucaoMovimento;
begin
 result := StrToEnumerado(ok, s, ['00', '09', '10', '11', '17', '19',
                                  '23', '25', '27', '33', '40', '99'],
                                 [imInclusaoRegistroDetalheLiberado,
                                  imInclusaoRegistroDetalheBloqueado,
                                  imAltecacaoPagamentoLiberadoparaBloqueio,
                                  imAlteracaoPagamentoBloqueadoparaLiberado,
                                  imAlteracaoValorTitulo,
                                  imAlteracaoDataPagamento,
                                  imPagamentoDiretoFornecedor,
                                  imManutencaoemCarteira,
                                  imRetiradadeCarteira,
                                  imEstornoDevolucaoCamaraCentralizadora,
                                  imAlegacaoSacado,
                                  imExclusaoRegistro]);
end;

function TpMoedaToStr(const t: TTipoMoeda): String;
begin
  result := EnumeradoToStr(t, ['BTN', 'BRL', 'USD', 'PTE', 'FRF', 'CHF', 'JPY',
                               'IGP', 'IGM', 'GBP', 'ITL', 'DEM', 'TRD', 'UPC',
                               'UPF', 'UFR', 'XEU'],
                   [tmBonusTesouroNacional, tmReal, tmDolarAmericano,
                    tmEscudoPortugues, tmFrancoFrances, tmFrancoSuico,
                    tmIenJapones, tmIndiceGeralPrecos, tmIndiceGeralPrecosMercado,
                    tmLibraEsterlina, tmMarcoAlemao, tmTaxaReferencialDiaria,
                    tmUnidadePadraoCapital, tmUnidadePadraoFinanciamento,
                    tmUnidadeFiscalReferencia, tmUnidadeMonetariaEuropeia]);
end;

function TpIndTributoToStr(const t: TIndTributo): String;
begin
  result := EnumeradoToStr(t, ['00', '16', '18', '17', '21', '25', '26', '27'],
                          [itNenhum, itDANFNormal, itDARFSimples, itGPS, itDARJ,
                           itIPVA, itLicenciamento, itDPVAT]);
end;

function TpOperacaoToStr(const t: TTipoOperacao): String;
begin
  result := EnumeradoToStr(t, ['C', 'D', 'E', 'G', 'I', 'R', 'T'],
                            [toCredito, toDebito, toExtrato, toGestao,
                             toInformacao, toRemessa, toRetorno]);
end;

function StrToTpOperacao(var ok: boolean; const s:string): TTipoOperacao;
begin
  result := StrToEnumerado(ok, s,
                           ['C', 'D', 'E', 'G', 'I', 'R', 'T'],
                           [toCredito, toDebito, toExtrato, toGestao,
                            toInformacao, toRemessa, toRetorno]);
end;

function TpMovimentoPagtoToStr(const t: TTipoMovimentoPagto): String;
begin
  result := EnumeradoToStr(t, ['1', '2', '3', '4', '5', '6', '7', '9'],
                            [tmpCredito,
                             tmpDebito,
                             tmpAcumulado,
                             tmpRendimentoTributavelDeducaoIRRF,
                             tmpRendimentoIsentoNaoTributavel,
                             tmpRendimentoSujeitoTributacaoExclusiva,
                             tmpInformacaoComplementar,
                             tmpRendimentoRecebidoAcumuladamente]);
end;

function StrToTpMovimentoPagto(var ok:boolean; const s:string): TTipoMovimentoPagto;
begin
  result := StrToEnumerado(ok, s,
                               ['1', '2', '3', '4', '5', '6', '7', '9'],
                               [tmpCredito, tmpDebito, tmpAcumulado, tmpRendimentoTributavelDeducaoIRRF,
                                tmpRendimentoIsentoNaoTributavel, tmpRendimentoSujeitoTributacaoExclusiva,
                                tmpInformacaoComplementar, tmpRendimentoRecebidoAcumuladamente]);
end;

function CodigoPagamentoGpsToStr(const t: TCodigoPagamentoGps): String;
begin
  result := EnumeradoToStr(t, ['1007', '1104', '1406', '1457', '1503', '1554', '2003', '2100', '2208', '2631', '2909'],
                            [cpgContribuinteIndividualRecolhimentoMensal,     //NIT PIS PASEP
                             cpgContribuinteIndividualRecolhimentoTrimestral, //NIT PIS PASEP
                             cpgSecuradoFacultativoRecolhimentoMensal,        //NIT PIS PASEP
                             cpgSecuradoFacultativoRecolhimentoTrimestral,    //NIT PIS PASEP
                             cpgSecuradoEspecialRecolhimentoMensal,           //NIT PIS PASEP
                             cpgSecuradoEspecialRecolhimentoTrimestral,       //NIT PIS PASEP
                             cpgEmpresasOptantesSimplesCNPJ,
                             cpgEmpresasGeralCNPJ,
                             cpgEmpresasGeralCEI,
                             cpgContribuicaoRetidaSobreNfFaturaEmpresaPrestadoraServicoCNPJ,
                             ReclamatoriaTrabalhistaCNPJ]);
end;

function StrToCodigoPagamentoGps(var ok: boolean; const s: string): TCodigoPagamentoGps;
begin
  result := StrToEnumerado(ok, s, ['1007', '1104', '1406', '1457', '1503', '1554', '2003', '2100', '2208', '2631', '2909'],
                                  [cpgContribuinteIndividualRecolhimentoMensal,     //NIT PIS PASEP
                                   cpgContribuinteIndividualRecolhimentoTrimestral, //NIT PIS PASEP
                                   cpgSecuradoFacultativoRecolhimentoMensal,        //NIT PIS PASEP
                                   cpgSecuradoFacultativoRecolhimentoTrimestral,    //NIT PIS PASEP
                                   cpgSecuradoEspecialRecolhimentoMensal,           //NIT PIS PASEP
                                   cpgSecuradoEspecialRecolhimentoTrimestral,       //NIT PIS PASEP
                                   cpgEmpresasOptantesSimplesCNPJ,
                                   cpgEmpresasGeralCNPJ,
                                   cpgEmpresasGeralCEI,
                                   cpgContribuicaoRetidaSobreNfFaturaEmpresaPrestadoraServicoCNPJ,
                                   ReclamatoriaTrabalhistaCNPJ]);
end;

function TpTributoToStr(const t: TTipoTributo): String;
begin
  Result := EnumeradoToStr(t, ['01', '02', '03', '04', '05', '07', '08', '', '11'],
                            [ttGPS, ttDARFNormal, ttDARFSimples, ttDARJ, ttGareICMS,
                             ttIPVA, ttDPVAT, ttLicenciamento, ttFGTS])
end;

function DescricaoRetornoItau(const ADesc: string): string;
begin
  if ADesc = '00' then
    Result := 'PAGAMENTO EFETUADO'
  else if ADesc = 'AE' then
    Result := 'DATA DE PAGAMENTO ALTERADA'
  else if ADesc = 'AG' then
    Result := 'NÚMERO DO LOTE INVÁLIDO'
  else if ADesc = 'AH' then
    Result := 'NÚMERO SEQUENCIAL DO REGISTRO NO LOTE INVÁLIDO'
  else if ADesc = 'AI' then
    Result := 'PRODUTO DEMONSTRATIVO DE PAGAMENTO NÃO CONTRATADO'
  else if ADesc = 'AJ' then
    Result := 'TIPO DE MOVIMENTO INVÁLIDO'
  else if ADesc = 'AL' then
    Result := 'CÓDIGO DO BANCO FAVORECIDO INVÁLIDO'
  else if ADesc = 'AM' then
    Result := 'AGÊNCIA DO FAVORECIDO INVÁLIDA'
  else if ADesc = 'AN' then
    Result := 'CONTA CORRENTE DO FAVORECIDO INVÁLIDA / CONTA INVESTIMENTO EXTINTA EM 30/04/2011'
  else if ADesc = 'AO' then
    Result := 'NOME DO FAVORECIDO INVÁLIDO'
  else if ADesc = 'AP' then
    Result := 'DATA DE PAGAMENTO / DATA DE VALIDADE / HORA DE LANÇAMENTO / ARRECADAÇÃO / APURAÇÃO INVÁLIDA'
  else if ADesc = 'AQ' then
    Result := 'QUANTIDADE DE REGISTROS MAIOR QUE 999999'
  else if ADesc = 'AR' then
    Result := 'VALOR ARRECADADO / LANÇAMENTO INVÁLIDO'
  else if ADesc = 'BC' then
    Result := 'NOSSO NÚMERO INVÁLIDO'
  else if ADesc = 'BD' then
    Result := 'PAGAMENTO AGENDADO'
  else if ADesc = 'BDCI' then
    Result := 'PAGAMENTO ACATADO, PORÉM O CPF/CNPJ É INVÁLIDO.'
  else if ADesc = 'BDCD' then
    Result := 'PAGAMENTO ACATADO, PORÉM O CPF/CNPJ INFORMADO NÃO É O MESMO QUE ESTÁ CADASTRADO PARA A AGÊNCIA CONTA CREDITADA'
  else if ADesc = 'BDCN' then
    Result := 'PAGAMENTO ACATADO, PORÉM A AGÊNCIA/CONTA INFORMADA (AINDA) NÃO EXISTE'
  else if ADesc = 'BE' then
    Result := 'PAGAMENTO AGENDADO COM FORMA ALTEARADA PARA OP'
  else if ADesc = 'BI' then
    Result := 'CNPJ/CPF DO BENEFICIÁRIO INVÁLIDO NO SEGMENTO J-52 ou B INVÁLIDO'
  else if ADesc = 'BL' then
    Result := 'VALOR DA PARCELA INVÁLIDO'
  else if ADesc = 'CD' then
    Result := 'CNPJ / CPF INFORMADO DIVERGENTE DO CADASTRADO'
  else if ADesc = 'CE' then
    Result := 'PAGAMENTO CANCELADO'
  else if ADesc = 'CF' then
    Result := 'VALOR DO DOCUMENTO INVÁLIDO'
  else if ADesc = 'CG' then
    Result := 'VALOR DO ABATIMENTO INVÁLIDO'
  else if ADesc = 'CH' then
    Result := 'VALOR DO DESCONTO INVÁLIDO'
  else if ADesc = 'CI' then
    Result := 'CNPJ / CPF / IDENTIFICADOR / INSCRIÇÃO ESTADUAL / INSCRIÇÃO NO CAD / ICMS INVÁLIDO'
  else if ADesc = 'CJ' then
    Result := 'VALOR DA MULTA INVÁLIDO'
  else if ADesc = 'CK' then
    Result := 'TIPO DE INSCRIÇÃO INVÁLIDA'
  else if ADesc = 'CL' then
    Result := 'VALOR DO INSS INVÁLIDO'
  else if ADesc = 'CM' then
    Result := 'VALOR DO COFINS INVÁLIDO'
  else if ADesc = 'CN' then
    Result := 'CONTA NÃO CADASTRADA'
  else if ADesc = 'CO' then
    Result := 'VALOR DE OUTRAS ENTIDADES INVÁLIDO'
  else if ADesc = 'CP' then
    Result := 'CONFIRMAÇÃO DE OP CUMPRIDA'
  else if ADesc = 'CQ' then
    Result := 'SOMA DAS FATURAS DIFERE DO PAGAMENTO'
  else if ADesc = 'CR' then
    Result := 'VALOR DO CSLL INVÁLIDO'
  else if ADesc = 'CS' then
    Result := 'DATA DE VENCIMENTO DA FATURA INVÁLIDA'
  else if ADesc = 'DA' then
    Result := 'NÚMERO DE DEPEND. SALÁRIO FAMILIA INVALIDO'
  else if ADesc = 'DB' then
    Result := 'NÚMERO DE HORAS SEMANAIS INVÁLIDO'
  else if ADesc = 'DC' then
    Result := 'SALÁRIO DE CONTRIBUIÇÃO INSS INVÁLIDO'
  else if ADesc = 'DD' then
    Result := 'SALÁRIO DE CONTRIBUIÇÃO FGTS INVÁLIDO'
  else if ADesc = 'DE' then
    Result := 'VALOR TOTAL DOS PROVENTOS INVÁLIDO'
  else if ADesc = 'DF' then
    Result := 'VALOR TOTAL DOS DESCONTOS INVÁLIDO'
  else if ADesc = 'DG' then
    Result := 'VALOR LÍQUIDO NÃO NUMÉRICO'
  else if ADesc = 'DH' then
    Result := 'VALOR LIQ. INFORMADO DIFERE DO CALCULADO'
  else if ADesc = 'DI' then
    Result := 'VALOR DO SALÁRIO-BASE INVÁLIDO'
  else if ADesc = 'DJ' then
    Result := 'BASE DE CÁLCULO IRRF INVÁLIDA'
  else if ADesc = 'DK' then
    Result := 'BASE DE CÁLCULO FGTS INVÁLIDA'
  else if ADesc = 'DL' then
    Result := 'FORMA DE PAGAMENTO INCOMPATÍVEL COM HOLERITE'
  else if ADesc = 'DM' then
    Result := 'E-MAIL DO FAVORECIDO INVÁLIDO'
  else if ADesc = 'DV' then
    Result := 'DOC / TED DEVOLVIDO PELO BANCO FAVORECIDO'
  else if ADesc = 'D0' then
    Result := 'FINALIDADE DO HOLERITE INVÁLIDA'
  else if ADesc = 'D1' then
    Result := 'MÊS DE COMPETENCIA DO HOLERITE INVÁLIDA'
  else if ADesc = 'D2' then
    Result := 'DIA DA COMPETENCIA DO HOLETITE INVÁLIDA'
  else if ADesc = 'D3' then
    Result := 'CENTRO DE CUSTO INVÁLIDO'
  else if ADesc = 'D4' then
    Result := 'CAMPO NUMÉRICO DA FUNCIONAL INVÁLIDO'
  else if ADesc = 'D5' then
    Result := 'DATA INÍCIO DE FÉRIAS NÃO NUMÉRICA'
  else if ADesc = 'D6' then
    Result := 'DATA INÍCIO DE FÉRIAS INCONSISTENTE'
  else if ADesc = 'D7' then
    Result := 'DATA FIM DE FÉRIAS NÃO NUMÉRICO'
  else if ADesc = 'D8' then
    Result := 'DATA FIM DE FÉRIAS INCONSISTENTE'
  else if ADesc = 'D9' then
    Result := 'NÚMERO DE DEPENDENTES IR INVÁLIDO'
  else if ADesc = 'EM' then
    Result := 'CONFIRMAÇÃO DE OP EMITIDA'
  else if ADesc = 'EX' then
    Result := 'DEVOLUÇÃO DE OP NÃO SACADA PELO FAVORECIDO'
  else if ADesc = 'E0' then
    Result := 'TIPO DE MOVIMENTO HOLERITE INVÁLIDO'
  else if ADesc = 'E1' then
    Result := 'VALOR 01 DO HOLERITE / INFORME INVÁLIDO'
  else if ADesc = 'E2' then
    Result := 'VALOR 02 DO HOLERITE / INFORME INVÁLIDO'
  else if ADesc = 'E3' then
    Result := 'VALOR 03 DO HOLERITE / INFORME INVÁLIDO'
  else if ADesc = 'E4' then
    Result := 'VALOR 04 DO HOLERITE / INFORME INVÁLIDO'
  else if ADesc = 'FC' then
    Result := 'PAGAMENTO EFETUADO ATRAVÉS DE FINANCIAMENTO COMPROR'
  else if ADesc = 'FD' then
    Result := 'PAGAMENTO EFETUADO ATRAVÉS DE FINANCIAMENTO DESCOMPROR'
  else if ADesc = 'HA' then
    Result := 'ERRO NO HEADER DE ARQUIVO'
  else if ADesc = 'HM' then
    Result := 'ERRO NO HEADER DE LOTE'
  else if ADesc = 'IB' then
    Result := 'VALOR E/OU DATA DO DOCUMENTO INVÁLIDO'
  else if ADesc = 'IC' then
    Result := 'VALOR DO ABATIMENTO INVÁLIDO'
  else if ADesc = 'ID' then
    Result := 'VALOR DO DESCONTO INVÁLIDO'
  else if ADesc = 'IE' then
    Result := 'VALOR DA MORA INVÁLIDO'
  else if ADesc = 'IF' then
    Result := 'VALOR DA MULTA INVÁLIDO'
  else if ADesc = 'IG' then
    Result := 'VALOR DA DEDUÇÃO INVÁLIDO'
  else if ADesc = 'IH' then
    Result := 'VALOR DO ACRÉSCIMO INVÁLIDO'
  else if ADesc = 'II' then
    Result := 'DATA DE VENCIMENTO INVÁLIDA'
  else if ADesc = 'IJ' then
    Result := 'COMPETÊNCIA / PERÍODO REFERÊNCIA / PARCELA INVÁLIDA'
  else if ADesc = 'IK' then
    Result := 'TRIBUTO NÃO LIQUIDÁVEL VIA SISPAG OU NÃO CONVENIADO COM ITAÚ'
  else if ADesc = 'IL' then
    Result := 'CÓDIGO DE PAGAMENTO / EMPRESA /RECEITA INVÁLIDO'
  else if ADesc = 'IM' then
    Result := 'TIPO X FORMA NÃO COMPATÍVEL'
  else if ADesc = 'IN' then
    Result := 'BANCO/AGENCIA NÃO CADASTRADOS'
  else if ADesc = 'IO' then
    Result := 'DAC / VALOR / COMPETÊNCIA / IDENTIFICADOR DO LACRE INVÁLIDO'
  else if ADesc = 'IP' then
    Result := 'DAC DO CÓDIGO DE BARRAS INVÁLIDO'
  else if ADesc = 'IQ' then
    Result := 'DÍVIDA ATIVA OU NÚMERO DE ETIQUETA INVÁLIDO'
  else if ADesc = 'IR' then
    Result := 'PAGAMENTO ALTERADO'
  else if ADesc = 'IS' then
    Result := 'CONCESSIONÁRIA NÃO CONVENIADA COM ITAÚ'
  else if ADesc = 'IT' then
    Result := 'VALOR DO TRIBUTO INVÁLIDO'
  else if ADesc = 'IU' then
    Result := 'VALOR DA RECEITA BRUTA ACUMULADA INVÁLIDO'
  else if ADesc = 'IV' then
    Result := 'NÚMERO DO DOCUMENTO ORIGEM / REFERÊNCIA INVÁLIDO'
  else if ADesc = 'IX' then
    Result := 'CÓDIGO DO PRODUTO INVÁLIDO'
  else if ADesc = 'LA' then
    Result := 'DATA DE PAGAMENTO DE UM LOTE ALTERADA'
  else if ADesc = 'LC' then
    Result := 'LOTE DE PAGAMENTOS CANCELADO'
  else if ADesc = 'NA' then
    Result := 'PAGAMENTO CANCELADO POR FALTA DE AUTORIZAÇÃO'
  else if ADesc = 'NB' then
    Result := 'IDENTIFICAÇÃO DO TRIBUTO INVÁLIDA'
  else if ADesc = 'NC' then
    Result := 'EXERCÍCIO (ANO BASE) INVÁLIDO'
  else if ADesc = 'ND' then
    Result := 'CÓDIGO RENAVAM NÃO ENCONTRADO/INVÁLIDO'
  else if ADesc = 'NE' then
    Result := 'UF INVÁLIDA'
  else if ADesc = 'NF' then
    Result := 'CÓDIGO DO MUNICÍPIO INVÁLIDO'
  else if ADesc = 'NG' then
    Result := 'PLACA INVÁLIDA'
  else if ADesc = 'NH' then
    Result := 'OPÇÃO/PARCELA DE PAGAMENTO INVÁLIDA'
  else if ADesc = 'NI' then
    Result := 'TRIBUTO JÁ FOI PAGO OU ESTÁ VENCIDO'
  else if ADesc = 'NR' then
    Result := 'OPERAÇÃO NÃO REALIZADA'
  else if ADesc = 'PD' then
    Result := 'AQUISIÇÃO CONFIRMADA (EQUIVALE A OCORRÊNCIA 02 NO LAYOUT DE RISCO SACADO)'
  else if ADesc = 'RJ' then
    Result := 'REGISTRO REJEITADO'
  else if ADesc = 'RS' then
    Result := 'PAGAMENTO DISPONÍVEL PARA ANTECIPAÇÃO NO RISCO SACADO – MODALIDADE RISCO SACADO PÓS AUTORIZADO'
  else if ADesc = 'SS' then
    Result := 'PAGAMENTO CANCELADO POR INSUFICIÊNCIA DE SALDO/LIMITE DIÁRIO DE PAGTO'
  else if ADesc = 'TA' then
    Result := 'LOTE NÃO ACEITO - TOTAIS DO LOTE COM DIFERENÇA'
  else if ADesc = 'TI' then
    Result := 'TITULARIDADE INVÁLIDA'
  else if ADesc = 'X1' then
    Result := 'FORMA INCOMPATÍVEL COM LAYOUT 010'
  else if ADesc = 'X2' then
    Result := 'NÚMERO DA NOTA FISCAL INVÁLIDO'
  else if ADesc = 'X3' then
    Result := 'IDENTIFICADOR DE NF/CNPJ INVÁLIDO'
  else if ADesc = 'X4' then
    Result := 'FORMA 32 INVÁLIDA'
  else
    Result := 'RETORNO NÃO IDENTIFICADO'
end;

function DescricaoRetornoSantander(const ADesc: string): string;
begin
  if ADesc = '00' then
    Result := 'Crédito ou Débito Efetivado'
  else if ADesc = '01' then
    Result := 'Insuficiência de Fundos - Débito Não Efetuado'
  else if ADesc = '02' then
    Result := 'Crédito ou Débito Cancelado pelo Pagador/Credor'
  else if ADesc = '03' then
    Result := 'Débito Autorizado pela Agência - Efetuado'
  else if ADesc = 'AA' then
    Result := 'Controle Inválido'
  else if ADesc = 'AB' then
    Result := 'Tipo de Operação Inválido'
  else if ADesc = 'AC' then
    Result := 'Tipo de Serviço Inválido'
  else if ADesc = 'AD' then
    Result := 'Forma de Lançamento Inválida'
  else if ADesc = 'AE' then
    Result := 'Tipo/Número de Inscrição Inválido (gerado na crítica ou para informar rejeição)'
  else if ADesc = 'AF' then
    Result := 'Código de Convênio Inválido'
  else if ADesc = 'AG' then
    Result := 'Agência/Conta Corrente/DV Inválido'
  else if ADesc = 'AH' then
    Result := 'Número Seqüencial do Registro no Lote Inválido'
  else if ADesc = 'AI' then
    Result := 'Código de Segmento de Detalhe Inválido'
  else if ADesc = 'AJ' then
    Result := 'Tipo de Movimento Inválido'
  else if ADesc = 'AK' then
    Result := 'Código da Câmara de Compensação do Banco do Favorecido/Depositário Inválido'
  else if ADesc = 'AL' then
    Result := 'Código do Banco do Favorecido, Instituição de Pagamento ou Depositário Inválido'
  else if ADesc = 'AM' then
    Result := 'Agência Mantenedora da Conta Corrente do Favorecido Inválida'
  else if ADesc = 'AN' then
    Result := 'Conta Corrente/DV /Conta de Pagamento do Favorecido Inválido'
  else if ADesc = 'AO' then
    Result := 'Nome do Favorecido não Informado'
  else if ADesc = 'AP' then
    Result := 'Data Lançamento Inválida/Vencimento Inválido/Data de Pagamento não permitda.'
  else if ADesc = 'AQ' then
    Result := 'Tipo/Quantidade da Moeda Inválido'
  else if ADesc = 'AR' then
    Result := 'Valor do Lançamento Inválido/Divergente'
  else if ADesc = 'AS' then
    Result := 'Aviso ao Favorecido - Identificação Inválida'
  else if ADesc = 'AT' then
    Result := 'Tipo/Número de Inscrição do Favorecido/Contribuinte Inválido'
  else if ADesc = 'AU' then
    Result := 'Logradouro do Favorecido não Informado'
  else if ADesc = 'AV' then
    Result := 'Número do Local do Favorecido não Informado'
  else if ADesc = 'AW' then
    Result := 'Cidade do Favorecido não Informada'
  else if ADesc = 'AX' then
    Result := 'CEP/Complemento do Favorecido Inválido'
  else if ADesc = 'AY' then
    Result := 'Sigla do Estado do Favorecido Inválido'
  else if ADesc = 'AZ' then
    Result := 'Código/Nome do Banco Depositário Inválido'
  else if ADesc = 'BA' then
    Result := 'Código/Nome da Agência Depositário não Informado'
  else if ADesc = 'BB' then
    Result := 'Número do Documento Inválido(Seu Número)'
  else if ADesc = 'BC' then
    Result := 'Nosso Número Invalido'
  else if ADesc = 'BD' then
    Result := 'Inclusão Efetuada com Sucesso'
  else if ADesc = 'BE' then
    Result := 'Alteração Efetuada com Sucesso'
  else if ADesc = 'BF' then
    Result := 'Exclusão Efetuada com Sucesso'
  else if ADesc = 'BG' then
    Result := 'Agência/Conta Impedida Legalmente'
  else if ADesc = 'B1' then
    Result := 'Bloqueado Pendente de Autorização'
  else if ADesc = 'B3' then
    Result := 'Bloqueado pelo cliente'
  else if ADesc = 'B4' then
    Result := 'Bloqueado pela captura de titulo da cobrança'
  else if ADesc = 'B8' then
    Result := 'Bloqueado pela Validação de Tributos'
  else if ADesc = 'CA' then
    Result := 'Código de barras - Código do Banco Inválido'
  else if ADesc = 'CB' then
    Result := 'Código de barras - Código da Moeda Inválido'
  else if ADesc = 'CC' then
    Result := 'Código de barras - Dígito Verificador Geral Inválido'
  else if ADesc = 'CD' then
    Result := 'Código de barras - Valor do Título Inválido'
  else if ADesc = 'CE' then
    Result := 'Código de barras - Campo Livre Inválido'
  else if ADesc = 'CF' then
    Result := 'Valor do Documento/Principal/menor que o minimo Inválido'
  else if ADesc = 'CH' then
    Result := 'Valor do Desconto Inválido'
  else if ADesc = 'CI' then
    Result := 'Valor de Mora Inválido'
  else if ADesc = 'CJ' then
    Result := 'Valor da Multa Inválido'
  else if ADesc = 'CK' then
    Result := 'Valor do IR Inválido'
  else if ADesc = 'CL' then
    Result := 'Valor do ISS Inválido'
  else if ADesc = 'CG' then
    Result := 'Valor do Abatimento inválido'
  else if ADesc = 'CM' then
    Result := 'Valor do IOF Inválido'
  else if ADesc = 'CN' then
    Result := 'Valor de Outras Deduções Inválido'
  else if ADesc = 'CO' then
    Result := 'Valor de Outros Acréscimos Inválido'
  else if ADesc = 'HA' then
    Result := 'Lote Não Aceito'
  else if ADesc = 'HB' then
    Result := 'Inscrição da Empresa Inválida para o Contrato'
  else if ADesc = 'HC' then
    Result := 'Convênio com a Empresa Inexistente/Inválido para o Contrato'
  else if ADesc = 'HD' then
    Result := 'Agência/Conta Corrente da Empresa Inexistente/Inválida para o Contrato'
  else if ADesc = 'HE' then
    Result := 'Tipo de Serviço Inválido para o Contrato'
  else if ADesc = 'HF' then
    Result := 'Conta Corrente da Empresa com Saldo Insuficiente'
  else if ADesc = 'HG' then
    Result := 'Lote de Serviço fora de Seqüência'
  else if ADesc = 'HH' then
    Result := 'Lote de Serviço Inválido'
  else if ADesc = 'HI' then
    Result := 'Arquivo não aceito'
  else if ADesc = 'HJ' then
    Result := 'Tipo de Registro Inválido'
  else if ADesc = 'HL' then
    Result := 'Versão de Layout Inválida'
  else if ADesc = 'HU' then
    Result := 'Hora de Envio Inválida'
  else if ADesc = 'IA' then
    Result := 'Pagamento exclusivo em Cartório.'
  else if ADesc = 'IJ' then
    Result := 'Competência ou Período de Referencia ou Numero da Parcela invalido'
  else if ADesc = 'IL' then
    Result := 'Codigo Pagamento / Receita não numérico ou com zeros'
  else if ADesc = 'IM' then
    Result := 'Município Invalido'
  else if ADesc = 'IN' then
    Result := 'Numero Declaração Invalido'
  else if ADesc = 'IO' then
    Result := 'Numero Etiqueta invalido'
  else if ADesc = 'IP' then
    Result := 'Numero Notificação invalido'
  else if ADesc = 'IQ' then
    Result := 'Inscrição Estadual invalida'
  else if ADesc = 'IR' then
    Result := 'Divida Ativa Invalida'
  else if ADesc = 'IS' then
    Result := 'Valor Honorários ou Outros Acréscimos invalido'
  else if ADesc = 'IT' then
    Result := 'Período Apuração invalido'
  else if ADesc = 'IU' then
    Result := 'Valor ou Percentual da Receita invalido'
  else if ADesc = 'IV' then
    Result := 'Numero Referencia invalida'
  else if ADesc = 'SC' then
    Result := 'Validação parcial'
  else if ADesc = 'TA' then
    Result := 'Lote não Aceito - Totais do Lote com Diferença'
  else if ADesc = 'XB' then
    Result := 'Número de Inscrição do Contribuinte Inválido'
  else if ADesc = 'XC' then
    Result := 'Código do Pagamento ou Competência ou Número de Inscrição Inválido'
  else if ADesc = 'XF' then
    Result := 'Código do Pagamento ou Competência não Numérico ou igual á zeros'
  else if ADesc = 'YA' then
    Result := 'Título não Encontrado'
  else if ADesc = 'YB' then
    Result := 'Identificação Registro Opcional Inválido'
  else if ADesc = 'YC' then
    Result := 'Código Padrão Inválido'
  else if ADesc = 'YD' then
    Result := 'Código de Ocorrência Inválido'
  else if ADesc = 'YE' then
    Result := 'Complemento de Ocorrência Inválido'
  else if ADesc = 'YF' then
    Result := 'Alegação já Informada'
  else if ADesc = 'ZA' then
    Result := 'Transferencia Devolvida'
  else if ADesc = 'ZB' then
    Result := 'Transferencia mesma titularidade não permitida'
  else if ADesc = 'ZC' then
    Result := 'Código pagamento Tributo inválido'
  else if ADesc = 'ZD' then
    Result := 'Competência Inválida'
  else if ADesc = 'ZE' then
    Result := 'Título Bloqueado na base'
  else if ADesc = 'ZF' then
    Result := 'Sistema em Contingência – Titulo com valor maior que referência'
  else if ADesc = 'ZG' then
    Result := 'Sistema em Contingência – Título vencido'
  else if ADesc = 'ZH' then
    Result := 'Sistema em contingência - Título indexado'
  else if ADesc = 'ZI' then
    Result := 'Beneficiário divergente'
  else if ADesc = 'ZJ' then
    Result := 'Limite de pagamentos parciais excedido'
  else if ADesc = 'ZK' then
    Result := 'Título já liquidado'
  else if ADesc = 'ZT' then
    Result := 'Valor outras entidades inválido'
  else if ADesc = 'ZU' then
    Result := 'Sistema Origem Inválido'
  else if ADesc = 'ZW' then
    Result := 'Banco Destino não recebe DOC'
  else if ADesc = 'ZX' then
    Result := 'Banco Destino inoperante para DOC'
  else if ADesc = 'ZY' then
    Result := 'Código do Histórico de Credito Invalido'
  else if ADesc = 'ZV' then
    Result := 'Autorização iniciada no Internet Banking'
  else if ADesc = 'Z0' then
    Result := 'Conta com bloqueio'
  else if ADesc = 'Z1' then
    Result := 'Conta fechada. É necessário ativar a conta'
  else if ADesc = 'Z2' then
    Result := 'Conta com movimento controlado'
  else if ADesc = 'Z3' then
    Result := 'Conta cancelada'
  else if ADesc = 'Z4' then
    Result := 'Registro inconsistente (Título)'
  else
    Result := 'RETORNO NÃO IDENTIFICADO';
end;

function DescricaoRetornoBancoDoBrasil(const ADesc: string): string;
var
  vDesc: string;
  i: integer;
begin
  result := '';
  i := 0;
  repeat
    inc(i);
    vDesc := copy(ADesc + '   ', i * 2 - 1, 2);
    if VDesc = '00' then
      Result := Result + IfThen(i = 1, '', '/') + 'Este código indica que o pagamento foi confirmado'
    else if VDesc = '01' then
      Result := Result + IfThen(i = 1, '', '/') + 'Insuficiência de Fundos - Débito Não Efetuado'
    else if VDesc = '02' then
      Result := Result + IfThen(i = 1, '', '/') + 'Crédito ou Débito Cancelado pelo Pagador/Credor'
    else if VDesc = '03' then
      Result := Result + IfThen(i = 1, '', '/') + 'Débito Autorizado pela Agência - Efetuado'
    else if VDesc = 'AA' then
      Result := Result + IfThen(i = 1, '', '/') + 'Controle Inválido'
    else if VDesc = 'AB' then
      Result := Result + IfThen(i = 1, '', '/') + 'Tipo de Operação Inválido'
    else if VDesc = 'AC' then
      Result := Result + IfThen(i = 1, '', '/') + 'Tipo de Serviço Inválido'
    else if VDesc = 'AD' then
      Result := Result + IfThen(i = 1, '', '/') + 'Forma de Lançamento Inválida'
    else if VDesc = 'AE' then
      Result := Result + IfThen(i = 1, '', '/') + 'Tipo/Número de Inscrição Inválido'
    else if VDesc = 'AF' then
      Result := Result + IfThen(i = 1, '', '/') + 'Código de Convênio Inválido'
    else if VDesc = 'AG' then
      Result := Result + IfThen(i = 1, '', '/') + 'Agência/Conta Corrente/DV Inválido'
    else if VDesc = 'AH' then
      Result := Result + IfThen(i = 1, '', '/') + 'Nº Seqüencial do Registro no Lote Inválido'
    else if VDesc = 'AI' then
      Result := Result + IfThen(i = 1, '', '/') + 'Código de Segmento de Detalhe Inválido'
    else if VDesc = 'AJ' then
      Result := Result + IfThen(i = 1, '', '/') + 'Tipo de Movimento Inválido'
    else if VDesc = 'AK' then
      Result := Result + IfThen(i = 1, '', '/') + 'Código da Câmara de Compensação do Banco Favorecido/Depositário Inválido'
    else if VDesc = 'AL' then
      Result := Result + IfThen(i = 1, '', '/') + 'Código do Banco Favorecido ou Depositário Inválido'
    else if VDesc = 'AM' then
      Result := Result + IfThen(i = 1, '', '/') + 'Agência Mantenedora da Conta Corrente do Favorecido Inválida'
    else if VDesc = 'AN' then
      Result := Result + IfThen(i = 1, '', '/') + 'Conta Corrente/DV do Favorecido Inválido'
    else if VDesc = 'AO' then
      Result := Result + IfThen(i = 1, '', '/') + 'Nome do Favorecido Não Informado'
    else if VDesc = 'AP' then
      Result := Result + IfThen(i = 1, '', '/') + 'Data Lançamento Inválido'
    else if VDesc = 'AQ' then
      Result := Result + IfThen(i = 1, '', '/') + 'Tipo/Quantidade da Moeda Inválido'
    else if VDesc = 'AR' then
      Result := Result + IfThen(i = 1, '', '/') + 'Valor do Lançamento Inválido'
    else if VDesc = 'AS' then
      Result := Result + IfThen(i = 1, '', '/') + 'Aviso ao Favorecido - Identificação Inválida'
    else if VDesc = 'AT' then
      Result := Result + IfThen(i = 1, '', '/') + 'Tipo/Número de Inscrição do Favorecido Inválido'
    else if VDesc = 'AU' then
      Result := Result + IfThen(i = 1, '', '/') + 'Logradouro do Favorecido Não Informado'
    else if VDesc = 'AV' then
      Result := Result + IfThen(i = 1, '', '/') + 'Nº do Local do Favorecido Não Informado'
    else if VDesc = 'AW' then
      Result := Result + IfThen(i = 1, '', '/') + 'Cidade do Favorecido Não Informada'
    else if VDesc = 'AX' then
      Result := Result + IfThen(i = 1, '', '/') + 'CEP/Complemento do Favorecido Inválido'
    else if VDesc = 'AY' then
      Result := Result + IfThen(i = 1, '', '/') + 'Sigla do Estado do Favorecido Inválida'
    else if VDesc = 'AZ' then
      Result := Result + IfThen(i = 1, '', '/') + 'Código/Nome do Banco Depositário Inválido'
    else if VDesc = 'BA' then
      Result := Result + IfThen(i = 1, '', '/') + 'Código/Nome da Agência Depositária Não Informado'
    else if VDesc = 'BB' then
      Result := Result + IfThen(i = 1, '', '/') + 'Seu Número Inválido'
    else if VDesc = 'BC' then
      Result := Result + IfThen(i = 1, '', '/') + 'Nosso Número Inválido'
    else if VDesc = 'BD' then
      Result := Result + IfThen(i = 1, '', '/') + 'Inclusão Efetuada com Sucesso'
    else if VDesc = 'BE' then
      Result := Result + IfThen(i = 1, '', '/') + 'Alteração Efetuada com Sucesso'
    else if VDesc = 'BF' then
      Result := Result + IfThen(i = 1, '', '/') + 'Exclusão Efetuada com Sucesso'
    else if VDesc = 'BG' then
      Result := Result + IfThen(i = 1, '', '/') + 'Agência/Conta Impedida Legalmente'
    else if VDesc = 'BH' then
      Result := Result + IfThen(i = 1, '', '/') + 'Empresa não pagou salário'
    else if VDesc = 'BI' then
      Result := Result + IfThen(i = 1, '', '/') + 'Falecimento do mutuário'
    else if VDesc = 'BJ' then
      Result := Result + IfThen(i = 1, '', '/') + 'Empresa não enviou remessa do mutuário'
    else if VDesc = 'BK' then
      Result := Result + IfThen(i = 1, '', '/') + 'Empresa não enviou remessa no vencimento'
    else if VDesc = 'BL' then
      Result := Result + IfThen(i = 1, '', '/') + 'Valor da parcela inválida'
    else if VDesc = 'BM' then
      Result := Result + IfThen(i = 1, '', '/') + 'Identificação do contrato inválida'
    else if VDesc = 'BN' then
      Result := Result + IfThen(i = 1, '', '/') + 'Operação de Consignação Incluída com Sucesso'
    else if VDesc = 'BO' then
      Result := Result + IfThen(i = 1, '', '/') + 'Operação de Consignação Alterada com Sucesso'
    else if VDesc = 'BP' then
      Result := Result + IfThen(i = 1, '', '/') + 'Operação de Consignação Excluída com Sucesso'
    else if VDesc = 'BQ' then
      Result := Result + IfThen(i = 1, '', '/') + 'Operação de Consignação Liquidada com Sucesso'
    else if VDesc = 'BR' then
      Result := Result + IfThen(i = 1, '', '/') + 'Reativação Efetuada com Sucesso'
    else if VDesc = 'BS' then
      Result := Result + IfThen(i = 1, '', '/') + 'Suspensão Efetuada com Sucesso'
    else if VDesc = 'CA' then
      Result := Result + IfThen(i = 1, '', '/') + 'Código de Barras - Código do Banco Inválido'
    else if VDesc = 'CB' then
      Result := Result + IfThen(i = 1, '', '/') + 'Código de Barras - Código da Moeda Inválido'
    else if VDesc = 'CC' then
      Result := Result + IfThen(i = 1, '', '/') + 'Código de Barras - Dígito Verificador Geral Inválido'
    else if VDesc = 'CD' then
      Result := Result + IfThen(i = 1, '', '/') + 'Código de Barras - Valor do Título Inválido'
    else if VDesc = 'CE' then
      Result := Result + IfThen(i = 1, '', '/') + 'Código de Barras - Campo Livre Inválido'
    else if VDesc = 'CF' then
      Result := Result + IfThen(i = 1, '', '/') + 'Valor do Documento Inválido'
    else if VDesc = 'CG' then
      Result := Result + IfThen(i = 1, '', '/') + 'Valor do Abatimento Inválido'
    else if VDesc = 'CH' then
      Result := Result + IfThen(i = 1, '', '/') + 'Valor do Desconto Inválido'
    else if VDesc = 'CI' then
      Result := Result + IfThen(i = 1, '', '/') + 'Valor de Mora Inválido'
    else if VDesc = 'CJ' then
      Result := Result + IfThen(i = 1, '', '/') + 'Valor da Multa Inválido'
    else if VDesc = 'CK' then
      Result := Result + IfThen(i = 1, '', '/') + 'Valor do IR Inválido'
    else if VDesc = 'CL' then
      Result := Result + IfThen(i = 1, '', '/') + 'Valor do ISS Inválido'
    else if VDesc = 'CM' then
      Result := Result + IfThen(i = 1, '', '/') + 'Valor do IOF Inválido'
    else if VDesc = 'CN' then
      Result := Result + IfThen(i = 1, '', '/') + 'Valor de Outras Deduções Inválido'
    else if VDesc = 'CO' then
      Result := Result + IfThen(i = 1, '', '/') + 'Valor de Outros Acréscimos Inválido'
    else if VDesc = 'CP' then
      Result := Result + IfThen(i = 1, '', '/') + 'Valor do INSS Inválido'
    else if VDesc = 'HA' then
      Result := Result + IfThen(i = 1, '', '/') + 'Lote Não Aceito'
    else if VDesc = 'HB' then
      Result := Result + IfThen(i = 1, '', '/') + 'Inscrição da Empresa Inválida para o Contrato'
    else if VDesc = 'HC' then
      Result := Result + IfThen(i = 1, '', '/') + 'Convênio com a Empresa Inexistente/Inválido para o Contrato'
    else if VDesc = 'HD' then
      Result := Result + IfThen(i = 1, '', '/') + 'Agência/Conta Corrente da Empresa Inexistente/Inválido para o Contrato'
    else if VDesc = 'HE' then
      Result := Result + IfThen(i = 1, '', '/') + 'Tipo de Serviço Inválido para o Contrato'
    else if VDesc = 'HF' then
      Result := Result + IfThen(i = 1, '', '/') + 'Conta Corrente da Empresa com Saldo Insuficiente'
    else if VDesc = 'HG' then
      Result := Result + IfThen(i = 1, '', '/') + 'Lote de Serviço Fora de Seqüência'
    else if VDesc = 'HH' then
      Result := Result + IfThen(i = 1, '', '/') + 'Lote de Serviço Inválido'
    else if VDesc = 'HI' then
      Result := Result + IfThen(i = 1, '', '/') + 'Arquivo não aceito'
    else if VDesc = 'HJ' then
      Result := Result + IfThen(i = 1, '', '/') + 'Tipo de Registro Inválido'
    else if VDesc = 'HK' then
      Result := Result + IfThen(i = 1, '', '/') + 'Código Remessa / Retorno Inválido'
    else if VDesc = 'HL' then
      Result := Result + IfThen(i = 1, '', '/') + 'Versão de layout inválida'
    else if VDesc = 'HM' then
      Result := Result + IfThen(i = 1, '', '/') + 'Mutuário não identificado'
    else if VDesc = 'HN' then
      Result := Result + IfThen(i = 1, '', '/') + 'Tipo do beneficio não permite empréstimo'
    else if VDesc = 'HO' then
      Result := Result + IfThen(i = 1, '', '/') + 'Beneficio cessado/suspenso'
    else if VDesc = 'HP' then
      Result := Result + IfThen(i = 1, '', '/') + 'Beneficio possui representante legal'
    else if VDesc = 'HQ' then
      Result := Result + IfThen(i = 1, '', '/') + 'Beneficio é do tipo PA (Pensão alimentícia)'
    else if VDesc = 'HR' then
      Result := Result + IfThen(i = 1, '', '/') + 'Quantidade de contratos permitida excedida'
    else if VDesc = 'HS' then
      Result := Result + IfThen(i = 1, '', '/') + 'Beneficio não pertence ao Banco informado'
    else if VDesc = 'HT' then
      Result := Result + IfThen(i = 1, '', '/') + 'Início do desconto informado já ultrapassado'
    else if VDesc = 'HU' then
      Result := Result + IfThen(i = 1, '', '/') + 'Número da parcela inválida'
    else if VDesc = 'HV' then
      Result := Result + IfThen(i = 1, '', '/') + 'Quantidade de parcela inválida'
    else if VDesc = 'HW' then
      Result := Result + IfThen(i = 1, '', '/') + 'Margem consignável excedida para o mutuário dentro do prazo do contrato'
    else if VDesc = 'HX' then
      Result := Result + IfThen(i = 1, '', '/') + 'Empréstimo já cadastrado'
    else if VDesc = 'HY' then
      Result := Result + IfThen(i = 1, '', '/') + 'Empréstimo inexistente'
    else if VDesc = 'HZ' then
      Result := Result + IfThen(i = 1, '', '/') + 'Empréstimo já encerrado'
    else if VDesc = 'H1' then
      Result := Result + IfThen(i = 1, '', '/') + 'Arquivo sem trailer'
    else if VDesc = 'H2' then
      Result := Result + IfThen(i = 1, '', '/') + 'Mutuário sem crédito na competência'
    else if VDesc = 'H3' then
      Result := Result + IfThen(i = 1, '', '/') + 'Não descontado – outros motivos'
    else if VDesc = 'H4' then
      Result := Result + IfThen(i = 1, '', '/') + 'Retorno de Crédito não pago'
    else if VDesc = 'H5' then
      Result := Result + IfThen(i = 1, '', '/') + 'Cancelamento de empréstimo retroativo'
    else if VDesc = 'H6' then
      Result := Result + IfThen(i = 1, '', '/') + 'Outros Motivos de Glosa'
    else if VDesc = 'H7' then
      Result := Result + IfThen(i = 1, '', '/') + 'Margem consignável excedida para o mutuário acima do prazo do contrato'
    else if VDesc = 'H8' then
      Result := Result + IfThen(i = 1, '', '/') + 'Mutuário desligado do empregador'
    else if VDesc = 'H9' then
      Result := Result + IfThen(i = 1, '', '/') + 'Mutuário afastado por licença'
    else if VDesc = 'IA' then
      Result := Result + IfThen(i = 1, '', '/') + 'Primeiro nome do mutuário diferente do primeiro nome do movimento do censo ou diferente da base de Titular do Benefício'
    else if VDesc = 'IB' then
      Result := Result + IfThen(i = 1, '', '/') + 'Benefício suspenso/cessado pela APS ou Sisobi'
    else if VDesc = 'IC' then
      Result := Result + IfThen(i = 1, '', '/') + 'Benefício suspenso por dependência de cálculo'
    else if VDesc = 'ID' then
      Result := Result + IfThen(i = 1, '', '/') + 'Benefício suspenso/cessado pela inspetoria/auditoria'
    else if VDesc = 'IE' then
      Result := Result + IfThen(i = 1, '', '/') + 'Benefício bloqueado para empréstimo pelo beneficiário'
    else if VDesc = 'IF' then
      Result := Result + IfThen(i = 1, '', '/') + 'Benefício bloqueado para empréstimo por TBM'
    else if VDesc = 'IG' then
      Result := Result + IfThen(i = 1, '', '/') + 'Benefício está em fase de concessão de PA ou desdobramento'
    else if VDesc = 'IH' then
      Result := Result + IfThen(i = 1, '', '/') + 'Benefício cessado por óbito'
    else if VDesc = 'II' then
      Result := Result + IfThen(i = 1, '', '/') + 'Benefício cessado por fraude'
    else if VDesc = 'IJ' then
      Result := Result + IfThen(i = 1, '', '/') + 'Benefício cessado por concessão de outro benefício'
    else if VDesc = 'IK' then
      Result := Result + IfThen(i = 1, '', '/') + 'Benefício cessado: estatutário transferido para órgão de origem'
    else if VDesc = 'IL' then
      Result := Result + IfThen(i = 1, '', '/') + 'Empréstimo suspenso pela APS'
    else if VDesc = 'IM' then
      Result := Result + IfThen(i = 1, '', '/') + 'Empréstimo cancelado pelo banco'
    else if VDesc = 'IN' then
      Result := Result + IfThen(i = 1, '', '/') + 'Crédito transformado em PAB'
    else if VDesc = 'IO' then
      Result := Result + IfThen(i = 1, '', '/') + 'Término da consignação foi alterado'
    else if VDesc = 'IP' then
      Result := Result + IfThen(i = 1, '', '/') + 'Fim do empréstimo ocorreu durante período de suspensão ou concessão'
    else if VDesc = 'IQ' then
      Result := Result + IfThen(i = 1, '', '/') + 'Empréstimo suspenso pelo banco'
    else if VDesc = 'IR' then
      Result := Result + IfThen(i = 1, '', '/') + 'Não averbação de contrato – quantidade de parcelas/competências informadas ultrapassou a data limite da extinção de cota do dependente titular de benefícios'
    else if VDesc = 'TA' then
      Result := Result + IfThen(i = 1, '', '/') + 'Lote Não Aceito - Totais do Lote com Diferença'
    else if VDesc = 'YA' then
      Result := Result + IfThen(i = 1, '', '/') + 'Título Não Encontrado'
    else if VDesc = 'YB' then
      Result := Result + IfThen(i = 1, '', '/') + 'Identificador Registro Opcional Inválido'
    else if VDesc = 'YC' then
      Result := Result + IfThen(i = 1, '', '/') + 'Código Padrão Inválido'
    else if VDesc = 'YD' then
      Result := Result + IfThen(i = 1, '', '/') + 'Código de Ocorrência Inválido'
    else if VDesc = 'YE' then
      Result := Result + IfThen(i = 1, '', '/') + 'Complemento de Ocorrência Inválido'
    else if VDesc = 'YF' then
      Result := Result + IfThen(i = 1, '', '/') + 'Alegação já Informada'
    else if VDesc = 'ZA' then
      Result := Result + IfThen(i = 1, '', '/') + 'Agência / Conta do Favorecido Substituída Observação: As ocorrências iniciadas ''ZA'' tem caráter informativo para o cliente'
    else if VDesc = 'ZB' then
      Result := Result + IfThen(i = 1, '', '/') + 'Divergência entre o primeiro e último nome do beneficiário versus primeiro e último nome na Receita Federal'
    else if VDesc = 'ZC' then
      Result := Result + IfThen(i = 1, '', '/') + 'Confirmação de Antecipação de Valor'
    else if VDesc = 'ZD' then
      Result := Result + IfThen(i = 1, '', '/') + 'Antecipação parcial de valor'
    else if VDesc = 'ZE' then
      Result := Result + IfThen(i = 1, '', '/') + 'Título bloqueado na base'
    else if VDesc = 'ZF' then
      Result := Result + IfThen(i = 1, '', '/') + 'Sistema em contingência – título valor maior que referência'
    else if VDesc = 'ZG' then
      Result := Result + IfThen(i = 1, '', '/') + 'Sistema em contingência – título vencido'
    else if VDesc = 'ZH' then
      Result := Result + IfThen(i = 1, '', '/') + 'Sistema em contingência – título indexado'
    else if VDesc = 'ZI' then
      Result := Result + IfThen(i = 1, '', '/') + 'Beneficiário divergente'
    else if VDesc = 'ZJ' then
      Result := Result + IfThen(i = 1, '', '/') + 'Limite de pagamentos parciais excedido'
    else if VDesc = 'ZK' then
      Result := Result + IfThen(i = 1, '', '/') + 'Boleto já liquidado'
    else if Length(trim(VDesc)) <> 0 then
      Result := Result + IfThen(i = 1, '', '/') + 'RETORNO NÃO IDENTIFICADO';
  until Length(trim(VDesc)) = 0;
end;

end.

