{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit pcteConversaoCTe;

interface

uses
  SysUtils, StrUtils, Classes,
  pcnConversao;

type

  TLayOutCTe = (LayCTeRecepcao, LayCTeRetRecepcao, LayCTeCancelamento,
                LayCTeInutilizacao, LayCTeConsulta, LayCTeStatusServico,
                LayCTeCadastro, LayCTeEvento, LayCTeEventoAN,
                LayCTeDistDFeInt, LayCTeRecepcaoOS, LayCTeRecepcaoSinc,
                LayCTeRecepcaoGTVe);

  TSchemaCTe = ( schErro, schCTe, schCTeOS, schGTVe, schcancCTe, schInutCTe,
                 schEventoCTe,
           //      schresCTe, schresEvento, schprocCTe, schprocEventoCTe,
                 schconsReciCTe, schconsSitCTe, schconsStatServCTe, schconsCad,
                 schcteModalAereo, schcteModalAquaviario, schcteModalDutoviario,
                 schcteModalFerroviario, schcteModalRodoviario, schcteMultiModal,
                 schevEPECCTe, schevCancCTe, schevRegMultimodal, schevCCeCTe,
                 schdistDFeInt, schcteModalRodoviarioOS, schevPrestDesacordo,
                 schevGTV, schevCECTe, schevCancCECTe, schevCancPrestDesacordo,
                 schevIECTe, schevCancIECTe);

  TStatusACBrCTe = (stCTeIdle, stCTeStatusServico, stCTeRecepcao, stCTeRetRecepcao,
                    stCTeConsulta, stCTeCancelamento, stCTeInutilizacao,
                    stCTeRecibo, stCTeCadastro, stCTeEmail, stCTeCCe,
                    stCTeEvento, stCTeDistDFeInt, stCTeEnvioWebService);

  TModeloCTe = (moCTe, moCTeOS, moGTVe);
  TVersaoCTe = (ve200, ve300, ve400);

  TpcteFormaPagamento = (fpPago, fpAPagar, fpOutros);
  TpcteTipoCTe = (tcNormal, tcComplemento, tcAnulacao, tcSubstituto, tcGTVe);
  TpcteTipoServico = (tsNormal, tsSubcontratacao, tsRedespacho, tsIntermediario,
                      tsMultimodal, tsTranspPessoas, tsTranspValores,
                      tsExcessoBagagem, tsGTV);
  TpcteRetira = (rtSim, rtNao);
  TpcteTomador = ( tmRemetente, tmExpedidor, tmRecebedor, tmDestinatario, tmOutros);
  TpcteLotacao = (ltNao, ltSim);
  TpcteDirecao = (drNorte, drLeste, drSul, drOeste);
  TpcteTipoTrafego = (ttProprio, ttMutuo, ttRodoferroviario, ttRodoviario);
  TpcteTipoDataPeriodo = (tdSemData, tdNaData, tdAteData, tdApartirData, tdNoPeriodo, tdNaoInformado);
  TpcteTipoHorarioIntervalo = (thSemHorario, thNoHorario, thAteHorario, thApartirHorario, thNoIntervalo, thNaoInformado);
  TpcteTipoDocumento = (tdDeclaracao, tdDutoviario, tdCFeSAT, tdNFCe, tdOutros);
  TpcteTipoDocumentoAnterior = (daCTRC, daCTAC, daACT, daNF7, daNF27, daCAN,
                                daCTMC, daATRE, daDTA, daCAI, daCCPI, daCA,
                                daTIF, daBL, daOutros);
  TpcteRspPagPedagio = (rpEmitente, rpRemetente, rpExpedidor, rpRecebedor, rpDestinatario, rpTomadorServico);
  TpcteTipoDispositivo = (tdCartaoMagnetico, tdTAG, tdTicket);
  TpcteTipoPropriedade = (tpProprio, tpTerceiro);
  TpcteTrafegoMutuo = (tmOrigem, tmDestino);
  TpcnindNegociavel = (inNaoNegociavel, inNegociavel);
  TpcteTipoVeiculo = (tvTracao, tvReboque);
  TpcteRspSeg = (rsRemetente, rsExpedidor, rsRecebedor, rsDestinatario, rsEmitenteCTe, rsTomadorServico);
  TEspecie = (teNumerario, teCheque, teMoeda, teOutros);
  TpInfManu = (imCEEAV, imAPCDEA, imSAC, imAPDENR, imAPQI, imGSR, imNR, imAPCC,
               imAAGA, imPI965, imPI966, imPI967, imPI968, imPI969, imPI970, imOUTRO);
  TpUniMed = (umKG, umKGG, umLitros, umTI, umUnidades);
  TtpFretamento = (tfNenhum, tfEventual, tpContinuo);

  TtpComp = (tcCustodia, tcEmbarque, tcTempodeespera, tcMalote, tcAdValorem, tcOutros);

  TtpNumerario = (tnNacional, tnEstrangeiro);

  TCRT = (crtNenhum, crtSimplesNacional, crtSimplesExcessoReceita, crtRegimeNormal,
          crtSimplesNacionalMEI);

  TtpMotivo = (tmNaoEncontrado, tmRecusa, tmInexistente, tmOutro);

function LayOutToServico(const t: TLayOutCTe): String;
function ServicoToLayOut(out ok: Boolean; const s: String): TLayOutCTe;

function LayOutToSchema(const t: TLayOutCTe): TSchemaCTe;

function SchemaCTeToStr(const t: TSchemaCTe): String;
function StrToSchemaCTe(out ok: Boolean; const s: String): TSchemaCTe;

function StrToVersaoCTe(out ok: Boolean; const s: String): TVersaoCTe;
function VersaoCTeToStr(const t: TVersaoCTe): String;

function DblToVersaoCTe(out ok: Boolean; const d: Double): TVersaoCTe;
function VersaoCTeToDbl(const t: TVersaoCTe): Double;

function tpforPagToStr(const t: TpcteFormaPagamento): string;
function tpforPagToStrText(const t: TpcteFormaPagamento): string;
function StrTotpforPag(out ok: boolean; const s: string): TpcteFormaPagamento;

function tpCTePagToStr(const t: TpcteTipoCTe): string;
function tpCTToStr(const t: TpcteTipoCTe): string;
function tpCTToStrText(const t: TpcteTipoCTe): string;
function StrTotpCTe(out ok: boolean; const s: string): TpcteTipoCTe;

function TpServPagToStr(const t: TpcteTipoServico): string;
function TpServToStrText(const t: TpcteTipoServico): string;
function StrToTpServ(out ok: boolean; const s: string): TpcteTipoServico;

function TpRetiraPagToStr(const t: TpcteRetira): string;
function StrToTpRetira(out ok: boolean; const s: string): TpcteRetira;

function TpTomadorPagToStr(const t: TpcteTomador): string;
function TpTomadorToStr(const t: TpcteTomador): String;
function TpTomadorToStrText(const t: TpcteTomador): String;
function StrToTpTomador(out ok: boolean; const s: String ): TpcteTomador;

function TpLotacaoToStr(const t: TpcteLotacao): string;
function StrToTpLotacao(out ok: boolean; const s: String ): TpcteLotacao;

function TpDirecaoToStr(const t: TpcteDirecao): string;
function StrToTpDirecao(out ok: boolean; const s: string): TpcteDirecao;

function TpTrafegoToStr(const t: TpcteTipoTrafego): string;
function StrToTpTrafego(out ok: boolean; const s: string): TpcteTipoTrafego;

function TpDataPeriodoToStr(const t: TpcteTipoDataPeriodo): string;
function StrToTpDataPeriodo(out ok: boolean; const s: string): TpcteTipoDataPeriodo;

function TpHorarioIntervaloToStr(const t: TpcteTipoHorarioIntervalo): string;
function StrToTpHorarioIntervalo(out ok: boolean; const s: string): TpcteTipoHorarioIntervalo;

function TpDocumentoToStr(const t: TpcteTipoDocumento): string;
function StrToTpDocumento(out ok: boolean; const s: string): TpcteTipoDocumento;

function TpDocumentoAnteriorToStr(const t: TpcteTipoDocumentoAnterior): string;
function StrToTpDocumentoAnterior(out ok: boolean; const s: string): TpcteTipoDocumentoAnterior;

function RspPagPedagioToStr(const t: TpcteRspPagPedagio): string;
function StrToRspPagPedagio(out ok: boolean; const s: string): TpcteRspPagPedagio;

function TpDispositivoToStr(const t: TpcteTipoDispositivo): string;
function StrToTpDispositivo(out ok: boolean; const s: string): TpcteTipoDispositivo;

function TpPropriedadeToStr(const t: TpcteTipoPropriedade): string;
function StrToTpPropriedade(out ok: boolean; const s: string): TpcteTipoPropriedade;

function TrafegoMutuoToStr(const t: TpcteTrafegoMutuo): string;
function StrToTrafegoMutuo(out ok: boolean; const s: string): TpcteTrafegoMutuo;

function indNegociavelToStr(const t: TpcnindNegociavel ): string;
function StrToindNegociavel(out ok: boolean; const s: string): TpcnindNegociavel;

function TpVeiculoToStr(const t: TpcteTipoVeiculo): string;
function StrToTpVeiculo(out ok: boolean; const s: string): TpcteTipoVeiculo;

function GetVersaoModalCTe(AVersaoDF: TVersaoCTe; AModal: TpcteModal): string;

function ModeloCTeToStr(const t: TModeloCTe): String;
function StrToModeloCTe(out ok: Boolean; const s: String): TModeloCTe;
function ModeloCTeToPrefixo(const t: TModeloCTe): String;

function TEspecieToStr(const t: TEspecie): String;
function StrToTEspecie(out ok: Boolean; const s: String): TEspecie;

function TpRspSeguroToStr(const t: TpcteRspSeg): String;
function TpRspSeguroToStrText(const t: TpcteRspSeg): String;
function StrToTpRspSeguro(out ok: boolean; const s: String ): TpcteRspSeg;

function TpInfManuToStr(const t: TpInfManu): string;
function StrToTpInfManu(out ok: boolean; const s: string): TpInfManu;

function TpInfManuToStrV2(const t: TpInfManu): string;
function StrToTpInfManuV2(out ok: boolean; const s: string): TpInfManu;

function UniMedToStr(const t: TpUniMed): String;
function StrToUniMed(out ok: Boolean; const s: String): TpUniMed;

function TpFretamentoToStr(const t: TtpFretamento): String;
function StrToTpFretamento(out ok: Boolean; const s: String): TtpFretamento;

function tpCompToStr(const t: TtpComp): String;
function StrTotpComp(out ok: Boolean; const s: String): TtpComp;

function tpNumerarioToStr(const t: TtpNumerario): String;
function StrTotpNumerario(out ok: Boolean; const s: String): TtpNumerario;

function CRTCTeToStr(const t: TCRT): string;
function StrToCRTCTe(out ok: boolean; const s: string): TCRT;

function tpMotivoToStr(const t: TtpMotivo): string;
function StrTotpMotivo(out ok: boolean; const s: string): TtpMotivo;

function StrToTpEventoCTe(out ok: boolean; const s: string): TpcnTpEvento;

implementation

uses
  typinfo;

function LayOutToServico(const t: TLayOutCTe): String;
begin
  Result := EnumeradoToStr(t,
    ['CTeRecepcao', 'CTeRetRecepcao', 'CTeCancelamento',
     'CTeInutilizacao', 'CTeConsultaProtocolo', 'CTeStatusServico',
     'CTeConsultaCadastro', 'RecepcaoEvento', 'RecepcaoEventoAN',
     'CTeDistribuicaoDFe', 'CTeRecepcaoOS', 'CTeRecepcaoSinc',
      'CTeRecepcaoGTVe'],
    [ LayCTeRecepcao, LayCTeRetRecepcao, LayCTeCancelamento,
      LayCTeInutilizacao, LayCTeConsulta, LayCTeStatusServico,
      LayCTeCadastro, LayCTeEvento, LayCTeEventoAN,
      LayCTeDistDFeInt, LayCTeRecepcaoOS, LayCTeRecepcaoSinc,
      LayCTeRecepcaoGTVe ]);
end;

function ServicoToLayOut(out ok: Boolean; const s: String): TLayOutCTe;
begin
  Result := StrToEnumerado(ok, s,
    ['CTeRecepcao', 'CTeRetRecepcao', 'CTeCancelamento',
     'CTeInutilizacao', 'CTeConsultaProtocolo', 'CTeStatusServico',
     'CTeConsultaCadastro', 'RecepcaoEvento', 'RecepcaoEventoAN',
     'CTeDistribuicaoDFe', 'CTeRecepcaoOS', 'CTeRecepcaoSinc',
     'CTeRecepcaoGTVe'],
    [ LayCTeRecepcao, LayCTeRetRecepcao, LayCTeCancelamento,
      LayCTeInutilizacao, LayCTeConsulta, LayCTeStatusServico,
      LayCTeCadastro, LayCTeEvento, LayCTeEventoAN,
      LayCTeDistDFeInt, LayCTeRecepcaoOS, LayCTeRecepcaoSinc,
      LayCTeRecepcaoGTVe ]);
end;

function LayOutToSchema(const t: TLayOutCTe): TSchemaCTe;
begin
  case t of
    LayCTeRecepcao,
    LayCTeRecepcaoSinc:  Result := schCTe;
    LayCTeRecepcaoOS:    Result := schCTeOS;

    LayCTeRecepcaoGTVe:  Result := schGTVe;

    LayCTeRetRecepcao:   Result := schconsReciCTe;
    LayCTeCancelamento:  Result := schcancCTe;
    LayCTeInutilizacao:  Result := schInutCTe;
    LayCTeConsulta:      Result := schconsSitCTe;
    LayCTeStatusServico: Result := schconsStatServCTe;
    LayCTeCadastro:      Result := schconsCad;
    LayCTeEvento,
    LayCTeEventoAN:      Result := schEventoCTe;
    LayCTeDistDFeInt:    Result := schdistDFeInt;
  else
    Result := schErro;
  end;
end;

function SchemaCTeToStr(const t: TSchemaCTe): String;
begin
  Result := GetEnumName(TypeInfo(TSchemaCTe), Integer( t ) );
  Result := copy(Result, 4, Length(Result)); // Remove prefixo "sch"
end;

function StrToSchemaCTe(out ok: Boolean; const s: String): TSchemaCTe;
var
  P: Integer;
  SchemaStr: String;
begin
  P := pos('_', s);
  if P > 0 then
    SchemaStr := copy(s, 1, P-1)
  else
    SchemaStr := s;

  if LeftStr(SchemaStr, 3) <> 'sch' then
    SchemaStr := 'sch' + SchemaStr;

  Result := TSchemaCTe( GetEnumValue(TypeInfo(TSchemaCTe), SchemaStr ) );
end;

function StrToVersaoCTe(out ok: Boolean; const s: String): TVersaoCTe;
begin
  Result := StrToEnumerado(ok, s, ['2.00', '3.00', '4.00'],
                                  [ve200, ve300, ve400]);
end;

function VersaoCTeToStr(const t: TVersaoCTe): String;
begin
  Result := EnumeradoToStr(t, ['2.00', '3.00', '4.00'], [ve200, ve300, ve400]);
end;

function DblToVersaoCTe(out ok: Boolean; const d: Double): TVersaoCTe;
begin
  ok := True;

  if d = 2.0 then
    Result := ve200
  else
  if d = 3.0 then
    Result := ve300
  else
  if d = 4.0 then
    Result := ve400
  else
  begin
    Result := ve200;
    ok := False;
  end;
end;

function VersaoCTeToDbl(const t: TVersaoCTe): Double;
begin
  case t of
    ve200: Result := 2.0;
    ve300: Result := 3.0;
    ve400: Result := 4.0;
  else
    Result := 0;
  end;
end;

function tpCTToStr(const t: TpcteTipoCTe): string;
begin
  result := EnumeradoToStr(t, ['0', '1', '2', '3', '4'],
                   [tcNormal, tcComplemento, tcAnulacao, tcSubstituto, tcGTVe]);
end;

function tpCTToStrText(const t: TpcteTipoCTe): string;
begin
  result := EnumeradoToStr(t, ['NORMAL', 'COMPLEMENTO', 'ANULAÇÃO', 'SUBSTITUTO',
                               'GTVe'],
                   [tcNormal, tcComplemento, tcAnulacao, tcSubstituto, tcGTVe]);
end;

function tpforPagToStr(const t: TpcteFormaPagamento): string;
begin
  result := EnumeradoToStr(t, ['0','1', '2'],
                              [fpPago, fpAPagar, fpOutros]);
end;

function tpforPagToStrText(const t: TpcteFormaPagamento): string;
begin
  result := EnumeradoToStr(t, ['PAGO','A PAGAR', 'OUTROS'],
                              [fpPago, fpAPagar, fpOutros]);
end;

function StrTotpforPag(out ok: boolean; const s: string): TpcteFormaPagamento;
begin
  result := StrToEnumerado(ok, s, ['0', '1', '2'],
                                  [fpPago, fpAPagar, fpOutros]);
end;

function tpCTePagToStr(const t: TpcteTipoCTe): string;
begin
  result := EnumeradoToStr(t, ['0','1', '2', '3', '4'],
                   [tcNormal, tcComplemento, tcAnulacao, tcSubstituto, tcGTVe]);
end;

function StrTotpCTe(out ok: boolean; const s: string): TpcteTipoCTe;
begin
  result := StrToEnumerado(ok, s, ['0', '1', '2', '3', '4'],
                   [tcNormal, tcComplemento, tcAnulacao, tcSubstituto, tcGTVe]);
end;

function TpServPagToStr(const t: TpcteTipoServico): string;
begin
  result := EnumeradoToStr(t, ['0','1', '2', '3', '4', '6', '7', '8', '9'],
                              [tsNormal, tsSubcontratacao, tsRedespacho,
                               tsIntermediario, tsMultimodal, tsTranspPessoas,
                               tsTranspValores, tsExcessoBagagem, tsGTV]);
end;

function TpServToStrText(const t: TpcteTipoServico): string;
begin
  result := EnumeradoToStr(t, ['NORMAL','SUBCONTRATAÇÃO', 'REDESPACHO',
                               'REDESP. INTERMEDIÁRIO', 'VINC. A MULTIMODAL',
                               'TRANSP. PESSOAS', 'TRANSP. VALORES',
                               'EXCESSO BAGAGEM', 'GTV'],
                              [tsNormal, tsSubcontratacao, tsRedespacho,
                               tsIntermediario, tsMultimodal,
                               tsTranspPessoas, tsTranspValores,
                               tsExcessoBagagem, tsGTV]);
end;

function StrToTpServ(out ok: boolean; const s: string): TpcteTipoServico;
begin
  result := StrToEnumerado(ok, s, ['0', '1', '2', '3', '4', '6', '7', '8', '9'],
                                  [tsNormal, tsSubcontratacao, tsRedespacho,
                                   tsIntermediario, tsMultimodal,
                                   tsTranspPessoas, tsTranspValores,
                                   tsExcessoBagagem, tsGTV]);
end;

function TpRetiraPagToStr(const t: TpcteRetira): string;
begin
  result := EnumeradoToStr(t, ['0','1'], [rtSim, rtNao]);
end;

function StrToTpRetira(out ok: boolean; const s: string): TpcteRetira;
begin
  result := StrToEnumerado(ok, s, ['0', '1'], [rtSim, rtNao]);
end;

function TpTomadorPagToStr(const t: TpcteTomador): string;
begin
  result := EnumeradoToStr(t, ['0','1', '2', '3', '4'],
             [tmRemetente, tmExpedidor, tmRecebedor, tmDestinatario, tmOutros]);
end;

function TpTomadorToStr(const t: TpcteTomador): String;
begin
  result := EnumeradoToStr(t, ['0', '1', '2', '3', '4'],
             [tmRemetente, tmExpedidor, tmRecebedor, tmDestinatario, tmOutros]);
end;

function TpTomadorToStrText(const t: TpcteTomador): String;
begin
  result := EnumeradoToStr(t, ['REMETENTE', 'EXPEDIDOR', 'RECEBEDOR',
                               'DESTINATARIO', 'OUTROS'],
             [tmRemetente, tmExpedidor, tmRecebedor, tmDestinatario, tmOutros]);
end;

function TpLotacaoToStr(const t: TpcteLotacao): string;
begin
  result := EnumeradoToStr(t, ['0','1'], [ltNao, ltSim]);
end;

function StrToTpTomador(out ok: boolean; const s: String ): TpcteTomador;
begin
  result := StrToEnumerado(ok, s, ['0', '1', '2', '3', '4'],
             [tmRemetente, tmExpedidor, tmRecebedor, tmDestinatario, tmOutros]);
end;

function StrToTpLotacao(out ok: boolean; const s: String ): TpcteLotacao;
begin
  result := StrToEnumerado(ok, s, ['0', '1'], [ltNao, ltSim]);
end;

function TpDirecaoToStr(const t: TpcteDirecao): string;
begin
  result := EnumeradoToStr(t, ['N','L','S','O'],
                              [drNorte , drLeste, drSul, drOeste]);
end;

function StrToTpDirecao(out ok: boolean; const s: string): TpcteDirecao;
begin
  result := StrToEnumerado(ok, s, ['N','L','S','O'],
                                  [drNorte , drLeste, drSul, drOeste]);
end;

function TpTrafegoToStr(const t: TpcteTipoTrafego): string;
begin
  result := EnumeradoToStr(t, ['0','1','2','3'],
                        [ttProprio , ttMutuo, ttRodoferroviario, ttRodoviario]);
end;

function StrToTpTrafego(out ok: boolean; const s: string): TpcteTipoTrafego;
begin
  result := StrToEnumerado(ok, s, ['0','1','2','3'],
                        [ttProprio , ttMutuo, ttRodoferroviario, ttRodoviario]);
end;

function TpDataPeriodoToStr(const t: TpcteTipoDataPeriodo): string;
begin
  result := EnumeradoToStr(t, ['0','1','2','3','4','N'],
                              [tdSemData, tdNaData, tdAteData, tdApartirData,
                               tdNoPeriodo, tdNaoInformado]);
end;

function StrToTpDataPeriodo(out ok: boolean; const s: string): TpcteTipoDataPeriodo;
begin
  result := StrToEnumerado(ok, s, ['0','1','2','3','4','N'],
                                  [tdSemData, tdNaData, tdAteData, tdApartirData,
                                   tdNoPeriodo, tdNaoInformado]);
end;

function TpHorarioIntervaloToStr(const t: TpcteTipoHorarioIntervalo): string;
begin
  result := EnumeradoToStr(t, ['0','1','2','3','4','N'],
                              [thSemHorario, thNoHorario, thAteHorario,
                               thApartirHorario, thNoIntervalo, thNaoInformado]);
end;

function StrToTpHorarioIntervalo(out ok: boolean; const s: string): TpcteTipoHorarioIntervalo;
begin
  result := StrToEnumerado(ok, s, ['0','1','2','3','4','N'],
                                  [thSemHorario, thNoHorario, thAteHorario,
                                   thApartirHorario, thNoIntervalo, thNaoInformado]);
end;

function TpDocumentoToStr(const t: TpcteTipoDocumento): string;
begin
  result := EnumeradoToStr(t, ['00', '10', '59', '65', '99'],
                              [tdDeclaracao, tdDutoviario, tdCFeSAT, tdNFCe, tdOutros]);
end;

function StrToTpDocumento(out ok: boolean; const s: string): TpcteTipoDocumento;
begin
  result := StrToEnumerado(ok, s, ['00', '10', '59', '65', '99'],
                                  [tdDeclaracao, tdDutoviario, tdCFeSAT, tdNFCe, tdOutros]);
end;

function TpDocumentoAnteriorToStr(const t: TpcteTipoDocumentoAnterior): string;
begin
  result := EnumeradoToStr(t, ['00', '01', '02', '03', '04', '05', '06', '07',
                               '08', '09', '10', '11', '12', '13', '99'],
                         [daCTRC, daCTAC, daACT, daNF7, daNF27, daCAN, daCTMC,
                          daATRE, daDTA, daCAI, daCCPI, daCA, daTIF, daBL, daOutros]);
end;

function StrToTpDocumentoAnterior(out ok: boolean; const s: string): TpcteTipoDocumentoAnterior;
begin
  result := StrToEnumerado(ok, s, ['00', '01', '02', '03', '04', '05', '06', '07',
                                   '08', '09', '10', '11', '12', '13', '99'],
                         [daCTRC, daCTAC, daACT, daNF7, daNF27, daCAN, daCTMC,
                          daATRE, daDTA, daCAI, daCCPI, daCA, daTIF, daBL, daOutros]);
end;

function RspPagPedagioToStr(const t: TpcteRspPagPedagio): string;
begin
  result := EnumeradoToStr(t, ['0','1','2','3','4','5'],
                             [rpEmitente, rpRemetente, rpExpedidor, rpRecebedor,
                              rpDestinatario, rpTomadorServico]);
end;

function StrToRspPagPedagio(out ok: boolean; const s: string): TpcteRspPagPedagio;
begin
  result := StrToEnumerado(ok, s, ['0','1','2','3','4','5'],
                             [rpEmitente, rpRemetente, rpExpedidor, rpRecebedor,
                              rpDestinatario, rpTomadorServico]);
end;

function TpDispositivoToStr(const t: TpcteTipoDispositivo): string;
begin
  result := EnumeradoToStr(t, ['1','2','3'], [tdCartaoMagnetico, tdTAG, tdTicket]);
end;

function StrToTpDispositivo(out ok: boolean; const s: string): TpcteTipoDispositivo;
begin
  result := StrToEnumerado(ok, s, ['1','2','3'], [tdCartaoMagnetico, tdTAG, tdTicket]);
end;

function TpPropriedadeToStr(const t: TpcteTipoPropriedade): string;
begin
  result := EnumeradoToStr(t, ['P','T'], [tpProprio, tpTerceiro]);
end;

function StrToTpPropriedade(out ok: boolean; const s: string): TpcteTipoPropriedade;
begin
  result := StrToEnumerado(ok, s, ['P','T'], [tpProprio, tpTerceiro]);
end;

function TrafegoMutuoToStr(const t: TpcteTrafegoMutuo): string;
begin
  result := EnumeradoToStr(t, ['1','2'],
   [tmOrigem, tmDestino]);
end;

function StrToTrafegoMutuo(out ok: boolean; const s: string): TpcteTrafegoMutuo;
begin
  result := StrToEnumerado(ok, s, ['1','2'],
   [tmOrigem, tmDestino]);
end;

function indNegociavelToStr(const t: TpcnindNegociavel ): string;
begin
  result := EnumeradoToStr(t, ['0', '1'], [inNaoNegociavel, inNegociavel]);
end;

function StrToindNegociavel(out ok: boolean; const s: string): TpcnindNegociavel;
begin
  result := StrToEnumerado(ok, s, ['0', '1'], [inNaoNegociavel, inNegociavel]);
end;

function TpVeiculoToStr(const t: TpcteTipoVeiculo): string;
begin
  result := EnumeradoToStr(t, ['0','1'], [tvTracao, tvReboque]);
end;

function StrToTpVeiculo(out ok: boolean; const s: string): TpcteTipoVeiculo;
begin
  result := StrToEnumerado(ok, s, ['0','1'], [tvTracao, tvReboque]);
end;

function GetVersaoModalCTe(AVersaoDF: TVersaoCTe; AModal: TpcteModal): string;
begin
  result := '';

  case AVersaoDF of
    ve200: begin
             case AModal of
               mdRodoviario:  result := '2.00';
               mdAereo:       result := '2.00';
               mdAquaviario:  result := '2.00';
               mdFerroviario: result := '2.00';
               mdDutoviario:  result := '2.00';
               mdMultimodal:  result := '2.00';
             end;
           end;
    ve300: begin
             case AModal of
               mdRodoviario:  result := '3.00';
               mdAereo:       result := '3.00';
               mdAquaviario:  result := '3.00';
               mdFerroviario: result := '3.00';
               mdDutoviario:  result := '3.00';
               mdMultimodal:  result := '3.00';
             end;
           end;
    ve400: begin
             case AModal of
               mdRodoviario:  result := '4.00';
               mdAereo:       result := '4.00';
               mdAquaviario:  result := '4.00';
               mdFerroviario: result := '4.00';
               mdDutoviario:  result := '4.00';
               mdMultimodal:  result := '4.00';
             end;
           end;
  end;
end;

function ModeloCTeToStr(const t: TModeloCTe): String;
begin
  Result := EnumeradoToStr(t, ['57', '64', '67'], [moCTe, moGTVe, moCTeOS]);
end;

function StrToModeloCTe(out ok: Boolean; const s: String): TModeloCTe;
begin
  Result := StrToEnumerado(ok, s, ['57', '64', '67'], [moCTe, moGTVe, moCTeOS]);
end;

function ModeloCTeToPrefixo(const t: TModeloCTe): String;
begin
  Case t of
    moCTeOS: Result := 'CTeOS';
    moGTVe: Result := 'GTVe';
  else
    Result := 'CTe';
  end;
end;

function TEspecieToStr(const t: TEspecie): String;
begin
  Result := EnumeradoToStr(t, ['1', '2', '3', '4'],
                              [teNumerario, teCheque, teMoeda, teOutros]);
end;

function StrToTEspecie(out ok: Boolean; const s: String): TEspecie;
begin
  Result := StrToEnumerado(ok, s, ['1', '2', '3', '4'],
                                  [teNumerario, teCheque, teMoeda, teOutros]);
end;

function TpRspSeguroToStr(const t: TpcteRspSeg): String;
begin
  result := EnumeradoToStr(t, ['0', '1', '2', '3', '4', '5'],
                         [rsRemetente, rsExpedidor, rsRecebedor, rsDestinatario,
                          rsEmitenteCTe, rsTomadorServico]);
end;

function TpRspSeguroToStrText(const t: TpcteRspSeg): String;
begin
  result := EnumeradoToStr(t, ['REMETENTE', 'EXPEDIDOR', 'RECEBEDOR',
                               'DESTINATARIO', 'EMITENTE', 'TOMADOR SERVICO'],
                         [rsRemetente, rsExpedidor, rsRecebedor, rsDestinatario,
                          rsEmitenteCTe, rsTomadorServico]);
end;

function StrToTpRspSeguro(out ok: boolean; const s: String ): TpcteRspSeg;
begin
  result := StrToEnumerado(ok, s, ['0', '1', '2', '3', '4', '5'],
                         [rsRemetente, rsExpedidor, rsRecebedor, rsDestinatario,
                          rsEmitenteCTe, rsTomadorServico]);
end;

function TpInfManuToStr(const t: TpInfManu): string;
begin
  result := EnumeradoToStr(t, ['01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','99'],
                              [imCEEAV, imAPCDEA, imSAC,   imAPDENR, imAPQI,  imGSR,   imNR,    imAPCC,
                               imAAGA,  imPI965,  imPI966, imPI967,  imPI968, imPI969, imPI970, imOUTRO]);
end;

function StrToTpInfManu(out ok: boolean; const s: string): TpInfManu;
begin
  result := StrToEnumerado(ok, s, ['01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','99'],
                                  [imCEEAV, imAPCDEA, imSAC,   imAPDENR, imAPQI,  imGSR,   imNR,    imAPCC,
                                   imAAGA,  imPI965,  imPI966, imPI967,  imPI968, imPI969, imPI970, imOUTRO]);
end;

function TpInfManuToStrV2(const t: TpInfManu): string;
begin
  result := EnumeradoToStr(t, ['1','2','3','4','5','6','7','8','9','99'],
                              [imCEEAV, imAPCDEA, imSAC, imAPDENR, imAPQI,
                               imGSR, imNR, imAPCC, imAAGA, imOUTRO]);
end;

function StrToTpInfManuV2(out ok: boolean; const s: string): TpInfManu;
begin
  result := StrToEnumerado(ok, s, ['1','2','3','4','5','6','7','8','9','99'],
                                  [imCEEAV, imAPCDEA, imSAC, imAPDENR, imAPQI,
                                   imGSR, imNR, imAPCC, imAAGA, imOUTRO]);
end;

function UniMedToStr(const t: TpUniMed): String;
begin
  result := EnumeradoToStr(t, ['1','2','3','4','5'],
                              [umKG, umKGG, umLitros, umTI, umUnidades]);
end;

function StrToUniMed(out ok: Boolean; const s: String): TpUniMed;
begin
  result := StrToEnumerado(ok, s, ['1','2','3','4','5'],
                                  [umKG, umKGG, umLitros, umTI, umUnidades]);
end;

function TpFretamentoToStr(const t: TtpFretamento): String;
begin
  result := EnumeradoToStr(t, ['', '1', '2'],
                              [tfNenhum, tfEventual, tpContinuo]);
end;

function StrToTpFretamento(out ok: Boolean; const s: String): TtpFretamento;
begin
  result := StrToEnumerado(ok, s, ['', '1', '2'],
                                  [tfNenhum, tfEventual, tpContinuo]);
end;

function tpCompToStr(const t: TtpComp): String;
begin
  result := EnumeradoToStr(t, ['1', '2', '3', '4', '5', '6'],
                              [tcCustodia, tcEmbarque, tcTempodeespera, tcMalote,
                               tcAdValorem, tcOutros]);
end;

function StrTotpComp(out ok: Boolean; const s: String): TtpComp;
begin
  result := StrToEnumerado(ok, s, ['1', '2', '3', '4', '5', '6'],
                              [tcCustodia, tcEmbarque, tcTempodeespera, tcMalote,
                               tcAdValorem, tcOutros]);
end;

function tpNumerarioToStr(const t: TtpNumerario): String;
begin
  result := EnumeradoToStr(t, ['1', '2'],
                              [tnNacional, tnEstrangeiro]);
end;

function StrTotpNumerario(out ok: Boolean; const s: String): TtpNumerario;
begin
  result := StrToEnumerado(ok, s, ['1', '2'],
                              [tnNacional, tnEstrangeiro]);
end;

function CRTCTeToStr(const t: TCRT): string;
begin
  result := EnumeradoToStr(t, ['', '1', '2', '3', '4'],
    [crtNenhum, crtSimplesNacional, crtSimplesExcessoReceita, crtRegimeNormal,
     crtSimplesNacionalMEI]);
end;

function StrToCRTCTe(out ok: boolean; const s: string): TCRT;
begin
  result := StrToEnumerado(ok, s, ['', '1', '2', '3', '4'],
    [crtNenhum, crtSimplesNacional, crtSimplesExcessoReceita, crtRegimeNormal,
     crtSimplesNacionalMEI]);
end;

function tpMotivoToStr(const t: TtpMotivo): string;
begin
  result := EnumeradoToStr(t, ['1', '2', '3', '4'],
    [tmNaoEncontrado, tmRecusa, tmInexistente, tmOutro]);
end;

function StrTotpMotivo(out ok: boolean; const s: string): TtpMotivo;
begin
  result := StrToEnumerado(ok, s, ['1', '2', '3', '4'],
    [tmNaoEncontrado, tmRecusa, tmInexistente, tmOutro]);
end;

function StrToTpEventoCTe(out ok: boolean; const s: string): TpcnTpEvento;
begin
  Result := StrToEnumerado(ok, s,
            ['-99999', '110110', '110111', '110113', '110160', '110170',
             '110180', '110181', '610110', '310610', '310611', '610111',
             '110190', '110191'],
            [teNaoMapeado, teCCe, teCancelamento, teEPEC, teMultiModal,
             teGTV, teComprEntrega, teCancComprEntrega, tePrestDesacordo,
             teMDFeAutorizado2, teMDFeCancelado2, teCancPrestDesacordo,
             teInsucessoEntregaCTe, teCancInsucessoEntregaCTe]);
end;

initialization
  RegisterStrToTpEventoDFe(StrToTpEventoCTe, 'CTe');

end.


function StrToTEspecie(out ok: Boolean; const s: string): TEspecie;
var
  idx: TEspecie;
begin
  ok := True;

  for idx := Low(TEspecieArrayStrings) to High(TEspecieArrayStrings) do
  begin
    if (TEspecieArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TEspecie: %s', [s]);
end;

function TpRspSeguroToStr(const t: TpcteRspSeg): string;
begin
  result := TRspSegArrayStrings[t];
end;

function TpRspSeguroToStrText(const t: TpcteRspSeg): string;
begin
  result := TRspSegDescArrayStrings[t];
end;

function StrToTpRspSeguro(out ok: boolean; const s: string ): TpcteRspSeg;
var
  idx: TpcteRspSeg;
begin
  ok := True;

  for idx := Low(TRspSegArrayStrings) to High(TRspSegArrayStrings) do
  begin
    if (TRspSegArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcteRspSeg: %s', [s]);
end;

function TpInfManuToStr(const t: TpInfManu): string;
begin
  result := TpInfManuArrayStrings[t];
end;

function StrToTpInfManu(out ok: boolean; const s: string): TpInfManu;
var
  idx: TpInfManu;
begin
  ok := True;

  for idx := Low(TpInfManuArrayStrings) to High(TpInfManuArrayStrings) do
  begin
    if (TpInfManuArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpInfManu: %s', [s]);
end;

function TpInfManuToStrV2(const t: TpInfManu): string;
begin
  result := TpInfManuV2ArrayStrings[t];
end;

function StrToTpInfManuV2(out ok: boolean; const s: string): TpInfManu;
var
  idx: TpInfManu;
begin
  ok := True;

  for idx := Low(TpInfManuV2ArrayStrings) to High(TpInfManuV2ArrayStrings) do
  begin
    if (TpInfManuV2ArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpInfManu: %s', [s]);
end;

function UniMedToStr(const t: TpUniMed): string;
begin
  result := TpUniMedArrayStrings[t];
end;

function StrToUniMed(out ok: Boolean; const s: string): TpUniMed;
var
  idx: TpUniMed;
begin
  ok := True;

  for idx := Low(TpUniMedArrayStrings) to High(TpUniMedArrayStrings) do
  begin
    if (TpUniMedArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpUniMed: %s', [s]);
end;

function TpFretamentoToStr(const t: TtpFretamento): string;
begin
  result := TtpFretamentoArrayStrings[t];
end;

function StrToTpFretamento(out ok: Boolean; const s: string): TtpFretamento;
var
  idx: TtpFretamento;
begin
  ok := True;

  for idx := Low(TtpFretamentoArrayStrings) to High(TtpFretamentoArrayStrings) do
  begin
    if (TtpFretamentoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpFretamento: %s', [s]);
end;

function tpCompToStr(const t: TtpComp): string;
begin
  result := TtpCompArrayStrings[t];
end;

function StrTotpComp(out ok: Boolean; const s: string): TtpComp;
var
  idx: TtpComp;
begin
  ok := True;

  for idx := Low(TtpCompArrayStrings) to High(TtpCompArrayStrings) do
  begin
    if (TtpCompArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpComp: %s', [s]);
end;

function tpNumerarioToStr(const t: TtpNumerario): string;
begin
  result := TtpNumerarioArrayStrings[t];
end;

function StrTotpNumerario(out ok: Boolean; const s: string): TtpNumerario;
var
  idx: TtpNumerario;
begin
  ok := True;

  for idx := Low(TtpNumerarioArrayStrings) to High(TtpNumerarioArrayStrings) do
  begin
    if (TtpNumerarioArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpNumerario: %s', [s]);
end;

function CRTCTeToStr(const t: TCRT): string;
begin
  result := TCRTArrayStrings[t];
end;

function StrToCRTCTe(out ok: boolean; const s: string): TCRT;
var
  idx: TCRT;
begin
  ok := True;

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

function tpMotivoToStr(const t: TtpMotivo): string;
begin
  result := TtpMotivoArrayStrings[t];
end;

function StrTotpMotivo(out ok: boolean; const s: string): TtpMotivo;
var
  idx: TtpMotivo;
begin
  ok := True;

  for idx := Low(TtpMotivoArrayStrings) to High(TtpMotivoArrayStrings) do
  begin
    if (TtpMotivoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpMotivo: %s', [s]);
end;

initialization
  RegisterStrToTpEventoDFe(StrToTpEventoCTe, 'CTe');

end.
