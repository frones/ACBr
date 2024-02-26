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

  TStatusACBrCTe = (stCTeIdle, stCTeStatusServico, stCTeRecepcao, stCTeRetRecepcao,
                    stCTeConsulta, stCTeCancelamento, stCTeInutilizacao,
                    stCTeRecibo, stCTeCadastro, stCTeEmail, stCTeCCe,
                    stCTeEvento, stCTeDistDFeInt, stCTeEnvioWebService);

  TVersaoCTe = (ve200, ve300, ve400);

const
  TVersaoCTeArrayStrings: array[TVersaoCTe] of string = ('2.00', '3.00', '4.00');
  TVersaoCTeArrayDouble: array[TVersaoCTe] of Double = (2.00, 3.00, 4.00);

type
  TSchemaCTe = ( schErro, schCTe, schCTeOS, schGTVe, schcancCTe, schInutCTe,
                 schEventoCTe,
                 schconsReciCTe, schconsSitCTe, schconsStatServCTe, schconsCad,
                 schcteModalAereo, schcteModalAquaviario, schcteModalDutoviario,
                 schcteModalFerroviario, schcteModalRodoviario, schcteMultiModal,
                 schevEPECCTe, schevCancCTe, schevRegMultimodal, schevCCeCTe,
                 schdistDFeInt, schcteModalRodoviarioOS, schevPrestDesacordo,
                 schevGTV, schevCECTe, schevCancCECTe, schevCancPrestDesacordo,
                 schevIECTe, schevCancIECTe);

const
  TSchemaCTeArrayStrings: array[TSchemaCTe] of string = ('', '', '', '', '', '',
    '', '', '', '', '', '', '', '', '', '', '', 'evEPECCTe', 'evCancCTe',
    'evRegMultimodal', 'evCCeCTe', 'distDFeInt', 'cteModalRodoviarioOS',
    'evPrestDesacordo', 'evGTV', 'evCECTe', 'evCancCECTe',
    'evCancPrestDesacordo', 'evIECTe', 'evCancIECTe');

type
  TLayOutCTe = (LayCTeRecepcao, LayCTeRetRecepcao, LayCTeCancelamento,
                LayCTeInutilizacao, LayCTeConsulta, LayCTeStatusServico,
                LayCTeCadastro, LayCTeEvento, LayCTeEventoAN,
                LayCTeDistDFeInt, LayCTeRecepcaoOS, LayCTeRecepcaoSinc,
                LayCTeRecepcaoGTVe, LayCTeURLQRCode, LayURLConsultaCTe);

const
  TLayOutCTeArrayStrings: array[TLayOutCTe] of string = ('CTeRecepcao',
    'CTeRetRecepcao', 'CTeCancelamento', 'CTeInutilizacao',
    'CTeConsultaProtocolo', 'CTeStatusServico', 'CTeConsultaCadastro',
    'RecepcaoEvento', 'RecepcaoEventoAN', 'CTeDistribuicaoDFe', 'CTeRecepcaoOS',
    'CTeRecepcaoSinc', 'CTeRecepcaoGTVe', 'CTeURLQRCode', 'URLConsultaCTe');

type
  TModeloCTe = (moCTe, moCTeOS, moGTVe);

const
  TModeloCTeArrayStrings: array[TModeloCTe] of string = ('57', '64', '67');

type
  TpcteFormaPagamento = (fpPago, fpAPagar, fpOutros);

const
  TFormaPagamentoArrayStrings: array[TpcteFormaPagamento] of string = ('0',
    '1', '2');
  TFormaPagamentoDescArrayStrings: array[TpcteFormaPagamento]
     of string = ('PAGO', 'A PAGAR', 'OUTROS');

type
  TpcteTipoCTe = (tcNormal, tcComplemento, tcAnulacao, tcSubstituto, tcGTVe);

const
  TTipoCTeArrayStrings: array[TpcteTipoCTe] of string = ('0', '1', '2', '3',
    '4');
  TTipoCTeDescArrayStrings: array[TpcteTipoCTe] of string = ('NORMAL',
    'COMPLEMENTO', 'ANULAÇÃO', 'SUBSTITUTO', 'GTVe');

type
  TpcteTipoServico = (tsNormal, tsSubcontratacao, tsRedespacho, tsIntermediario,
                      tsMultimodal, tsTranspPessoas, tsTranspValores,
                      tsExcessoBagagem, tsGTV);

const
  TTipoServicoArrayStrings: array[TpcteTipoServico] of string = ('0', '1',
    '2', '3', '4', '6', '7', '8', '9');
  TTipoServicoDescArrayStrings: array[TpcteTipoServico]
    of string = ('NORMAL', 'SUBCONTRATAÇÃO', 'REDESPACHO',
      'REDESP. INTERMEDIÁRIO', 'VINC. A MULTIMODAL', 'TRANSP. PESSOAS',
      'TRANSP. VALORES', 'EXCESSO BAGAGEM', 'GTV');

type
  TpcteRetira = (rtSim, rtNao);

const
  TRetiraArrayStrings: array[TpcteRetira] of string = ('0', '1');

type
  TpcteTomador = (tmRemetente, tmExpedidor, tmRecebedor, tmDestinatario,
                  tmOutros);

const
  TTomadorArrayStrings: array[TpcteTomador] of string = ('0', '1', '2', '3',
    '4');
  TTomadorDescArrayStrings: array[TpcteTomador] of string = ('REMETENTE',
    'EXPEDIDOR', 'RECEBEDOR', 'DESTINATARIO', 'OUTROS');

type
  TpcteLotacao = (ltNao, ltSim);

const
  TLotacaoArrayStrings: array[TpcteLotacao] of string = ('0', '1');

type
  TpcteDirecao = (drNorte, drLeste, drSul, drOeste);

const
  TDirecaoArrayStrings: array[TpcteDirecao] of string = ('N', 'L', 'S', 'O');

type
  TpcteTipoTrafego = (ttProprio, ttMutuo, ttRodoferroviario, ttRodoviario);

const
  TTipoTrafegoArrayStrings: array[TpcteTipoTrafego] of string = ('0', '1',
    '2', '3');

type
  TpcteTipoDataPeriodo = (tdSemData, tdNaData, tdAteData, tdApartirData,
                          tdNoPeriodo, tdNaoInformado);

const
  TTipoDataPeriodoArrayStrings: array[TpcteTipoDataPeriodo] of string = ('0',
    '1', '2', '3', '4', 'N');

type
  TpcteTipoHorarioIntervalo = (thSemHorario, thNoHorario, thAteHorario,
                               thApartirHorario, thNoIntervalo, thNaoInformado);

const
  TTipoHorarioIntervaloArrayStrings: array[TpcteTipoHorarioIntervalo]
     of string = ('0', '1', '2', '3', '4', 'N');

type
  TpcteTipoDocumento = (tdDeclaracao, tdDutoviario, tdCFeSAT, tdNFCe, tdOutros);

const
  TTipoDocumentoArrayStrings: array[TpcteTipoDocumento] of string = ('00',
    '10', '59', '65', '99');

type
  TpcteTipoDocumentoAnterior = (daCTRC, daCTAC, daACT, daNF7, daNF27, daCAN,
                                daCTMC, daATRE, daDTA, daCAI, daCCPI, daCA,
                                daTIF, daBL, daOutros);

const
  TTipoDocumentoAnteriorArrayStrings: array[TpcteTipoDocumentoAnterior]
     of string = ('00', '01', '02', '03', '04', '05', '06', '07', '08', '09',
       '10', '11', '12', '13', '99');

type
  TpcteRspPagPedagio = (rpEmitente, rpRemetente, rpExpedidor, rpRecebedor,
                        rpDestinatario, rpTomadorServico);

const
  TRspPagPedagioArrayStrings: array[TpcteRspPagPedagio] of string = ('0',
    '1', '2', '3', '4', '5');

type
  TpcteTipoDispositivo = (tdCartaoMagnetico, tdTAG, tdTicket);

const
  TTipoDispositivoArrayStrings: array[TpcteTipoDispositivo]
     of string = ('1', '2', '3');

type
  TpcteTipoPropriedade = (tpProprio, tpTerceiro);

const
  TTipoPropriedadeArrayStrings: array[TpcteTipoPropriedade] of string = ('P',
    'T');

type
  TpcteTrafegoMutuo = (tmOrigem, tmDestino);

const
  TTrafegoMutuoArrayStrings: array[TpcteTrafegoMutuo] of string = ('1', '2');

type
  TpcnindNegociavel = (inNaoNegociavel, inNegociavel);

const
  TindNegociavelArrayStrings: array[TpcnindNegociavel] of string = ('0', '1');

type
  TpcteTipoVeiculo = (tvTracao, tvReboque);

const
  TTipoVeiculoArrayStrings: array[TpcteTipoVeiculo] of string = ('0', '1');

type
  TpcteRspSeg = (rsRemetente, rsExpedidor, rsRecebedor, rsDestinatario,
                 rsEmitenteCTe, rsTomadorServico);

const
  TRspSegArrayStrings: array[TpcteRspSeg] of string = ('0', '1', '2', '3',
    '4', '5');
  TRspSegDescArrayStrings: array[TpcteRspSeg] of string = ('REMETENTE',
    'EXPEDIDOR', 'RECEBEDOR', 'DESTINATARIO', 'EMITENTE', 'TOMADOR SERVICO');

type
  TEspecie = (teNumerario, teCheque, teMoeda, teOutros);

const
  TEspecieArrayStrings: array[TEspecie] of string = ('1', '2', '3', '4');

type
  TpInfManu = (imCEEAV, imAPCDEA, imSAC, imAPDENR, imAPQI, imGSR, imNR, imAPCC,
               imAAGA, imPI965, imPI966, imPI967, imPI968, imPI969, imPI970, imOUTRO);

const
  TpInfManuArrayStrings: array[TpInfManu] of string = ('01', '02', '03', '04',
    '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15', '99');
  TpInfManuV2ArrayStrings: array[TpInfManu] of string = ('1', '2', '3', '4', '5',
    '6', '7', '8', '9', '', '', '', '', '', '', '99');

type
  TpUniMed = (umKG, umKGG, umLitros, umTI, umUnidades);

const
  TpUniMedArrayStrings: array[TpUniMed] of string = ('1', '2', '3', '4', '5');

type
  TtpFretamento = (tfNenhum, tfEventual, tpContinuo);

const
  TtpFretamentoArrayStrings: array[TtpFretamento] of string = ('', '1', '2');

type
  TtpComp = (tcCustodia, tcEmbarque, tcTempodeespera, tcMalote, tcAdValorem,
             tcOutros);

const
  TtpCompArrayStrings: array[TtpComp] of string = ('1', '2', '3', '4', '5', '6');

type
  TtpNumerario = (tnNacional, tnEstrangeiro);

const
  TtpNumerarioArrayStrings: array[TtpNumerario] of string = ('1', '2');

type
  TCRT = (crtSimplesNacional, crtSimplesExcessoReceita, crtRegimeNormal,
          crtSimplesNacionalMEI);
const
  TCRTArrayStrings: array[TCRT] of string = ('1', '2', '3', '4');

type
  TtpMotivo = (tmNaoEncontrado, tmRecusa, tmInexistente, tmOutro);

const
  TtpMotivoArrayStrings: array[TtpMotivo] of string = ('1', '2', '3', '4');

{
  Declaração das funções de conversão
}
function StrToTpEventoCTe(out ok: boolean; const s: string): TpcnTpEvento;

function GetVersaoModalCTe(AVersaoDF: TVersaoCTe; AModal: TpcteModal): string;

function LayOutToServico(const t: TLayOutCTe): string;
function ServicoToLayOut(out ok: Boolean; const s: string): TLayOutCTe;

function LayOutToSchema(const t: TLayOutCTe): TSchemaCTe;

function SchemaCTeToStr(const t: TSchemaCTe): string;
function StrToSchemaCTe(out ok: Boolean; const s: string): TSchemaCTe;

function StrToVersaoCTe(out ok: Boolean; const s: string): TVersaoCTe;
function VersaoCTeToStr(const t: TVersaoCTe): string;

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
function TpTomadorToStr(const t: TpcteTomador): string;
function TpTomadorToStrText(const t: TpcteTomador): string;
function StrToTpTomador(out ok: boolean; const s: string ): TpcteTomador;

function TpLotacaoToStr(const t: TpcteLotacao): string;
function StrToTpLotacao(out ok: boolean; const s: string ): TpcteLotacao;

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

function ModeloCTeToStr(const t: TModeloCTe): string;
function StrToModeloCTe(out ok: Boolean; const s: string): TModeloCTe;
function ModeloCTeToPrefixo(const t: TModeloCTe): string;

function TEspecieToStr(const t: TEspecie): string;
function StrToTEspecie(out ok: Boolean; const s: string): TEspecie;

function TpRspSeguroToStr(const t: TpcteRspSeg): string;
function TpRspSeguroToStrText(const t: TpcteRspSeg): string;
function StrToTpRspSeguro(out ok: boolean; const s: string ): TpcteRspSeg;

function TpInfManuToStr(const t: TpInfManu): string;
function StrToTpInfManu(out ok: boolean; const s: string): TpInfManu;

function TpInfManuToStrV2(const t: TpInfManu): string;
function StrToTpInfManuV2(out ok: boolean; const s: string): TpInfManu;

function UniMedToStr(const t: TpUniMed): string;
function StrToUniMed(out ok: Boolean; const s: string): TpUniMed;

function TpFretamentoToStr(const t: TtpFretamento): string;
function StrToTpFretamento(out ok: Boolean; const s: string): TtpFretamento;

function tpCompToStr(const t: TtpComp): string;
function StrTotpComp(out ok: Boolean; const s: string): TtpComp;

function tpNumerarioToStr(const t: TtpNumerario): string;
function StrTotpNumerario(out ok: Boolean; const s: string): TtpNumerario;

function CRTCTeToStr(const t: TCRT): string;
function StrToCRTCTe(out ok: boolean; const s: string): TCRT;

function tpMotivoToStr(const t: TtpMotivo): string;
function StrTotpMotivo(out ok: boolean; const s: string): TtpMotivo;

implementation

uses
  typinfo,
  ACBrBase;

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

function LayOutToServico(const t: TLayOutCTe): string;
begin
  result := TLayOutCTeArrayStrings[t];
end;

function ServicoToLayOut(out ok: Boolean; const s: string): TLayOutCTe;
var
  idx: TLayOutCTe;
begin
  for idx := Low(TLayOutCTeArrayStrings) to High(TLayOutCTeArrayStrings) do
  begin
    if (TLayOutCTeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TLayOutCTe: %s', [s]);
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

function SchemaCTeToStr(const t: TSchemaCTe): string;
begin
  Result := GetEnumName(TypeInfo(TSchemaCTe), Integer( t ) );
  Result := copy(Result, 4, Length(Result)); // Remove prefixo "sch"
end;

function StrToSchemaCTe(out ok: Boolean; const s: string): TSchemaCTe;
var
  P: Integer;
  SchemaStr: string;
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

function StrToVersaoCTe(out ok: Boolean; const s: string): TVersaoCTe;
var
  idx: TVersaoCTe;
begin
  for idx := Low(TVersaoCTeArrayStrings) to High(TVersaoCTeArrayStrings) do
  begin
    if (TVersaoCTeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TVersaoCTe: %s', [s]);
end;

function VersaoCTeToStr(const t: TVersaoCTe): string;
begin
  result := TVersaoCTeArrayStrings[t];
end;

function DblToVersaoCTe(out ok: Boolean; const d: Double): TVersaoCTe;
var
  idx: TVersaoCTe;
begin
  for idx := Low(TVersaoCTeArrayDouble) to High(TVersaoCTeArrayDouble) do
  begin
    if (TVersaoCTeArrayDouble[idx] = d) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TVersaoCTe: %s',
    [FormatFloat('0.00', d)]);
end;

function VersaoCTeToDbl(const t: TVersaoCTe): Double;
begin
  result := TVersaoCTeArrayDouble[t];
end;

function tpCTToStr(const t: TpcteTipoCTe): string;
begin
  result := TTipoCTeArrayStrings[t];
end;

function tpCTToStrText(const t: TpcteTipoCTe): string;
begin
  result := TTipoCTeDescArrayStrings[t];
end;

function tpforPagToStr(const t: TpcteFormaPagamento): string;
begin
  result := TFormaPagamentoArrayStrings[t];
end;

function tpforPagToStrText(const t: TpcteFormaPagamento): string;
begin
  result := TFormaPagamentoDescArrayStrings[t];
end;

function StrTotpforPag(out ok: boolean; const s: string): TpcteFormaPagamento;
var
  idx: TpcteFormaPagamento;
begin
  for idx := Low(TFormaPagamentoDescArrayStrings) to High(TFormaPagamentoDescArrayStrings) do
  begin
    if (TFormaPagamentoDescArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcteFormaPagamento: %s', [s]);
end;

function tpCTePagToStr(const t: TpcteTipoCTe): string;
begin
  result := TTipoCTeArrayStrings[t];
end;

function StrTotpCTe(out ok: boolean; const s: string): TpcteTipoCTe;
var
  idx: TpcteTipoCTe;
begin
  for idx := Low(TTipoCTeArrayStrings) to High(TTipoCTeArrayStrings) do
  begin
    if (TTipoCTeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcteTipoCTe: %s', [s]);
end;

function TpServPagToStr(const t: TpcteTipoServico): string;
begin
  result := TTipoServicoArrayStrings[t];
end;

function TpServToStrText(const t: TpcteTipoServico): string;
begin
  result := TTipoServicoDescArrayStrings[t];
end;

function StrToTpServ(out ok: boolean; const s: string): TpcteTipoServico;
var
  idx: TpcteTipoServico;
begin
  for idx := Low(TTipoServicoArrayStrings) to High(TTipoServicoArrayStrings) do
  begin
    if (TTipoServicoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcteTipoServico: %s', [s]);
end;

function TpRetiraPagToStr(const t: TpcteRetira): string;
begin
  result := TRetiraArrayStrings[t];
end;

function StrToTpRetira(out ok: boolean; const s: string): TpcteRetira;
var
  idx: TpcteRetira;
begin
  for idx := Low(TRetiraArrayStrings) to High(TRetiraArrayStrings) do
  begin
    if (TRetiraArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcteRetira: %s', [s]);
end;

function TpTomadorPagToStr(const t: TpcteTomador): string;
begin
  result := TTomadorArrayStrings[t];
end;

function TpTomadorToStr(const t: TpcteTomador): string;
begin
  result := TTomadorArrayStrings[t];
end;

function TpTomadorToStrText(const t: TpcteTomador): string;
begin
  result := TTomadorDescArrayStrings[t];
end;

function StrToTpTomador(out ok: boolean; const s: string ): TpcteTomador;
var
  idx: TpcteTomador;
begin
  for idx := Low(TTomadorArrayStrings) to High(TTomadorArrayStrings) do
  begin
    if (TTomadorArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcteTomador: %s', [s]);
end;

function TpLotacaoToStr(const t: TpcteLotacao): string;
begin
  result := TLotacaoArrayStrings[t];
end;

function StrToTpLotacao(out ok: boolean; const s: string ): TpcteLotacao;
var
  idx: TpcteLotacao;
begin
  for idx := Low(TLotacaoArrayStrings) to High(TLotacaoArrayStrings) do
  begin
    if (TLotacaoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcteLotacao: %s', [s]);
end;

function TpDirecaoToStr(const t: TpcteDirecao): string;
begin
  result := TDirecaoArrayStrings[t];
end;

function StrToTpDirecao(out ok: boolean; const s: string): TpcteDirecao;
var
  idx: TpcteDirecao;
begin
  for idx := Low(TDirecaoArrayStrings) to High(TDirecaoArrayStrings) do
  begin
    if (TDirecaoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcteDirecao: %s', [s]);
end;

function TpTrafegoToStr(const t: TpcteTipoTrafego): string;
begin
  result := TTipoTrafegoArrayStrings[t];
end;

function StrToTpTrafego(out ok: boolean; const s: string): TpcteTipoTrafego;
var
  idx: TpcteTipoTrafego;
begin
  for idx := Low(TTipoTrafegoArrayStrings) to High(TTipoTrafegoArrayStrings) do
  begin
    if (TTipoTrafegoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcteTipoTrafego: %s', [s]);
end;

function TpDataPeriodoToStr(const t: TpcteTipoDataPeriodo): string;
begin
  result := TTipoDataPeriodoArrayStrings[t];
end;

function StrToTpDataPeriodo(out ok: boolean; const s: string): TpcteTipoDataPeriodo;
var
  idx: TpcteTipoDataPeriodo;
begin
  for idx := Low(TTipoDataPeriodoArrayStrings) to High(TTipoDataPeriodoArrayStrings) do
  begin
    if (TTipoDataPeriodoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcteTipoDataPeriodo: %s', [s]);
end;

function TpHorarioIntervaloToStr(const t: TpcteTipoHorarioIntervalo): string;
begin
  result := TTipoHorarioIntervaloArrayStrings[t];
end;

function StrToTpHorarioIntervalo(out ok: boolean; const s: string): TpcteTipoHorarioIntervalo;
var
  idx: TpcteTipoHorarioIntervalo;
begin
  for idx := Low(TTipoHorarioIntervaloArrayStrings) to High(TTipoHorarioIntervaloArrayStrings) do
  begin
    if (TTipoHorarioIntervaloArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcteTipoHorarioIntervalo: %s', [s]);
end;

function TpDocumentoToStr(const t: TpcteTipoDocumento): string;
begin
  result := TTipoDocumentoArrayStrings[t];
end;

function StrToTpDocumento(out ok: boolean; const s: string): TpcteTipoDocumento;
var
  idx: TpcteTipoDocumento;
begin
  for idx := Low(TTipoDocumentoArrayStrings) to High(TTipoDocumentoArrayStrings) do
  begin
    if (TTipoDocumentoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcteTipoDocumento: %s', [s]);
end;

function TpDocumentoAnteriorToStr(const t: TpcteTipoDocumentoAnterior): string;
begin
  result := TTipoDocumentoAnteriorArrayStrings[t];
end;

function StrToTpDocumentoAnterior(out ok: boolean; const s: string): TpcteTipoDocumentoAnterior;
var
  idx: TpcteTipoDocumentoAnterior;
begin
  for idx := Low(TTipoDocumentoAnteriorArrayStrings) to High(TTipoDocumentoAnteriorArrayStrings) do
  begin
    if (TTipoDocumentoAnteriorArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcteTipoDocumentoAnterior: %s', [s]);
end;

function RspPagPedagioToStr(const t: TpcteRspPagPedagio): string;
begin
  result := TRspPagPedagioArrayStrings[t];
end;

function StrToRspPagPedagio(out ok: boolean; const s: string): TpcteRspPagPedagio;
var
  idx: TpcteRspPagPedagio;
begin
  for idx := Low(TRspPagPedagioArrayStrings) to High(TRspPagPedagioArrayStrings) do
  begin
    if (TRspPagPedagioArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcteRspPagPedagio: %s', [s]);
end;

function TpDispositivoToStr(const t: TpcteTipoDispositivo): string;
begin
  result := TTipoDispositivoArrayStrings[t];
end;

function StrToTpDispositivo(out ok: boolean; const s: string): TpcteTipoDispositivo;
var
  idx: TpcteTipoDispositivo;
begin
  for idx := Low(TTipoDispositivoArrayStrings) to High(TTipoDispositivoArrayStrings) do
  begin
    if (TTipoDispositivoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcteTipoDispositivo: %s', [s]);
end;

function TpPropriedadeToStr(const t: TpcteTipoPropriedade): string;
begin
  result := TTipoPropriedadeArrayStrings[t];
end;

function StrToTpPropriedade(out ok: boolean; const s: string): TpcteTipoPropriedade;
var
  idx: TpcteTipoPropriedade;
begin
  for idx := Low(TTipoPropriedadeArrayStrings) to High(TTipoPropriedadeArrayStrings) do
  begin
    if (TTipoPropriedadeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcteTipoPropriedade: %s', [s]);
end;

function TrafegoMutuoToStr(const t: TpcteTrafegoMutuo): string;
begin
  result := TTrafegoMutuoArrayStrings[t];
end;

function StrToTrafegoMutuo(out ok: boolean; const s: string): TpcteTrafegoMutuo;
var
  idx: TpcteTrafegoMutuo;
begin
  for idx := Low(TTrafegoMutuoArrayStrings) to High(TTrafegoMutuoArrayStrings) do
  begin
    if (TTrafegoMutuoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcteTrafegoMutuo: %s', [s]);
end;

function indNegociavelToStr(const t: TpcnindNegociavel ): string;
begin
  result := TindNegociavelArrayStrings[t];
end;

function StrToindNegociavel(out ok: boolean; const s: string): TpcnindNegociavel;
var
  idx: TpcnindNegociavel;
begin
  for idx := Low(TindNegociavelArrayStrings) to High(TindNegociavelArrayStrings) do
  begin
    if (TindNegociavelArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcnindNegociavel: %s', [s]);
end;

function TpVeiculoToStr(const t: TpcteTipoVeiculo): string;
begin
  result := TTipoVeiculoArrayStrings[t];
end;

function StrToTpVeiculo(out ok: boolean; const s: string): TpcteTipoVeiculo;
var
  idx: TpcteTipoVeiculo;
begin
  for idx := Low(TTipoVeiculoArrayStrings) to High(TTipoVeiculoArrayStrings) do
  begin
    if (TTipoVeiculoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcteTipoVeiculo: %s', [s]);
end;

function ModeloCTeToStr(const t: TModeloCTe): string;
begin
  result := TModeloCTeArrayStrings[t];
end;

function StrToModeloCTe(out ok: Boolean; const s: string): TModeloCTe;
var
  idx: TModeloCTe;
begin
  for idx := Low(TModeloCTeArrayStrings) to High(TModeloCTeArrayStrings) do
  begin
    if (TModeloCTeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TModeloCTe: %s', [s]);
end;

function ModeloCTeToPrefixo(const t: TModeloCTe): string;
begin
  Case t of
    moCTeOS: Result := 'CTeOS';
    moGTVe: Result := 'GTVe';
  else
    Result := 'CTe';
  end;
end;

function TEspecieToStr(const t: TEspecie): string;
begin
  result := TEspecieArrayStrings[t];
end;

function StrToTEspecie(out ok: Boolean; const s: string): TEspecie;
var
  idx: TEspecie;
begin
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
  for idx := Low(TRspSegDescArrayStrings) to High(TRspSegDescArrayStrings) do
  begin
    if (TRspSegDescArrayStrings[idx] = s) then
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
