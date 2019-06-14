////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar CTe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da CTe          //
//                                                                            //
//        site: www.projetocooperar.org/cte                                   //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_cte/        //
//     projeto: http://code.google.com/p/projetocooperar/                     //
//         svn: http://projetocooperar.googlecode.com/svn/trunk/              //
//                                                                            //
// Coordenação: (c) 2009 - Paulo Casagrande                                   //
//                                                                            //
//      Equipe: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//      Versão: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//     Licença: GNU Lesser General Public License (GNU LGPL)                  //
//                                                                            //
//              - Este programa é software livre; você pode redistribuí-lo    //
//              e/ou modificá-lo sob os termos da Licença Pública Geral GNU,  //
//              conforme publicada pela Free Software Foundation; tanto a     //
//              versão 2 da Licença como (a seu critério) qualquer versão     //
//              mais nova.                                                    //
//                                                                            //
//              - Este programa é distribuído na expectativa de ser útil,     //
//              mas SEM QUALQUER GARANTIA; sem mesmo a garantia implícita de  //
//              COMERCIALIZAÇÃO ou de ADEQUAÇÃO A QUALQUER PROPÓSITO EM       //
//              PARTICULAR. Consulte a Licença Pública Geral GNU para obter   //
//              mais detalhes. Você deve ter recebido uma cópia da Licença    //
//              Pública Geral GNU junto com este programa; se não, escreva    //
//              para a Free Software Foundation, Inc., 59 Temple Place,       //
//              Suite 330, Boston, MA - 02111-1307, USA ou consulte a         //
//              licença oficial em http://www.gnu.org/licenses/gpl.txt        //
//                                                                            //
//    Nota (1): - Esta  licença  não  concede  o  direito  de  uso  do nome   //
//              "PCN  -  Projeto  Cooperar  CTe", não  podendo o mesmo ser    //
//              utilizado sem previa autorização.                             //
//                                                                            //
//    Nota (2): - O uso integral (ou parcial) das units do projeto esta       //
//              condicionado a manutenção deste cabeçalho junto ao código     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

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
                LayCTeDistDFeInt, LayCTeRecepcaoOS);

  TSchemaCTe = ( schErro, schCTe, schCTeOS, schcancCTe, schInutCTe, schEventoCTe,
           //      schresCTe, schresEvento, schprocCTe, schprocEventoCTe,
                 schconsReciCTe, schconsSitCTe, schconsStatServCTe, schconsCad,
                 schcteModalAereo, schcteModalAquaviario, schcteModalDutoviario,
                 schcteModalFerroviario, schcteModalRodoviario, schcteMultiModal,
                 schevEPECCTe, schevCancCTe, schevRegMultimodal, schevCCeCTe,
                 schdistDFeInt, schcteModalRodoviarioOS, schevPrestDesacordo,
                 schevGTV{, schprocCTeOS} );

  TStatusACBrCTe = (stCTeIdle, stCTeStatusServico, stCTeRecepcao, stCTeRetRecepcao,
                    stCTeConsulta, stCTeCancelamento, stCTeInutilizacao,
                    stCTeRecibo, stCTeCadastro, stCTeEmail, stCTeCCe,
                    stCTeEvento, stCTeDistDFeInt, stCTeEnvioWebService);

  TModeloCTe = (moCTe, moCTeOS);
  TVersaoCTe = (ve200, ve300);

  TpcteFormaPagamento = (fpPago, fpAPagar, fpOutros);
  TpcteTipoCTe = (tcNormal, tcComplemento, tcAnulacao, tcSubstituto);
  TpcteTipoServico = (tsNormal, tsSubcontratacao, tsRedespacho, tsIntermediario,
                      tsMultimodal, tsTranspPessoas, tsTranspValores, tsExcessoBagagem);
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

function StrToTpEventoCTe(out ok: boolean; const s: string): TpcnTpEvento;

implementation

uses
  typinfo, ACBrUtil;

function LayOutToServico(const t: TLayOutCTe): String;
begin
  Result := EnumeradoToStr(t,
    ['CTeRecepcao', 'CTeRetRecepcao', 'CTeCancelamento',
     'CTeInutilizacao', 'CTeConsultaProtocolo', 'CTeStatusServico',
     'CTeConsultaCadastro', 'RecepcaoEvento', 'RecepcaoEventoAN',
     'CTeDistribuicaoDFe', 'CTeRecepcaoOS'],
    [ LayCTeRecepcao, LayCTeRetRecepcao, LayCTeCancelamento,
      LayCTeInutilizacao, LayCTeConsulta, LayCTeStatusServico,
      LayCTeCadastro, LayCTeEvento, LayCTeEventoAN,
      LayCTeDistDFeInt, LayCTeRecepcaoOS ]);
end;

function ServicoToLayOut(out ok: Boolean; const s: String): TLayOutCTe;
begin
  Result := StrToEnumerado(ok, s,
    ['CTeRecepcao', 'CTeRetRecepcao', 'CTeCancelamento',
     'CTeInutilizacao', 'CTeConsultaProtocolo', 'CTeStatusServico',
     'CTeConsultaCadastro', 'RecepcaoEvento', 'RecepcaoEventoAN',
     'CTeDistribuicaoDFe', 'CTeRecepcaoOS'],
    [ LayCTeRecepcao, LayCTeRetRecepcao, LayCTeCancelamento,
      LayCTeInutilizacao, LayCTeConsulta, LayCTeStatusServico,
      LayCTeCadastro, LayCTeEvento, LayCTeEventoAN,
      LayCTeDistDFeInt, LayCTeRecepcaoOS ]);
end;

function LayOutToSchema(const t: TLayOutCTe): TSchemaCTe;
begin
  case t of
    LayCTeRecepcao:      Result := schCTe;
    LayCTeRecepcaoOS:    Result := schCTeOS;
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
  Result := StrToEnumerado(ok, s, ['2.00', '3.00'], [ve200, ve300]);
end;

function VersaoCTeToStr(const t: TVersaoCTe): String;
begin
  Result := EnumeradoToStr(t, ['2.00', '3.00'], [ve200, ve300]);
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
  else
    Result := 0;
  end;
end;

function tpCTToStr(const t: TpcteTipoCTe): string;
begin
  result := EnumeradoToStr(t, ['0', '1', '2', '3'],
                           [tcNormal, tcComplemento, tcAnulacao, tcSubstituto]);
end;

function tpCTToStrText(const t: TpcteTipoCTe): string;
begin
  result := EnumeradoToStr(t, ['NORMAL', 'COMPLEMENTO', 'ANULAÇÃO', 'SUBSTITUTO'],
                           [tcNormal, tcComplemento, tcAnulacao, tcSubstituto]);
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
  result := EnumeradoToStr(t, ['0','1', '2', '3'],
                           [tcNormal, tcComplemento, tcAnulacao, tcSubstituto]);
end;

function StrTotpCTe(out ok: boolean; const s: string): TpcteTipoCTe;
begin
  result := StrToEnumerado(ok, s, ['0', '1', '2', '3'],
                           [tcNormal, tcComplemento, tcAnulacao, tcSubstituto]);
end;

function TpServPagToStr(const t: TpcteTipoServico): string;
begin
  result := EnumeradoToStr(t, ['0','1', '2', '3', '4', '6', '7', '8'],
                              [tsNormal, tsSubcontratacao, tsRedespacho,
                               tsIntermediario, tsMultimodal, tsTranspPessoas,
                               tsTranspValores, tsExcessoBagagem]);
end;

function TpServToStrText(const t: TpcteTipoServico): string;
begin
  result := EnumeradoToStr(t, ['NORMAL','SUBCONTRATAÇÃO', 'REDESPACHO',
                               'REDESP. INTERMEDIÁRIO', 'VINC. A MULTIMODAL',
                               'TRANSP. PESSOAS', 'TRANSP. VALORES', 'EXCESSO BAGAGEM'],
                              [tsNormal, tsSubcontratacao, tsRedespacho,
                               tsIntermediario, tsMultimodal,
                               tsTranspPessoas, tsTranspValores, tsExcessoBagagem]);
end;

function StrToTpServ(out ok: boolean; const s: string): TpcteTipoServico;
begin
  result := StrToEnumerado(ok, s, ['0', '1', '2', '3', '4', '6', '7', '8'],
                                  [tsNormal, tsSubcontratacao, tsRedespacho,
                                   tsIntermediario, tsMultimodal,
                                   tsTranspPessoas, tsTranspValores, tsExcessoBagagem]);
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
  end;
end;

function ModeloCTeToStr(const t: TModeloCTe): String;
begin
  Result := EnumeradoToStr(t, ['57', '67'], [moCTe, moCTeOS]);
end;

function StrToModeloCTe(out ok: Boolean; const s: String): TModeloCTe;
begin
  Result := StrToEnumerado(ok, s, ['57', '67'], [moCTe, moCTeOS]);
end;

function ModeloCTeToPrefixo(const t: TModeloCTe): String;
begin
  Case t of
    moCTeOS: Result := 'CTeOS';
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
  result := EnumeradoToStr(t, ['', '1','2'],
                              [tfNenhum, tfEventual, tpContinuo]);
end;

function StrToTpFretamento(out ok: Boolean; const s: String): TtpFretamento;
begin
  result := StrToEnumerado(ok, s, ['', '1','2'],
                                  [tfNenhum, tfEventual, tpContinuo]);
end;

function StrToTpEventoCTe(out ok: boolean; const s: string): TpcnTpEvento;
begin
  Result := StrToEnumerado(ok, s,
            ['-99999', '110110', '110111', '110113', '110160', '110170',
             '110180', '110181', '610110'],
            [teNaoMapeado, teCCe, teCancelamento, teEPEC, teMultiModal,
             teGTV, teComprEntrega, teCancComprEntrega, tePrestDesacordo]);
end;

initialization
  RegisterStrToTpEventoDFe(StrToTpEventoCTe, 'CTe');

end.
