{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

unit ACBrNF3eConversao;

interface

uses
  SysUtils, StrUtils, Classes,
  pcnConversao;

type

  TLayOut = (LayNF3eStatusServico, LayNF3eRecepcao, LayNF3eRecepcaoSinc,
             LayNF3eRetRecepcao, LayNF3eConsulta, LayNF3eInutilizacao,
             LayNF3eEvento, LayDistDFeInt);

  TSchemaNF3e = (schErro, schconsStatServNF3e, schNF3e, schconsReciNF3e,
                 schconsSitNF3e, schInutNF3e, schenvEvento, schdistDFeInt,
                 schCancNF3e, schEnvEPEC);

  TStatusACBrNF3e = (stIdle, stNF3eStatusServico, stNF3eRecepcao,
                     stNF3eRetRecepcao, stNF3eRecibo, stNF3eConsulta,
                     stNF3eInutilizacao, stNF3eEvento, stDistDFeInt,
                     stEnvioWebService, stNF3eEmail);

  TVersaoNF3e = (ve100);

  TpcnVersaoQrCode = (veqr000, veqr100, veqr200);

  TpcnFinalidadeNF3e = (fnNormal, fnSubstituicao, fnAjuste);

  TtpAcesso = (taGerador, taCativo, taLivre, taParcialmenteLivre,
               taConsumidorEspecial, taParcialmenteEspecial, taComunhao,
               taSuprimento, taDistribuidora);

  TtpClasse = (tcComercial, tcConsumidorProprio, tcIluminacaoPublica,
               tcIndustrial, tcPoderPublico, tcResidencial, tcRural,
               tcServicoPublico);

  TtpSubClasse = (tscResidencial, tscResidBaixaRenda,
                  tscResidBaixaRendaIndigena, tscResidBaixaRendaQuilombola,
                  tscResidBaixaRendaAssitSocial, tscResidBaixaRendaMultifamiliar,
                  tscComercial, tscServTransporte, tscServComunicacao,
                  tscAssociacao, tscTemplosReligiosos, tscAdmCondominial,
                  tscIluminacaoRodovias, tscSermafaros, tscOutrosServicos,
                  tscAgropecuariaRural, tscAgropecuariaUrbana,
                  tscResidenciaRural, tscCooperativaEletrifRural,
                  tscAgroindustria, tscServPublIrrigacaoRural, tscEscolaAgrotecnica,
                  tscAquicultura, tscPoderPublicoFederal, tscPoderPublicoEstadual,
                  tscPoderPublicoMunicipal, tscTracaoEletrica, tscAguaEsgoto,
                  tscOutros);

  TtpFase = (tfMonofasico, tfBifasico, tfTrifasico);

  TtpGrpTensao = (tgtA1, tgtA2, tgtA3, tgtA3a, tgtA4, tgtAS, tgtB1,
                  tgtB1BaixaRenda, tgtB2, tgtB2Cooperativa, tgtB2ServicoPublico,
                  tgtB3, tgtB4a, tgtB4b);

  TtpModTar = (tmtConvencionalMonomia, tmtConvencionalBinomia, tmtHorariaAzul,
               tmtHorariaAzulAPE, tmtHorariaVerde, tmtHorariaVerdeAPE,
               tmtHorariaBranca, tmtPrePagamento, tmtGeracao, tmtDistribuicao);

  TmotSub = (msErroLeitura, msErroPreco, msDecisaoJudicial,
             msErroCadastral, msErroTributacao);

  TtpGrContrat = (tgDemanda, tgMontante, tgReserva, tgEnergia);

  TtpPosTar = (tpUnico, tpPonta, tpForaPonto, tpIntermediario);

  TtpPartComp = (tpMini, tpMultiplas, tpAutoconsumo, tpGeracaoCompartilhada,
                 tpMista);

  TtpAjuste = (taNenhum, taItemaserSubstituido, taItemSubstituicao, taItemEliminado,
               taItemIncluido);

  TmotAjuste = (maErroLeitura, maErroPreco, maDecisaoJudicial,
                maErroCadastral, maErroTributacao);

  TtpAto = (taREH, taDespacho, taREN);

  TtpTarif = (ttTE, ttTUSD);

  TcPosTarif = (tptUnico, tptPonta, tptForaPonta, tptIntermediario, tptPontaReservado,
                tptForaPontoReservado, tptItermediarioReservado, tptReservado);

  TuMed = (umkW, umkWh);

  TuMedFat = (umfkW, umfkWh, umfkVAr, umfkVArh);

  TmotDifTarif = (mdtDecisaoJudicial, mdtDecisaoDistribuidora, mdtDesconto,
                  mdtAlteracao);

  TtpBand = (tbVerde, tbAmarela, tbVermelha1, tbVermelha2, tbEscassez);

  TmotDifBand = (mdbDecisaoJudicial, mdbDecisaoDistribuidora, mdbDesconto,
                 mdbAlteracao);

  TindOrigemQtd = (ioMedia, ioMedido, ioContatada, ioCalculada, ioCusto,
                   ioSemQuantidade);

  TtpGrMed = (tgmDemanda, tgmDemandaReativa, tgmEnergiaAtiva,
              tgmEnergiaAtivaInjetada, tgmEnergiaReativa);

  TtpMotNaoLeitura = (tmNenhum, tmConsumidor, tmDistribuidora, tmIndependente);

  TtpProc = (tpSEFAZ, tpJusticaFederal, tpJusticaEstadual);

  TtpLanc = (tlDebito, tlCredito);

  TtpFonteEnergia = (feHidraulica, feSolar, feEolica, feBiomassa, feBiogas,
                     feHibrida);

  TSiteAutorizador = (sa0, sa1, sa2, sa3, sa4, sa5, sa6, sa7, sa8, sa9);

  TIndicador = (tiSim, tiNao);

  TCSTCofins = (cof01, cof02, cof03, cof04, cof05, cof06, cof07, cof08, cof09,
                cof49, cof50, cof51, cof52, cof53, cof54, cof55, cof56, cof60,
                cof61, cof62, cof63, cof64, cof65, cof66, cof67, cof70, cof71,
                cof72, cof73, cof74, cof75, cof98, cof99);

  TCSTPis = (pis01, pis02, pis03, pis04, pis05, pis06, pis07, pis08, pis09,
             pis49, pis50, pis51, pis52, pis53, pis54, pis55, pis56, pis60,
             pis61, pis62, pis63, pis64, pis65, pis66, pis67, pis70, pis71,
             pis72, pis73, pis74, pis75, pis98, pis99);

  TCSTIcms = (cst00, cst10, cst20, cst30, cst40, cst41, cst45, cst50, cst51,
              cst60, cst70, cst80, cst81, cst90, cstPart10, cstPart90,
              cstRep41, cstVazio, cstICMSOutraUF, cstICMSSN, cstRep60); //80 e 81 apenas para CTe

  TindIEDest = (inContribuinte, inIsento, inNaoContribuinte);

function StrToEnumerado(out ok: boolean; const s: string; const AString: array of string;
  const AEnumerados: array of variant): variant;
function EnumeradoToStr(const t: variant; const AString:
  array of string; const AEnumerados: array of variant): variant;

  function LayOutToServico(const t: TLayOut): String;
function ServicoToLayOut(out ok: Boolean; const s: String): TLayOut;

function LayOutToSchema(const t: TLayOut): TSchemaNF3e;

function SchemaNF3eToStr(const t: TSchemaNF3e): String;
function StrToSchemaNF3e(const s: String): TSchemaNF3e;
function SchemaEventoToStr(const t: TSchemaNF3e): String;

function StrToVersaoNF3e(out ok: Boolean; const s: String): TVersaoNF3e;
function VersaoNF3eToStr(const t: TVersaoNF3e): String;

function DblToVersaoNF3e(out ok: Boolean; const d: Real): TVersaoNF3e;
function VersaoNF3eToDbl(const t: TVersaoNF3e): Real;

function VersaoQrCodeToStr(const t: TpcnVersaoQrCode): String;
function StrToVersaoQrCode(out ok: Boolean; const s: String): TpcnVersaoQrCode;
function VersaoQrCodeToDbl(const t: TpcnVersaoQrCode): Real;

function finNF3eToStr(const t: TpcnFinalidadeNF3e): String;
function StrTofinNF3e(out ok: Boolean; const s: String): TpcnFinalidadeNF3e;

function tpAcessoToStr(const t: TtpAcesso): String;
function StrTotpAcesso(out ok: Boolean; const s: String): TtpAcesso;

function tpClasseToStr(const t: TtpClasse): String;
function StrTotpClasse(out ok: Boolean; const s: String): TtpClasse;
function tpClasseToDesc(const t: TtpClasse): String;

function tpSubClasseToStr(const t: TtpSubClasse): String;
function StrTotpSubClasse(out ok: Boolean; const s: String): TtpSubClasse;

function tpFaseToStr(const t: TtpFase): String;
function StrTotpFase(out ok: Boolean; const s: String): TtpFase;
function tpFaseToDesc(const t: TtpFase): String;

function tpGrpTensaoToStr(const t: TtpGrpTensao): String;
function StrTotpGrpTensao(out ok: Boolean; const s: String): TtpGrpTensao;

function tpModTarToStr(const t: TtpModTar): String;
function StrTotpModTar(out ok: Boolean; const s: String): TtpModTar;

function MotSubToStr(const t: TmotSub): String;
function StrToMotSub(out ok: Boolean; const s: String): TmotSub;

function tpGrContratToStr(const t: TtpGrContrat): String;
function StrTotpGrContrat(out ok: Boolean; const s: String): TtpGrContrat;

function tpPosTarToStr(const t: TtpPosTar): String;
function StrTotpPosTar(out ok: Boolean; const s: String): TtpPosTar;

function tpPartCompToStr(const t: TtpPartComp): String;
function StrTotpPartComp(out ok: Boolean; const s: String): TtpPartComp;

function tpAjusteToStr(const t: TtpAjuste): String;
function StrTotpAjuste(out ok: Boolean; const s: String): TtpAjuste;

function MotAjusteToStr(const t: TmotAjuste): String;
function StrToMotAjuste(out ok: Boolean; const s: String): TmotAjuste;

function tpAtoToStr(const t: TtpAto): String;
function StrTotpAto(out ok: Boolean; const s: String): TtpAto;

function tpTarifToStr(const t: TtpTarif): String;
function StrTotpTarif(out ok: Boolean; const s: String): TtpTarif;

function cPosTarifToStr(const t: TcPosTarif): String;
function StrTocPosTarif(out ok: Boolean; const s: String): TcPosTarif;

function uMedToStr(const t: TuMed): String;
function StrTouMed(out ok: Boolean; const s: String): TuMed;

function uMedFatToStr(const t: TuMedFat): String;
function StrTouMedFat(out ok: Boolean; const s: String): TuMedFat;
function uMedFatToDesc(const t: TuMedFat): String;

function motDifTarifToStr(const t: TmotDifTarif): String;
function StrTomotDifTarif(out ok: Boolean; const s: String): TmotDifTarif;

function tpBandToStr(const t: TtpBand): String;
function StrTotpBand(out ok: Boolean; const s: String): TtpBand;

function motDifBandToStr(const t: TmotDifBand): String;
function StrTomotDifBand(out ok: Boolean; const s: String): TmotDifBand;

function indOrigemQtdToStr(const t: TindOrigemQtd): String;
function StrToindOrigemQtd(out ok: Boolean; const s: String): TindOrigemQtd;

function tpGrMedToStr(const t: TtpGrMed): String;
function StrTotpGrMed(out ok: Boolean; const s: String): TtpGrMed;

function tpMotNaoLeituraToStr(const t: TtpMotNaoLeitura): String;
function StrTotpMotNaoLeitura(out ok: Boolean; const s: String): TtpMotNaoLeitura;

function tpProcToStr(const t: TtpProc): String;
function StrTotpProc(out ok: Boolean; const s: String): TtpProc;

function tpLancToStr(const t: TtpLanc): String;
function StrTotpLanc(out ok: Boolean; const s: String): TtpLanc;

function tpFonteEnergiaToStr(const t: TtpFonteEnergia): String;
function StrTotpFonteEnergia(out ok: Boolean; const s: String): TtpFonteEnergia;

function SiteAutorizadorToStr(const t: TSiteAutorizador): String;
function StrToSiteAutorizator(out ok: Boolean; const s: String): TSiteAutorizador;

function TIndicadorToStr(const t: TIndicador): string;
function StrToTIndicador(out ok: boolean; const s: string): TIndicador;

function CSTCOFINSToStrTagPosText(const t: TCSTCofins): string;
function CSTCOFINSToStr(const t: TCSTCofins): string;
function StrToCSTCOFINS(out ok: boolean; const s: string): TCSTCofins;

function CSTPISToStrTagPosText(const t: TCSTPis): string;
function CSTPISToStr(const t: TCSTPis): string;
function StrToCSTPIS(out ok: boolean; const s: string): TCSTPis;

function CSTICMSToStr(const t: TCSTIcms): string;
function StrToCSTICMS(out ok: boolean; const s: string): TCSTIcms;
function CSTICMSToStrTagPos(const t: TCSTIcms): string;
function CSTICMSToStrTagPosText(const t: TCSTIcms): string;

function indIEDestToStr(const t: TindIEDest ): string;
function StrToindIEDest(out ok: boolean; const s: string): TindIEDest;

function StrToTpEventoNF3e(out ok: boolean; const s: string): TpcnTpEvento;

implementation

uses
  typinfo;

function StrToEnumerado(out ok: boolean; const s: string; const AString:
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

function LayOutToServico(const t: TLayOut): String;
begin
  Result := EnumeradoToStr(t,
    ['NF3eStatusServico', 'NF3eRecepcao', 'NF3eRecepcaoSinc', 'NF3eRetRecepcao',
     'NF3eConsulta', 'NF3eInutilizacao', 'NF3eRecepcaoEvento', 'NF3eDistribuicaoDFe'],
    [LayNF3eStatusServico, LayNF3eRecepcao, LayNF3eRecepcaoSinc,
     LayNF3eRetRecepcao, LayNF3eConsulta, LayNF3eInutilizacao, LayNF3eEvento,
     LayDistDFeInt] );
end;

function ServicoToLayOut(out ok: Boolean; const s: String): TLayOut;
begin
  Result := StrToEnumerado(ok, s,
    ['NF3eStatusServico', 'NF3eRecepcao', 'NF3eRecepcaoSinc', 'NF3eRetRecepcao',
     'NF3eConsulta', 'NF3eInutilizacao', 'NF3eRecepcaoEvento', 'NF3eDistribuicaoDFe'],
    [LayNF3eStatusServico, LayNF3eRecepcao, LayNF3eRecepcaoSinc,
     LayNF3eRetRecepcao, LayNF3eConsulta, LayNF3eInutilizacao, LayNF3eEvento,
     LayDistDFeInt] );
end;

function LayOutToSchema(const t: TLayOut): TSchemaNF3e;
begin
  case t of
    LayNF3eStatusServico: Result := schconsStatServNF3e;
    LayNF3eRecepcao,
    LayNF3eRecepcaoSinc:  Result := schNF3e;
    LayNF3eRetRecepcao:   Result := schconsReciNF3e;
    LayNF3eConsulta:      Result := schconsSitNF3e;
    LayNF3eInutilizacao:  Result := schInutNF3e;
    LayNF3eEvento:        Result := schenvEvento;
    LayDistDFeInt:        Result := schdistDFeInt;
  else
    Result := schErro;
  end;
end;

function SchemaNF3eToStr(const t: TSchemaNF3e): String;
begin
  Result := GetEnumName(TypeInfo(TSchemaNF3e), Integer( t ) );
  Result := copy(Result, 4, Length(Result)); // Remove prefixo "sch"
end;

function StrToSchemaNF3e(const s: String): TSchemaNF3e;
var
  P: Integer;
  SchemaStr: String;
  CodSchema: Integer;
begin
  P := pos('_',s);
  if p > 0 then
    SchemaStr := copy(s,1,P-1)
  else
    SchemaStr := s;

  if LeftStr(SchemaStr,3) <> 'sch' then
    SchemaStr := 'sch'+SchemaStr;

  CodSchema := GetEnumValue(TypeInfo(TSchemaNF3e), SchemaStr );

  if CodSchema = -1 then
  begin
    raise Exception.Create(Format('"%s" não é um valor TSchemaNF3e válido.',[SchemaStr]));
  end;

  Result := TSchemaNF3e( CodSchema );
end;

function StrToVersaoNF3e(out ok: Boolean; const s: String): TVersaoNF3e;
begin
  Result := StrToEnumerado(ok, s, ['1.00'], [ve100]);
end;

function VersaoNF3eToStr(const t: TVersaoNF3e): String;
begin
  Result := EnumeradoToStr(t, ['1.00'], [ve100]);
end;

 function DblToVersaoNF3e(out ok: Boolean; const d: Real): TVersaoNF3e;
 begin
   ok := True;

   if (d = 1.0) then
     Result := ve100
   else
   begin
     Result := ve100;
     ok := False;
   end;
 end;

 function VersaoNF3eToDbl(const t: TVersaoNF3e): Real;
 begin
   case t of
     ve100: Result := 1.00;
   else
     Result := 0;
   end;
 end;

function VersaoQrCodeToStr(const t: TpcnVersaoQrCode): String;
begin
  Result := EnumeradoToStr(t, ['0', '1', '2'],
    [veqr000, veqr100, veqr200]);
end;

function StrToVersaoQrCode(out ok: Boolean; const s: String): TpcnVersaoQrCode;
begin
  Result := StrToEnumerado(ok, s, ['0', '1', '2'],
    [veqr000, veqr100, veqr200]);
end;

function VersaoQrCodeToDbl(const t: TpcnVersaoQrCode): Real;
begin
  case t of
    veqr000: Result := 0;
    veqr100: Result := 1;
    veqr200: Result := 2;
  else
    Result := 0;
  end;
end;

function SchemaEventoToStr(const t: TSchemaNF3e): String;
begin
  result := EnumeradoToStr(t, ['evCancNF3e'],
    [schCancNF3e]);
end;

function finNF3eToStr(const t: TpcnFinalidadeNF3e): String;
begin
  Result := EnumeradoToStr(t, ['1', '2', '3'],
    [fnNormal, fnSubstituicao, fnAjuste]);
end;

function StrTofinNF3e(out ok: Boolean; const s: String): TpcnFinalidadeNF3e;
begin
  Result := StrToEnumerado(ok, s, ['1', '2', '3'],
    [fnNormal, fnSubstituicao, fnAjuste]);
end;

function tpAcessoToStr(const t: TtpAcesso): String;
begin
  Result := EnumeradoToStr(t, ['0', '1', '2', '3', '4', '5', '6', '7', '8'],
       [taGerador, taCativo, taLivre, taParcialmenteLivre, taConsumidorEspecial,
        taParcialmenteEspecial, taComunhao, taSuprimento, taDistribuidora]);
end;

function StrTotpAcesso(out ok: Boolean; const s: String): TtpAcesso;
begin
  Result := StrToEnumerado(ok, s, ['0', '1', '2', '3', '4', '5', '6', '7', '8'],
       [taGerador, taCativo, taLivre, taParcialmenteLivre, taConsumidorEspecial,
        taParcialmenteEspecial, taComunhao, taSuprimento, taDistribuidora]);
end;

function tpClasseToStr(const t: TtpClasse): String;
begin
  Result := EnumeradoToStr(t, ['01', '02', '03', '04', '05', '06', '07', '08'],
    [tcComercial, tcConsumidorProprio, tcIluminacaoPublica, tcIndustrial,
     tcPoderPublico, tcResidencial, tcRural, tcServicoPublico]);
end;

function StrTotpClasse(out ok: Boolean; const s: String): TtpClasse;
begin
  Result := StrToEnumerado(ok, s, ['01', '02', '03', '04', '05', '06', '07', '08'],
    [tcComercial, tcConsumidorProprio, tcIluminacaoPublica, tcIndustrial,
     tcPoderPublico, tcResidencial, tcRural, tcServicoPublico]);
end;

function tpClasseToDesc(const t: TtpClasse): String;
begin
  Result := EnumeradoToStr(t, ['Comercial', 'Consumo Proprio', 'Iluminacao Publica',
    'Industrial', 'Poder Publico', 'Residencial', 'Rural', 'Servico Publico'],
    [tcComercial, tcConsumidorProprio, tcIluminacaoPublica, tcIndustrial,
     tcPoderPublico, tcResidencial, tcRural, tcServicoPublico]);
end;

function tpSubClasseToStr(const t: TtpSubClasse): String;
begin
  Result := EnumeradoToStr(t, ['01', '02', '03', '04', '05', '06', '07', '08',
     '09', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21',
     '22', '23', '24', '25', '26', '27', '28', '99'],
    [tscResidencial, tscResidBaixaRenda, tscResidBaixaRendaIndigena,
     tscResidBaixaRendaQuilombola, tscResidBaixaRendaAssitSocial,
     tscResidBaixaRendaMultifamiliar, tscComercial, tscServTransporte,
     tscServComunicacao, tscAssociacao, tscTemplosReligiosos, tscAdmCondominial,
     tscIluminacaoRodovias, tscSermafaros, tscOutrosServicos, tscAgropecuariaRural,
     tscAgropecuariaUrbana, tscResidenciaRural, tscCooperativaEletrifRural,
     tscAgroindustria, tscServPublIrrigacaoRural, tscEscolaAgrotecnica,
     tscAquicultura, tscPoderPublicoFederal, tscPoderPublicoEstadual,
     tscPoderPublicoMunicipal, tscTracaoEletrica, tscAguaEsgoto, tscOutros]);
end;

function StrTotpSubClasse(out ok: Boolean; const s: String): TtpSubClasse;
begin
  Result := StrToEnumerado(ok, s, ['01', '02', '03', '04', '05', '06', '07', '08',
     '09', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21',
     '22', '23', '24', '25', '26', '27', '28', '99'],
    [tscResidencial, tscResidBaixaRenda, tscResidBaixaRendaIndigena,
     tscResidBaixaRendaQuilombola, tscResidBaixaRendaAssitSocial,
     tscResidBaixaRendaMultifamiliar, tscComercial, tscServTransporte,
     tscServComunicacao, tscAssociacao, tscTemplosReligiosos, tscAdmCondominial,
     tscIluminacaoRodovias, tscSermafaros, tscOutrosServicos, tscAgropecuariaRural,
     tscAgropecuariaUrbana, tscResidenciaRural, tscCooperativaEletrifRural,
     tscAgroindustria, tscServPublIrrigacaoRural, tscEscolaAgrotecnica,
     tscAquicultura, tscPoderPublicoFederal, tscPoderPublicoEstadual,
     tscPoderPublicoMunicipal, tscTracaoEletrica, tscAguaEsgoto, tscOutros]);
end;

function tpFaseToStr(const t: TtpFase): String;
begin
  Result := EnumeradoToStr(t, ['1', '2', '3'],
                              [tfMonofasico, tfBifasico, tfTrifasico]);
end;

function StrTotpFase(out ok: Boolean; const s: String): TtpFase;
begin
  Result := StrToEnumerado(ok, s, ['1', '2', '3'],
                                  [tfMonofasico, tfBifasico, tfTrifasico]);
end;

function tpFaseToDesc(const t: TtpFase): String;
begin
  Result := EnumeradoToStr(t, ['Monofasico', 'Bifasico', 'Trifasico'],
                              [tfMonofasico, tfBifasico, tfTrifasico]);
end;

function tpGrpTensaoToStr(const t: TtpGrpTensao): String;
begin
  Result := EnumeradoToStr(t, ['01', '02', '03', '04', '05', '06', '07', '08',
     '09', '10', '11', '12', '13', '14'],
    [tgtA1, tgtA2, tgtA3, tgtA3a, tgtA4, tgtAS, tgtB1, tgtB1BaixaRenda, tgtB2,
     tgtB2Cooperativa, tgtB2ServicoPublico, tgtB3, tgtB4a, tgtB4b]);
end;

function StrTotpGrpTensao(out ok: Boolean; const s: String): TtpGrpTensao;
begin
  Result := StrToEnumerado(ok, s, ['01', '02', '03', '04', '05', '06', '07', '08',
     '09', '10', '11', '12', '13', '14'],
    [tgtA1, tgtA2, tgtA3, tgtA3a, tgtA4, tgtAS, tgtB1, tgtB1BaixaRenda, tgtB2,
     tgtB2Cooperativa, tgtB2ServicoPublico, tgtB3, tgtB4a, tgtB4b]);
end;

function tpModTarToStr(const t: TtpModTar): String;
begin
  Result := EnumeradoToStr(t, ['01', '02', '03', '04', '05', '06', '07', '08',
                               '09', '10'],
    [tmtConvencionalMonomia, tmtConvencionalBinomia, tmtHorariaAzul,
     tmtHorariaAzulAPE, tmtHorariaVerde, tmtHorariaVerdeAPE, tmtHorariaBranca,
     tmtPrePagamento, tmtGeracao, tmtDistribuicao]);
end;

function StrTotpModTar(out ok: Boolean; const s: String): TtpModTar;
begin
  Result := StrToEnumerado(ok, s, ['01', '02', '03', '04', '05', '06', '07', '08',
                                   '09', '10'],
    [tmtConvencionalMonomia, tmtConvencionalBinomia, tmtHorariaAzul,
     tmtHorariaAzulAPE, tmtHorariaVerde, tmtHorariaVerdeAPE, tmtHorariaBranca,
     tmtPrePagamento, tmtGeracao, tmtDistribuicao]);
end;

function MotSubToStr(const t: TmotSub): String;
begin
  Result := EnumeradoToStr(t, ['01', '02', '03', '04', '05'],
    [msErroLeitura, msErroPreco, msDecisaoJudicial, msErroCadastral, msErroTributacao]);
end;

function StrToMotSub(out ok: Boolean; const s: String): TmotSub;
begin
  Result := StrToEnumerado(ok, s, ['01', '02', '03', '04', '05'],
    [msErroLeitura, msErroPreco, msDecisaoJudicial, msErroCadastral, msErroTributacao]);
end;

function tpGrContratToStr(const t: TtpGrContrat): String;
begin
  Result := EnumeradoToStr(t, ['1', '2', '3', '4'],
    [tgDemanda, tgMontante, tgReserva, tgEnergia]);
end;

function StrTotpGrContrat(out ok: Boolean; const s: String): TtpGrContrat;
begin
  Result := StrToEnumerado(ok, s, ['1', '2', '3', '4'],
    [tgDemanda, tgMontante, tgReserva, tgEnergia]);
end;

function tpPosTarToStr(const t: TtpPosTar): String;
begin
  Result := EnumeradoToStr(t, ['0', '1', '2', '3'],
    [tpUnico, tpPonta, tpForaPonto, tpIntermediario]);
end;

function StrTotpPosTar(out ok: Boolean; const s: String): TtpPosTar;
begin
  Result := StrToEnumerado(ok, s, ['0', '1', '2', '3'],
    [tpUnico, tpPonta, tpForaPonto, tpIntermediario]);
end;

function tpPartCompToStr(const t: TtpPartComp): String;
begin
  Result := EnumeradoToStr(t, ['1', '2', '3', '4', '5'],
    [tpMini, tpMultiplas, tpAutoconsumo, tpGeracaoCompartilhada, tpMista]);
end;

function StrTotpPartComp(out ok: Boolean; const s: String): TtpPartComp;
begin
  Result := StrToEnumerado(ok, s, ['1', '2', '3', '4', '5'],
    [tpMini, tpMultiplas, tpAutoconsumo, tpGeracaoCompartilhada, tpMista]);
end;

function tpAjusteToStr(const t: TtpAjuste): String;
begin
  Result := EnumeradoToStr(t, ['0', '1', '2', '3', '4'],
    [taNenhum, taItemaserSubstituido, taItemSubstituicao, taItemEliminado,
     taItemIncluido]);
end;

function StrTotpAjuste(out ok: Boolean; const s: String): TtpAjuste;
begin
  Result := StrToEnumerado(ok, s, ['0', '1', '2', '3', '4'],
    [taNenhum, taItemaserSubstituido, taItemSubstituicao, taItemEliminado,
     taItemIncluido]);
end;

function MotAjusteToStr(const t: TmotAjuste): String;
begin
  Result := EnumeradoToStr(t, ['1', '2', '3', '4', '5'],
    [maErroLeitura, maErroPreco, maDecisaoJudicial, maErroCadastral, maErroTributacao]);
end;

function StrToMotAjuste(out ok: Boolean; const s: String): TmotAjuste;
begin
  Result := StrToEnumerado(ok, s, ['1', '2', '3', '4', '5'],
    [maErroLeitura, maErroPreco, maDecisaoJudicial, maErroCadastral, maErroTributacao]);
end;

function tpAtoToStr(const t: TtpAto): String;
begin
  Result := EnumeradoToStr(t, ['1', '2', '3'],
    [taREH, taDespacho, taREN]);
end;

function StrTotpAto(out ok: Boolean; const s: String): TtpAto;
begin
  Result := StrToEnumerado(ok, s, ['1', '2', '3'],
    [taREH, taDespacho, taREN]);
end;

function tpTarifToStr(const t: TtpTarif): String;
begin
  Result := EnumeradoToStr(t, ['1', '2'],
    [ttTE, ttTUSD]);
end;

function StrTotpTarif(out ok: Boolean; const s: String): TtpTarif;
begin
  Result := StrToEnumerado(ok, s, ['1', '2'],
    [ttTE, ttTUSD]);
end;

function cPosTarifToStr(const t: TcPosTarif): String;
begin
  Result := EnumeradoToStr(t, ['0', '1', '2', '3', '4', '5', '6', '7'],
    [tptUnico, tptPonta, tptForaPonta, tptIntermediario, tptPontaReservado,
                tptForaPontoReservado, tptItermediarioReservado, tptReservado]);
end;

function StrTocPosTarif(out ok: Boolean; const s: String): TcPosTarif;
begin
  Result := StrToEnumerado(ok, s, ['0', '1', '2', '3', '4', '5', '6', '7'],
    [tptUnico, tptPonta, tptForaPonta, tptIntermediario, tptPontaReservado,
                tptForaPontoReservado, tptItermediarioReservado, tptReservado]);
end;

function uMedToStr(const t: TuMed): String;
begin
  Result := EnumeradoToStr(t, ['1', '2'],
    [umkW, umkWh]);
end;

function StrTouMed(out ok: Boolean; const s: String): TuMed;
begin
  Result := StrToEnumerado(ok, s, ['1', '2'],
    [umkW, umkWh]);
end;

function uMedFatToStr(const t: TuMedFat): String;
begin
  Result := EnumeradoToStr(t, ['1', '2', '3', '4'],
    [umfkW, umfkWh, umfkVAr, umfkVArh]);
end;

function StrTouMedFat(out ok: Boolean; const s: String): TuMedFat;
begin
  Result := StrToEnumerado(ok, s, ['1', '2', '3', '4'],
    [umfkW, umfkWh, umfkVAr, umfkVArh]);
end;

function uMedFatToDesc(const t: TuMedFat): String;
begin
  Result := EnumeradoToStr(t, ['kW', 'kWh', 'kVAr', 'kVArh'],
    [umfkW, umfkWh, umfkVAr, umfkVArh]);
end;

function motDifTarifToStr(const t: TmotDifTarif): String;
begin
  Result := EnumeradoToStr(t, ['01', '02', '03', '04'],
    [mdtDecisaoJudicial, mdtDecisaoDistribuidora, mdtDesconto, mdtAlteracao]);
end;

function StrTomotDifTarif(out ok: Boolean; const s: String): TmotDifTarif;
begin
  Result := StrToEnumerado(ok, s, ['01', '02', '03', '04'],
    [mdtDecisaoJudicial, mdtDecisaoDistribuidora, mdtDesconto, mdtAlteracao]);
end;

function tpBandToStr(const t: TtpBand): String;
begin
  Result := EnumeradoToStr(t, ['1', '2', '3', '4', '5'],
    [tbVerde, tbAmarela, tbVermelha1, tbVermelha2, tbEscassez]);
end;

function StrTotpBand(out ok: Boolean; const s: String): TtpBand;
begin
  Result := StrToEnumerado(ok, s, ['1', '2', '3', '4', '5'],
    [tbVerde, tbAmarela, tbVermelha1, tbVermelha2, tbEscassez]);
end;

function motDifBandToStr(const t: TmotDifBand): String;
begin
  Result := EnumeradoToStr(t, ['01', '02', '03', '04'],
    [mdbDecisaoJudicial, mdbDecisaoDistribuidora, mdbDesconto, mdbAlteracao]);
end;

function StrTomotDifBand(out ok: Boolean; const s: String): TmotDifBand;
begin
  Result := StrToEnumerado(ok, s, ['01', '02', '03', '04'],
    [mdbDecisaoJudicial, mdbDecisaoDistribuidora, mdbDesconto, mdbAlteracao]);
end;

function indOrigemQtdToStr(const t: TindOrigemQtd): String;
begin
  Result := EnumeradoToStr(t, ['1', '2', '3', '4', '5', '6'],
    [ioMedia, ioMedido, ioContatada, ioCalculada, ioCusto, ioSemQuantidade]);
end;

function StrToindOrigemQtd(out ok: Boolean; const s: String): TindOrigemQtd;
begin
  Result := StrToEnumerado(ok, s, ['1', '2', '3', '4', '5', '6'],
    [ioMedia, ioMedido, ioContatada, ioCalculada, ioCusto, ioSemQuantidade]);
end;

function tpGrMedToStr(const t: TtpGrMed): String;
begin
  Result := EnumeradoToStr(t, ['01', '02', '03', '04', '05'],
    [tgmDemanda, tgmDemandaReativa, tgmEnergiaAtiva, tgmEnergiaAtivaInjetada,
     tgmEnergiaReativa]);
end;

function StrTotpGrMed(out ok: Boolean; const s: String): TtpGrMed;
begin
  Result := StrToEnumerado(ok, s, ['01', '02', '03', '04', '05'],
    [tgmDemanda, tgmDemandaReativa, tgmEnergiaAtiva, tgmEnergiaAtivaInjetada,
     tgmEnergiaReativa]);
end;

function tpMotNaoLeituraToStr(const t: TtpMotNaoLeitura): String;
begin
  Result := EnumeradoToStr(t, ['0', '1', '2', '3'],
    [tmNenhum, tmConsumidor, tmDistribuidora, tmIndependente]);
end;

function StrTotpMotNaoLeitura(out ok: Boolean; const s: String): TtpMotNaoLeitura;
begin
  Result := StrToEnumerado(ok, s, ['0', '1', '2', '3'],
    [tmNenhum, tmConsumidor, tmDistribuidora, tmIndependente]);
end;

function tpProcToStr(const t: TtpProc): String;
begin
  Result := EnumeradoToStr(t, ['0', '1', '2'],
    [tpSEFAZ, tpJusticaFederal, tpJusticaEstadual]);
end;

function StrTotpProc(out ok: Boolean; const s: String): TtpProc;
begin
  Result := StrToEnumerado(ok, s, ['0', '1', '2'],
    [tpSEFAZ, tpJusticaFederal, tpJusticaEstadual]);
end;

function tpLancToStr(const t: TtpLanc): String;
begin
  Result := EnumeradoToStr(t, ['D', 'C'],
    [tlDebito, tlCredito]);
end;

function StrTotpLanc(out ok: Boolean; const s: String): TtpLanc;
begin
  Result := StrToEnumerado(ok, s, ['D', 'C'],
    [tlDebito, tlCredito]);
end;

function tpFonteEnergiaToStr(const t: TtpFonteEnergia): String;
begin
  Result := EnumeradoToStr(t, ['1', '2', '3', '4', '5', '6'],
            [feHidraulica, feSolar, feEolica, feBiomassa, feBiogas, feHibrida]);
end;

function StrTotpFonteEnergia(out ok: Boolean; const s: String): TtpFonteEnergia;
begin
  Result := StrToEnumerado(ok, s, ['1', '2', '3', '4', '5', '6'],
            [feHidraulica, feSolar, feEolica, feBiomassa, feBiogas, feHibrida]);
end;

function SiteAutorizadorToStr(const t: TSiteAutorizador): String;
begin
  Result := EnumeradoToStr(t, ['0','1', '2', '3', '4', '5', '6', '7', '8', '9'],
            [sa0, sa1, sa2, sa3, sa4, sa5, sa6, sa7, sa8, sa9]);
end;

function StrToSiteAutorizator(out ok: Boolean; const s: String): TSiteAutorizador;
begin
  Result := StrToEnumerado(ok, s, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'],
            [sa0, sa1, sa2, sa3, sa4, sa5, sa6, sa7, sa8, sa9]);
end;

function TIndicadorToStr(const t: TIndicador): string;
begin
  Result := EnumeradoToStr(t, ['1', '0'], [tiSim, tiNao]);
end;

function StrToTIndicador(out ok: boolean; const s: string): TIndicador;
begin
  Result := StrToEnumerado(ok, s, ['1', '0'], [tiSim, tiNao]);
end;

function CSTCOFINSToStrTagPosText(const t: TCSTCofins): string;
begin
     result := EnumeradoToStr(t,
          ['01 - Operação Tributável com Alíquota Básica',
          '02 - Operação Tributável com Alíquota Diferenciada',
          '03 - Operação Tributável com Alíquota por Unidade de Medida de Produto',
          '04 - Operação Tributável Monofásica - Revenda a Alíquota Zero',
          '05 - Operação Tributável por Substituição Tributária',
          '06 - Operação Tributável a Alíquota Zero',
          '07 - Operação Isenta da Contribuição',
          '08 - Operação sem Incidência da Contribuição',
          '09 - Operação com Suspensão da Contribuição',
          '49 - Outras Operações de Saída',
          '50 - Operação com Direito a Crédito - Vinculada Exclusivamente a Receita Tributada no Mercado Interno',
          '51 - Operação com Direito a Crédito - Vinculada Exclusivamente a Receita Não Tributada no Mercado Interno',
          '52 - Operação com Direito a Crédito - Vinculada Exclusivamente a Receita de Exportação',
          '53 - Operação com Direito a Crédito - Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno',
          '54 - Operação com Direito a Crédito - Vinculada a Receitas Tributadas no Mercado Interno e de Exportação',
          '55 - Operação com Direito a Crédito - Vinculada a Receitas Não-Tributadas no Mercado Interno e de Exportação',
          '56 - Operação com Direito a Crédito - Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno, e de Exportação',
          '60 - Crédito Presumido - Operação de Aquisição Vinculada Exclusivamente a Receita Tributada no Mercado Interno',
          '61 - Crédito Presumido - Operação de Aquisição Vinculada Exclusivamente a Receita Não-Tributada no Mercado Interno',
          '62 - Crédito Presumido - Operação de Aquisição Vinculada Exclusivamente a Receita de Exportação',
          '63 - Crédito Presumido - Operação de Aquisição Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno',
          '64 - Crédito Presumido - Operação de Aquisição Vinculada a Receitas Tributadas no Mercado Interno e de Exportação',
          '65 - Crédito Presumido - Operação de Aquisição Vinculada a Receitas Não-Tributadas no Mercado Interno e de Exportação',
          '66 - Crédito Presumido - Operação de Aquisição Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno, e de Exportação',
          '67 - Crédito Presumido - Outras Operações',
          '70 - Operação de Aquisição sem Direito a Crédito',
          '71 - Operação de Aquisição com Isenção',
          '72 - Operação de Aquisição com Suspensão',
          '73 - Operação de Aquisição a Alíquota Zero',
          '74 - Operação de Aquisição sem Incidência da Contribuição',
          '75 - Operação de Aquisição por Substituição Tributária',
          '98 - Outras Operações de Entrada',
          '99 - Outras Operações'],
          [cof01, cof02, cof03, cof04, cof05, cof06, cof07, cof08, cof09, cof49, cof50, cof51, cof52, cof53, cof54, cof55, cof56, cof60, cof61, cof62, cof63, cof64, cof65, cof66, cof67, cof70, cof71, cof72, cof73, cof74, cof75, cof98, cof99]);
end;

function CSTCOFINSToStr(const t: TCSTCofins): string;
begin
  result := EnumeradoToStr(t, ['01', '02', '03', '04', '05', '06', '07', '08', '09', '49', '50', '51', '52', '53', '54', '55', '56', '60', '61', '62', '63', '64', '65', '66', '67', '70', '71', '72', '73', '74', '75', '98', '99'],
    [cof01, cof02, cof03, cof04, cof05, cof06, cof07, cof08, cof09, cof49, cof50, cof51, cof52, cof53, cof54, cof55, cof56, cof60, cof61, cof62, cof63, cof64, cof65, cof66, cof67, cof70, cof71, cof72, cof73, cof74, cof75, cof98, cof99]);
end;

function StrToCSTCOFINS(out ok: boolean; const s: string): TCSTCofins;
begin
  result := StrToEnumerado(ok, s, ['01', '02', '03', '04', '05', '06', '07', '08', '09', '49', '50', '51', '52', '53', '54', '55', '56', '60', '61', '62', '63', '64', '65', '66', '67', '70', '71', '72', '73', '74', '75', '98', '99'],
    [cof01, cof02, cof03, cof04, cof05, cof06, cof07, cof08, cof09, cof49, cof50, cof51, cof52, cof53, cof54, cof55, cof56, cof60, cof61, cof62, cof63, cof64, cof65, cof66, cof67, cof70, cof71, cof72, cof73, cof74, cof75, cof98, cof99]);
end;

function CSTPISToStrTagPosText(const t: TCSTPis): string;
begin
     result := EnumeradoToStr(t,
          ['01 - Operação Tributável com Alíquota Básica',
          '02 - Operação Tributável com Alíquota Diferenciada',
          '03 - Operação Tributável com Alíquota por Unidade de Medida de Produto',
          '04 - Operação Tributável Monofásica - Revenda a Alíquota Zero',
          '05 - Operação Tributável por Substituição Tributária',
          '06 - Operação Tributável a Alíquota Zero',
          '07 - Operação Isenta da Contribuição',
          '08 - Operação sem Incidência da Contribuição',
          '09 - Operação com Suspensão da Contribuição',
          '49 - Outras Operações de Saída',
          '50 - Operação com Direito a Crédito - Vinculada Exclusivamente a Receita Tributada no Mercado Interno',
          '51 - Operação com Direito a Crédito - Vinculada Exclusivamente a Receita Não Tributada no Mercado Interno',
          '52 - Operação com Direito a Crédito - Vinculada Exclusivamente a Receita de Exportação',
          '53 - Operação com Direito a Crédito - Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno',
          '54 - Operação com Direito a Crédito - Vinculada a Receitas Tributadas no Mercado Interno e de Exportação',
          '55 - Operação com Direito a Crédito - Vinculada a Receitas Não-Tributadas no Mercado Interno e de Exportação',
          '56 - Operação com Direito a Crédito - Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno, e de Exportação',
          '60 - Crédito Presumido - Operação de Aquisição Vinculada Exclusivamente a Receita Tributada no Mercado Interno',
          '61 - Crédito Presumido - Operação de Aquisição Vinculada Exclusivamente a Receita Não-Tributada no Mercado Interno',
          '62 - Crédito Presumido - Operação de Aquisição Vinculada Exclusivamente a Receita de Exportação',
          '63 - Crédito Presumido - Operação de Aquisição Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno',
          '64 - Crédito Presumido - Operação de Aquisição Vinculada a Receitas Tributadas no Mercado Interno e de Exportação',
          '65 - Crédito Presumido - Operação de Aquisição Vinculada a Receitas Não-Tributadas no Mercado Interno e de Exportação',
          '66 - Crédito Presumido - Operação de Aquisição Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno, e de Exportação',
          '67 - Crédito Presumido - Outras Operações',
          '70 - Operação de Aquisição sem Direito a Crédito',
          '71 - Operação de Aquisição com Isenção',
          '72 - Operação de Aquisição com Suspensão',
          '73 - Operação de Aquisição a Alíquota Zero',
          '74 - Operação de Aquisição sem Incidência da Contribuição',
          '75 - Operação de Aquisição por Substituição Tributária',
          '98 - Outras Operações de Entrada',
          '99 - Outras Operações'],
          [pis01, pis02, pis03, pis04, pis05, pis06, pis07, pis08, pis09, pis49, pis50, pis51, pis52, pis53, pis54, pis55, pis56, pis60, pis61, pis62, pis63, pis64, pis65, pis66, pis67, pis70, pis71, pis72, pis73, pis74, pis75, pis98, pis99]);
end;

function CSTPISToStr(const t: TCSTPIS): string;
begin
  result := EnumeradoToStr(t, ['01', '02', '03', '04', '05', '06', '07', '08', '09', '49', '50', '51', '52', '53', '54', '55', '56', '60', '61', '62', '63', '64', '65', '66', '67', '70', '71', '72', '73', '74', '75', '98', '99'],
    [pis01, pis02, pis03, pis04, pis05, pis06, pis07, pis08, pis09, pis49, pis50, pis51, pis52, pis53, pis54, pis55, pis56, pis60, pis61, pis62, pis63, pis64, pis65, pis66, pis67, pis70, pis71, pis72, pis73, pis74, pis75, pis98, pis99]);
end;

function StrToCSTPIS(out ok: boolean; const s: string): TCSTPIS;
begin
  result := StrToEnumerado(ok, s, ['01', '02', '03', '04', '05', '06', '07', '08', '09', '49', '50', '51', '52', '53', '54', '55', '56', '60', '61', '62', '63', '64', '65', '66', '67', '70', '71', '72', '73', '74', '75', '98', '99'],
    [pis01, pis02, pis03, pis04, pis05, pis06, pis07, pis08, pis09, pis49, pis50, pis51, pis52, pis53, pis54, pis55, pis56, pis60, pis61, pis62, pis63, pis64, pis65, pis66, pis67, pis70, pis71, pis72, pis73, pis74, pis75, pis98, pis99]);
end;

function CSTICMSToStr(const t: TCSTIcms): string;
begin
  // ID -> N02  - Tributada integralmente
  // ID -> N03  - Tributada e com cobrança do ICMS por substituição tributária
  // ID -> N04  - Com redução de base de cálculo
  // ID -> N05  - Isenta ou não tributada e com cobrança do ICMS por substituição tributária
  // ID -> N06  - Isenta
  // ID -> N06  - Não tributada
  // ID -> N06  - Suspensão
  // ID -> N07  - Diferimento A exigência do preenchimento das informações do ICMS diferido fica à critério de cada UF.
  // ID -> N08  - ICMS cobrado anteriormente por substituição
  // ID -> N09  - Com redução de base de cálculo e cobrança do ICMS por substituição tributária
  // ID -> N10  - ICMS pagto atribuído ao tomador ou ao terceiro previsto na legislação p/ ST
  // ID -> N10a - Operação interestadual para consumidor final com partilhado ICMS devido na operaçãoentre a UF de origem e a UF do destinatário ou a UF definida na legislação. (Ex. UF daconcessionária de entrega do veículos) (v2.0)
  // ID -> N10b - Grupo de informação do ICMS ST devido para a UF de destino,nas operações interestaduais de produtos que tiveram retenção antecipada de ICMS por ST na UF do remetente. Repasse via Substituto Tributário. (v2.0)
  // ID -> N11  - ICMS devido para outras UF
  // ID -> N12  - Outros
  result := EnumeradoToStr(t, ['', '00', '10', '20', '30', '40', '41', '45', '50', '51',
                               '60', '70', '80', '81', '90', '90', 'SN',
                               '10', '90', '41', '60'],
                              [cstVazio, cst00, cst10, cst20, cst30, cst40, cst41, cst45, cst50, cst51,
                              cst60, cst70, cst80, cst81, cst90, cstICMSOutraUF, cstICMSSN,
                              cstPart10, cstPart90, cstRep41, cstRep60]);
end;

function StrToCSTICMS(out ok: boolean; const s: string): TCSTIcms;
begin
  result := StrToEnumerado(ok, s, ['', '00', '10', '20', '30', '40', '41', '45', '50', '51', '60',
                                   '70', '80', '81', '90', '91', 'SN',
                                   '10part', '90part', '41rep', '60rep'],
                                  [cstVazio, cst00, cst10, cst20, cst30, cst40, cst41, cst45, cst50, cst51, cst60,
                                   cst70, cst80, cst81, cst90, cstICMSOutraUF, cstICMSSN,
                                   cstPart10, cstPart90, cstRep41, cstRep60]);
end;

function CSTICMSToStrTagPos(const t: TCSTIcms): string;
begin
  result := EnumeradoToStr(t, ['02', '03', '04', '05', '06', '06', '06', '07', '08', '09', '10', '11', '12', '10a', '10a', '10b', '10b'],
    [cst00, cst10, cst20, cst30, cst40, cst41, cst50, cst51, cst60, cst70, cst80, cst81, cst90, cstPart10 , cstPart90 , cstRep41, cstRep60]);
end;

function CSTICMSToStrTagPosText(const t: TCSTIcms): string;
begin
  result := EnumeradoToStr(t,
   ['VAZIO',
    '00 - TRIBUTAÇÃO NORMAL DO ICMS',
    '10 - TRIBUTAÇÃO COM COBRANÇA DO ICMS POR SUBST. TRIBUTÁRIA',
    '20 - TRIBUTAÇÃO COM REDUÇÃO DE BC DO ICMS',
    '30 - TRIBUTAÇÃO ISENTA E COM COBRANÇA DO ICMS POR SUBST. TRIBUTÁRIA',
    '40 - ICMS ISENÇÃO',
    '41 - ICMS NÃO TRIBUTADO',
    '45 - ICMS ISENTO, NÃO TRIBUTADO OU DIFERIDO',
    '50 - ICMS SUSPENSÃO',
    '51 - ICMS DIFERIDO',
    '60 - ICMS COBRADO POR SUBSTITUIÇÃO TRIBUTÁRIA',
    '70 - TRIBUTAÇÃO COM REDUÇÃO DE BC E COBRANÇA DO ICMS POR SUBST. TRIBUTÁRIA',
    '80 - RESPONSABILIDADE DO RECOLHIMENTO DO ICMS ATRIBUÍDO AO TOMADOR OU 3° POR ST',
    '81 - ICMS DEVIDO À OUTRA UF',
    '90 - ICMS OUTROS',
    '90 - ICMS DEVIDO A UF DE ORIGEM DA PRESTACAO, QUANDO DIFERENTE DA UF DO EMITENTE',
    '90 - SIMPLES NACIONAL',
    '10 - TRIBUTADA E COM COBRANÇA DO ICMS POR SUBSTITUIÇÃO TRIBUTÁRIA - PARTILHA',
    '90 - OUTROS - PARTILHA',
    '41 - NÃO TRIBUTADO - REPASSE',
    '60 - COBRADO ANTERIORMENTE POR SUBSTITUIÇÃO TRIBUTÁRIA - REPASSE'
    ],
    [cstVazio, cst00, cst10, cst20, cst30, cst40, cst41, cst45, cst50, cst51, cst60, cst70,
    cst80, cst81, cst90, cstICMSOutraUF, cstICMSSN, cstPart10, cstPart90, cstRep41, cstRep60]);
end;

function indIEDestToStr(const t: TindIEDest ): string;
begin
  result := EnumeradoToStr(t, ['1', '2', '9'], [inContribuinte, inIsento, inNaoContribuinte]);
end;

function StrToindIEDest(out ok: boolean; const s: string): TindIEDest;
begin
  result := StrToEnumerado(ok, s, ['1', '2', '9'], [inContribuinte, inIsento, inNaoContribuinte]);
end;

function StrToTpEventoNF3e(out ok: boolean; const s: string): TpcnTpEvento;
begin
  Result := StrToEnumerado(ok, s,
            ['-99999', '110111', '240140', '240150', '240170'],
            [teNaoMapeado, teCancelamento, teAutorizadoSubstituicao,
             teAutorizadoAjuste, teLiberacaoPrazoCancelado]);
end;

initialization
  RegisterStrToTpEventoDFe(StrToTpEventoNF3e, 'NF3e');

end.

