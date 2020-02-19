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

unit pcnConversaoNF3e;

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

  TtpPartComp = (tpMini, tpMultiplas, tpAutoconsumo, tpGeracaoCompartilhada);

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

  TmotDifTarif = (mdtDecisaoJudicial, mdtDecisaoDistribuidora, mdtDesconto);

  TtpBand = (tbVerde, tbAmarela, tbVermelha1, tbVermelha2);

  TmotDifBand = (mdbDecisaoJudicial, mdbDecisaoDistribuidora, mdbDesconto);

  TindOrigemQtd = (ioMedia, ioMedido, ioContatada, ioCalculada, ioCusto,
                   ioSemQuantidade);

  TtpGrMed = (tgmDemanda, tgmDemandaReativa, tgmEnergiaAtiva,
              tgmEnergiaAtivaInjetada, tgmEnergiaReativa);

  TtpMotNaoLeitura = (tmNenhum, tmConsumidor, tmDistribuidora, tmIndependente);

  TtpProc = (tpSEFAZ, tpJusticaFederal, tpJusticaEstadual);

  TtpLanc = (tlDebito, tlCredito);

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

function StrToTpEventoNF3e(out ok: boolean; const s: string): TpcnTpEvento;

implementation

uses
  typinfo;

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
  Result := EnumeradoToStr(t, ['1', '2', '3', '4'],
    [tpMini, tpMultiplas, tpAutoconsumo, tpGeracaoCompartilhada]);
end;

function StrTotpPartComp(out ok: Boolean; const s: String): TtpPartComp;
begin
  Result := StrToEnumerado(ok, s, ['1', '2', '3', '4'],
    [tpMini, tpMultiplas, tpAutoconsumo, tpGeracaoCompartilhada]);
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
  Result := EnumeradoToStr(t, ['01', '02', '03'],
    [mdtDecisaoJudicial, mdtDecisaoDistribuidora, mdtDesconto]);
end;

function StrTomotDifTarif(out ok: Boolean; const s: String): TmotDifTarif;
begin
  Result := StrToEnumerado(ok, s, ['01', '02', '03'],
    [mdtDecisaoJudicial, mdtDecisaoDistribuidora, mdtDesconto]);
end;

function tpBandToStr(const t: TtpBand): String;
begin
  Result := EnumeradoToStr(t, ['1', '2', '3', '4'],
    [tbVerde, tbAmarela, tbVermelha1, tbVermelha2]);
end;

function StrTotpBand(out ok: Boolean; const s: String): TtpBand;
begin
  Result := StrToEnumerado(ok, s, ['1', '2', '3', '4'],
    [tbVerde, tbAmarela, tbVermelha1, tbVermelha2]);
end;

function motDifBandToStr(const t: TmotDifBand): String;
begin
  Result := EnumeradoToStr(t, ['01', '02', '03'],
    [mdbDecisaoJudicial, mdbDecisaoDistribuidora, mdbDesconto]);
end;

function StrTomotDifBand(out ok: Boolean; const s: String): TmotDifBand;
begin
  Result := StrToEnumerado(ok, s, ['01', '02', '03'],
    [mdbDecisaoJudicial, mdbDecisaoDistribuidora, mdbDesconto]);
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

