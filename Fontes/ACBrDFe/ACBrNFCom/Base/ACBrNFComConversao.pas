{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrNFComConversao;

interface

uses
  SysUtils, StrUtils, Classes,
//  ACBrDFeConversao,
  pcnConversao;

type
  TVersaoNFCom = (ve100);

  TStatusNFCom = (stNFComIdle, stNFComStatusServico, stNFComRecepcao,
                  stNFComRetRecepcao, stNFComRecibo, stNFComConsulta,
                  stNFComEvento, stNFComDistDFeInt, stNFComEnvioWebService,
                  stNFComEmail);

  TSchemaNFCom = (schErroNFCom, schconsStatServNFCom, schNFCom, schconsReciNFCom,
                  schconsSitNFCom, schEventoNFCom, schdistDFeInt,
                  schevCancNFCom, schEnvEPEC);

  TLayOutNFCom = (LayNFComStatusServico, LayNFComRecepcao, LayNFComRetRecepcao,
                  LayNFComConsulta, LayDistDFeInt, LayNFComEvento);

  TVersaoQrCode = (veqr000, veqr100, veqr200);

  TSiteAutorizador = (sa0, sa1, sa2, sa3, sa4, sa5, sa6, sa7, sa8, sa9);

  TIndicador = (tiSim, tiNao);

  TFinalidadeNFCom = (fnNormal, fnSubstituicao, fnAjuste);

  TindIEDest = (inContribuinte, inIsento, inNaoContribuinte);

  TTipoFaturamento = (tfNormal, tfCentralizado, tfCofaturamento);

  TtpAssinante = (taComercial, taIndustrial, taResidencial, taProdutorRural,
                  taOrgaoPublico, taPrestadorServicoTeleCom,
                  taMissoesDiplomaticas, taIgrejasTemplos, taOutros);

  TtpServUtil = (suTelefonia, suComunicacaoDados, suTVAssinatura,
                 suAcessoInternet, suMultimidia, suOutros, suCombo);

  TmotSub = (msErroPreco, msErroCadastral, msDecisaoJudicial,
             msErroTributacao, msDescontServico, msComplValores);

  TuMed = (umMinuto, umMB, umGB, umUN);

  TCSTIcms = (cst00, cst20, cst40, cst41, cst51, cst90, cstICMSSN);

  TCSTPis = (pis01, pis02, pis06, pis07, pis08, pis09, pis49);

  TCSTCofins = (cof01, cof02, cof06, cof07, cof08, cof09, cof49);

  TtpProc = (tpSEFAZ, tpJusticaFederal, tpJusticaEstadual);

  TtpRessarc = (tpCobrancaIndevida, tpInterrupcao, tpOutros);

  // Futuramente deve ir para a unit ACBrDFeConversao
  TCRT = (crtSimplesNacional, crtSimplesExcessoReceita, crtRegimeNormal);

function StrToVersaoNFCom(out ok: Boolean; const s: string): TVersaoNFCom;
function VersaoNFComToStr(const t: TVersaoNFCom): string;

function DblToVersaoNFCom(out ok: Boolean; const d: Real): TVersaoNFCom;
function VersaoNFComToDbl(const t: TVersaoNFCom): Real;

function SchemaNFComToStr(const t: TSchemaNFCom): string;
function StrToSchemaNFCom(const s: string): TSchemaNFCom;
function SchemaEventoToStr(const t: TSchemaNFCom): string;

function LayOutNFComToSchema(const t: TLayOutNFCom): TSchemaNFCom;

function LayOutNFComToServico(const t: TLayOutNFCom): string;
function ServicoToLayOutNFCom(out ok: Boolean; const s: string): TLayOutNFCom;

function VersaoQrCodeToStr(const t: TVersaoQrCode): string;
function StrToVersaoQrCode(out ok: Boolean; const s: string): TVersaoQrCode;
function VersaoQrCodeToDbl(const t: TVersaoQrCode): Real;

function SiteAutorizadorToStr(const t: TSiteAutorizador): string;
function StrToSiteAutorizator(out ok: Boolean; const s: string): TSiteAutorizador;

function TIndicadorToStr(const t: TIndicador): string;
function StrToTIndicador(out ok: boolean; const s: string): TIndicador;

function finNFComToStr(const t: TFinalidadeNFCom): string;
function StrTofinNFCom(out ok: Boolean; const s: string): TFinalidadeNFCom;

function indIEDestToStr(const t: TindIEDest): string;
function StrToindIEDest(out ok: boolean; const s: string): TindIEDest;

function TipoFaturamentoToStr(const t: TTipoFaturamento): string;
function StrToTipoFaturamento(out ok: boolean; const s: string): TTipoFaturamento;

function tpAssinanteToStr(const t: TtpAssinante): string;
function StrTotpAssinante(out ok: boolean; const s: string): TtpAssinante;

function tpServUtilToStr(const t: TtpServUtil): string;
function StrTotpServUtil(out ok: boolean; const s: string): TtpServUtil;

function MotSubToStr(const t: TmotSub): string;
function StrToMotSub(out ok: Boolean; const s: string): TmotSub;

function uMedToStr(const t: TuMed): string;
function StrTouMed(out ok: Boolean; const s: string): TuMed;

function CSTICMSToStr(const t: TCSTIcms): string;
function StrToCSTICMS(out ok: boolean; const s: string): TCSTIcms;

function CSTPISToStr(const t: TCSTPis): string;
function StrToCSTPIS(out ok: boolean; const s: string): TCSTPis;

function CSTCOFINSToStr(const t: TCSTCofins): string;
function StrToCSTCOFINS(out ok: boolean; const s: string): TCSTCofins;

function tpProcToStr(const t: TtpProc): string;
function StrTotpProc(out ok: Boolean; const s: string): TtpProc;

function tpRessarcToStr(const t: TtpRessarc): string;
function StrTotpRessarc(out ok: Boolean; const s: string): TtpRessarc;

function CRTToStr(const t: TCRT): string;
function StrToCRT(out ok: boolean; const s: string): TCRT;

function StrToTpEventoNFCom(out ok: boolean; const s: string): TpcnTpEvento;

implementation

uses
  typinfo;

function StrToVersaoNFCom(out ok: Boolean; const s: string): TVersaoNFCom;
begin
  Result := StrToEnumerado(ok, s, ['1.00'], [ve100]);
end;

function VersaoNFComToStr(const t: TVersaoNFCom): string;
begin
  Result := EnumeradoToStr(t, ['1.00'], [ve100]);
end;

 function DblToVersaoNFCom(out ok: Boolean; const d: Real): TVersaoNFCom;
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

 function VersaoNFComToDbl(const t: TVersaoNFCom): Real;
 begin
   case t of
     ve100: Result := 1.00;
   else
     Result := 0;
   end;
 end;

function SchemaNFComToStr(const t: TSchemaNFCom): string;
begin
  Result := GetEnumName(TypeInfo(TSchemaNFCom), Integer( t ) );
  Result := copy(Result, 4, Length(Result)); // Remove prefixo "sch"
end;

function StrToSchemaNFCom(const s: string): TSchemaNFCom;
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

  CodSchema := GetEnumValue(TypeInfo(TSchemaNFCom), SchemaStr );

  if CodSchema = -1 then
  begin
    raise Exception.Create(Format('"%s" não é um valor TSchemaNFCom válido.', [SchemaStr]));
  end;

  Result := TSchemaNFCom( CodSchema );
end;

function LayOutNFComToSchema(const t: TLayOutNFCom): TSchemaNFCom;
begin
  case t of
    LayNFComStatusServico: Result := schconsStatServNFCom;
    LayNFComRecepcao:      Result := schNFCom;
    LayNFComRetRecepcao:   Result := schconsReciNFCom;
    LayNFComConsulta:      Result := schconsSitNFCom;
    LayNFComEvento:        Result := schEventoNFCom;
    LayDistDFeInt:         Result := schdistDFeInt;
  else
    Result := schErroNFCom;
  end;
end;

function LayOutNFComToServico(const t: TLayOutNFCom): string;
begin
  Result := EnumeradoToStr(t,
    ['NFComStatusServico', 'NFComRecepcao', 'NFComRetRecepcao',
     'NFComConsulta', 'NFComRecepcaoEvento', 'NFComDistribuicaoDFe'],
    [LayNFComStatusServico, LayNFComRecepcao, LayNFComRetRecepcao,
     LayNFComConsulta, LayNFComEvento, LayDistDFeInt]);
end;

function ServicoToLayOutNFCom(out ok: Boolean; const s: string): TLayOutNFCom;
begin
  Result := StrToEnumerado(ok, s,
    ['NFComStatusServico', 'NFComRecepcao', 'NFComRetRecepcao',
     'NFComConsulta', 'NFComRecepcaoEvento', 'NFComDistribuicaoDFe'],
    [LayNFComStatusServico, LayNFComRecepcao, LayNFComRetRecepcao,
     LayNFComConsulta, LayNFComEvento, LayDistDFeInt]);
end;

function VersaoQrCodeToStr(const t: TVersaoQrCode): string;
begin
  Result := EnumeradoToStr(t, ['0', '1', '2'],
                              [veqr000, veqr100, veqr200]);
end;

function StrToVersaoQrCode(out ok: Boolean; const s: string): TVersaoQrCode;
begin
  Result := StrToEnumerado(ok, s, ['0', '1', '2'],
                                  [veqr000, veqr100, veqr200]);
end;

function VersaoQrCodeToDbl(const t: TVersaoQrCode): Real;
begin
  case t of
    veqr000: Result := 0;
    veqr100: Result := 1;
    veqr200: Result := 2;
  else
    Result := 0;
  end;
end;

function SiteAutorizadorToStr(const t: TSiteAutorizador): string;
begin
  Result := EnumeradoToStr(t, ['0','1', '2', '3', '4', '5', '6', '7', '8', '9'],
            [sa0, sa1, sa2, sa3, sa4, sa5, sa6, sa7, sa8, sa9]);
end;

function StrToSiteAutorizator(out ok: Boolean; const s: string): TSiteAutorizador;
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

function SchemaEventoToStr(const t: TSchemaNFCom): string;
begin
  result := EnumeradoToStr(t, ['evCancNFCom'],
                              [schevCancNFCom]);
end;

function finNFComToStr(const t: TFinalidadeNFCom): string;
begin
  Result := EnumeradoToStr(t, ['0', '3', '4'],
                              [fnNormal, fnSubstituicao, fnAjuste]);
end;

function StrTofinNFCom(out ok: Boolean; const s: string): TFinalidadeNFCom;
begin
  Result := StrToEnumerado(ok, s, ['0', '3', '4'],
                                  [fnNormal, fnSubstituicao, fnAjuste]);
end;

function indIEDestToStr(const t: TindIEDest ): string;
begin
  result := EnumeradoToStr(t, ['1', '2', '9'],
                              [inContribuinte, inIsento, inNaoContribuinte]);
end;

function StrToindIEDest(out ok: boolean; const s: string): TindIEDest;
begin
  result := StrToEnumerado(ok, s, ['1', '2', '9'],
                                  [inContribuinte, inIsento, inNaoContribuinte]);
end;

function TipoFaturamentoToStr(const t: TTipoFaturamento): string;
begin
  result := EnumeradoToStr(t, ['0', '1', '2'],
                              [tfNormal, tfCentralizado, tfCofaturamento]);
end;

function StrToTipoFaturamento(out ok: boolean; const s: string): TTipoFaturamento;
begin
  result := StrToEnumerado(ok, s, ['0', '1', '2'],
                                  [tfNormal, tfCentralizado, tfCofaturamento]);
end;

function tpAssinanteToStr(const t: TtpAssinante): string;
begin
  Result := EnumeradoToStr(t, ['1', '2', '3', '4', '5', '6', '7', '8', '99'],
                 [taComercial, taIndustrial, taResidencial, taProdutorRural,
                  taOrgaoPublico, taPrestadorServicoTeleCom,
                  taMissoesDiplomaticas, taIgrejasTemplos, taOutros]);
end;

function StrTotpAssinante(out ok: boolean; const s: string): TtpAssinante;
begin
  Result := StrToEnumerado(ok, s, ['1', '2', '3', '4', '5', '6', '7', '8', '99'],
                 [taComercial, taIndustrial, taResidencial, taProdutorRural,
                  taOrgaoPublico, taPrestadorServicoTeleCom,
                  taMissoesDiplomaticas, taIgrejasTemplos, taOutros]);
end;

function tpServUtilToStr(const t: TtpServUtil): string;
begin
  Result := EnumeradoToStr(t, ['1', '2', '3', '4', '5', '6', '7'],
                 [suTelefonia, suComunicacaoDados, suTVAssinatura,
                  suAcessoInternet, suMultimidia, suOutros, suCombo]);
end;

function StrTotpServUtil(out ok: boolean; const s: string): TtpServUtil;
begin
  Result := StrToEnumerado(ok, s, ['1', '2', '3', '4', '5', '6', '7'],
                 [suTelefonia, suComunicacaoDados, suTVAssinatura,
                  suAcessoInternet, suMultimidia, suOutros, suCombo]);
end;

function MotSubToStr(const t: TmotSub): string;
begin
  Result := EnumeradoToStr(t, ['01', '02', '03', '04', '05', '06'],
                  [msErroPreco, msErroCadastral, msDecisaoJudicial,
                   msErroTributacao, msDescontServico, msComplValores]);
end;

function StrToMotSub(out ok: Boolean; const s: string): TmotSub;
begin
  Result := StrToEnumerado(ok, s, ['01', '02', '03', '04', '05', '06'],
                  [msErroPreco, msErroCadastral, msDecisaoJudicial,
                   msErroTributacao, msDescontServico, msComplValores]);
end;

function uMedToStr(const t: TuMed): string;
begin
  Result := EnumeradoToStr(t, ['1', '2', '3', '4'],
                  [umMinuto, umMB, umGB, umUN]);
end;

function StrTouMed(out ok: Boolean; const s: string): TuMed;
begin
  Result := StrToEnumerado(ok, s, ['1', '2', '3', '4'],
                  [umMinuto, umMB, umGB, umUN]);
end;

function CSTICMSToStr(const t: TCSTIcms): string;
begin
  result := EnumeradoToStr(t, ['00', '20', '40', '41', '51', '90', 'SN'],
                              [cst00, cst20, cst40, cst41, cst51, cst90,
                               cstICMSSN]);
end;

function StrToCSTICMS(out ok: boolean; const s: string): TCSTIcms;
begin
  result := StrToEnumerado(ok, s, ['00', '20', '40', '41', '51', '90', 'SN'],
                              [cst00, cst20, cst40, cst41, cst51, cst90,
                               cstICMSSN]);
end;

function CSTPISToStr(const t: TCSTPIS): string;
begin
  result := EnumeradoToStr(t, ['01', '02', '06', '07', '08', '09', '49'],
                             [pis01, pis02, pis06, pis07, pis08, pis09, pis49]);
end;

function StrToCSTPIS(out ok: boolean; const s: string): TCSTPIS;
begin
  result := StrToEnumerado(ok, s, ['01', '02', '06', '07', '08', '09', '49'],
                             [pis01, pis02, pis06, pis07, pis08, pis09, pis49]);
end;

function CSTCOFINSToStr(const t: TCSTCofins): string;
begin
  result := EnumeradoToStr(t, ['01', '02', '06', '07', '08', '09', '49'],
                             [cof01, cof02, cof06, cof07, cof08, cof09, cof49]);
end;

function StrToCSTCOFINS(out ok: boolean; const s: string): TCSTCofins;
begin
  result := StrToEnumerado(ok, s, ['01', '02', '06', '07', '08', '09', '49'],
                             [cof01, cof02, cof06, cof07, cof08, cof09, cof49]);
end;

function tpProcToStr(const t: TtpProc): string;
begin
  Result := EnumeradoToStr(t, ['0', '1', '2'],
                              [tpSEFAZ, tpJusticaFederal, tpJusticaEstadual]);
end;

function StrTotpProc(out ok: Boolean; const s: string): TtpProc;
begin
  Result := StrToEnumerado(ok, s, ['0', '1', '2'],
                                  [tpSEFAZ, tpJusticaFederal, tpJusticaEstadual]);
end;

function tpRessarcToStr(const t: TtpRessarc): string;
begin
  Result := EnumeradoToStr(t, ['1', '2', '99'],
                              [tpCobrancaIndevida, tpInterrupcao, tpOutros]);
end;

function StrTotpRessarc(out ok: Boolean; const s: string): TtpRessarc;
begin
  Result := StrToEnumerado(ok, s, ['1', '2', '99'],
                                  [tpCobrancaIndevida, tpInterrupcao, tpOutros]);
end;

function CRTToStr(const t: TCRT): string;
begin
  Result := EnumeradoToStr(t, ['','1', '2', '3'],
    [crtRegimeNormal, crtSimplesNacional, crtSimplesExcessoReceita,
     crtRegimeNormal]);
end;

function StrToCRT(out ok: boolean; const s: string): TCRT;
begin
  Result := StrToEnumerado(ok, s, ['','1', '2', '3'],
    [crtRegimeNormal, crtSimplesNacional, crtSimplesExcessoReceita,
     crtRegimeNormal]);
end;

function StrToTpEventoNFCom(out ok: boolean; const s: string): TpcnTpEvento;
begin
  Result := StrToEnumerado(ok, s,
            ['-99999', '110111', '240140', '240150', '240170'],
            [teNaoMapeado, teCancelamento, teAutorizadoSubstituicao,
             teAutorizadoAjuste, teLiberacaoPrazoCancelado]);
end;

initialization
  RegisterStrToTpEventoDFe(StrToTpEventoNFCom, 'NFCom');

end.

