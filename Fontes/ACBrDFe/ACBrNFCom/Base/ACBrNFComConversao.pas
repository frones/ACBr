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
  pcnConversao;

type
  TVersaoNFCom = (ve100);

  TStatusNFCom = (stNFComIdle, stNFComStatusServico, stNFComRecepcao,
                  stNFComRetRecepcao, stNFComRecibo, stNFComConsulta,
                  stNFComEvento, stNFComDistDFeInt, stNFComEnvioWebService,
                  stNFComEmail);

  TSchemaNFCom = (schErroNFCom, schconsStatServNFCom, schNFCom, schconsReciNFCom,
                  schconsSitNFCom, schEventoNFCom, schdistDFeInt,
                  schevCancNFCom);

  TLayOutNFCom = (LayNFComStatusServico, LayNFComRecepcao, LayNFComRetRecepcao,
                  LayNFComConsulta, LayNFComEvento, LayDistDFeInt,
                  LayNFComURLQRCode, LayURLConsultaNFCom);

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

const
  TSchemaNFComArrayStrings: array[TSchemaNFCom] of string = ('', '', '', '',
    '', '', '', 'evCancNFCom');

  TVersaoNFComArrayStrings: array[TVersaoNFCom] of string = ('1.00');
  TVersaoNFComArrayDouble: array[TVersaoNFCom] of Double = (1.00);

  TLayOutNFComArrayStrings: array[TLayOutNFCom] of string = ('NFComStatusServico',
    'NFComRecepcao', 'NFComRetRecepcao', 'NFComConsulta', 'NFComRecepcaoEvento',
    'NFComDistribuicaoDFe', 'URL-QRCode', 'URL-ConsultaNFCom');

  TVersaoQrCodeArrayStrings: array[TVersaoQrCode] of string = ('0', '1', '2');
  TVersaoQrCodeArrayDouble: array[TVersaoQrCode] of Double = (0, 1, 2);

  TSiteAutorizadorArrayStrings: array[TSiteAutorizador] of string = ('0','1', '2',
    '3', '4', '5', '6', '7', '8', '9');

  TIndicadorArrayStrings: array[TIndicador] of string = ('1', '0');

  TFinalidadeNFComArrayStrings: array[TFinalidadeNFCom] of string = ('0', '3',
    '4');

  TindIEDestArrayStrings: array[TindIEDest] of string = ('1', '2', '9');

  TTipoFaturamentoArrayStrings: array[TTipoFaturamento] of string = ('0', '1',
    '2');

  TtpAssinanteArrayStrings: array[TtpAssinante] of string = ('1', '2', '3', '4',
    '5', '6', '7', '8', '99');

  TtpServUtilArrayStrings: array[TtpServUtil] of string = ('1', '2', '3', '4',
    '5', '6', '7');

  TmotSubArrayStrings: array[TmotSub] of string = ('01', '02', '03', '04', '05',
    '06');

  TuMedArrayStrings: array[TuMed] of string = ('1', '2', '3', '4');

  TCSTIcmsArrayStrings: array[TCSTIcms] of string = ('00', '20', '40', '41', '51',
    '90', 'SN');

  TCSTPisArrayStrings: array[TCSTPis] of string = ('01', '02', '06', '07', '08',
    '09', '49');

  TCSTCofinsArrayStrings: array[TCSTCofins] of string = ('01', '02', '06', '07',
    '08', '09', '49');

  TtpProcArrayStrings: array[TtpProc] of string = ('0', '1', '2');

  TtpRessarcArrayStrings: array[TtpRessarc] of string = ('1', '2', '99');

  // Futuramente deve ir para a unit ACBrDFeConversao
  TCRTArrayStrings: array[TCRT] of string = ('1', '2', '3');

function VersaoNFComToStr(const t: TVersaoNFCom): string;
function StrToVersaoNFCom(const s: string): TVersaoNFCom;

function DblToVersaoNFCom(out ok: Boolean; const d: Double): TVersaoNFCom;
function VersaoNFComToDbl(const t: TVersaoNFCom): Double;

function SchemaNFComToStr(const t: TSchemaNFCom): string;
function StrToSchemaNFCom(const s: string): TSchemaNFCom;
function SchemaEventoToStr(const t: TSchemaNFCom): string;

function LayOutNFComToSchema(const t: TLayOutNFCom): TSchemaNFCom;

function LayOutNFComToServico(const t: TLayOutNFCom): string;
function ServicoToLayOutNFCom(const s: string): TLayOutNFCom;

function VersaoQrCodeToStr(const t: TVersaoQrCode): string;
function StrToVersaoQrCode(const s: string): TVersaoQrCode;
function VersaoQrCodeToDbl(const t: TVersaoQrCode): Double;

function SiteAutorizadorToStr(const t: TSiteAutorizador): string;
function StrToSiteAutorizator(const s: string): TSiteAutorizador;

function TIndicadorToStr(const t: TIndicador): string;
function StrToTIndicador(const s: string): TIndicador;

function finNFComToStr(const t: TFinalidadeNFCom): string;
function StrTofinNFCom(const s: string): TFinalidadeNFCom;

function indIEDestToStr(const t: TindIEDest): string;
function StrToindIEDest(const s: string): TindIEDest;

function TipoFaturamentoToStr(const t: TTipoFaturamento): string;
function StrToTipoFaturamento(const s: string): TTipoFaturamento;

function tpAssinanteToStr(const t: TtpAssinante): string;
function StrTotpAssinante(const s: string): TtpAssinante;

function tpServUtilToStr(const t: TtpServUtil): string;
function StrTotpServUtil(const s: string): TtpServUtil;

function MotSubToStr(const t: TmotSub): string;
function StrToMotSub(const s: string): TmotSub;

function uMedToStr(const t: TuMed): string;
function StrTouMed(const s: string): TuMed;

function CSTICMSToStr(const t: TCSTIcms): string;
function StrToCSTICMS(const s: string): TCSTIcms;

function CSTPISToStr(const t: TCSTPis): string;
function StrToCSTPIS(const s: string): TCSTPis;

function CSTCOFINSToStr(const t: TCSTCofins): string;
function StrToCSTCOFINS(const s: string): TCSTCofins;

function tpProcToStr(const t: TtpProc): string;
function StrTotpProc(const s: string): TtpProc;

function tpRessarcToStr(const t: TtpRessarc): string;
function StrTotpRessarc(const s: string): TtpRessarc;

function CRTToStr(const t: TCRT): string;
function StrToCRT(const s: string): TCRT;

function StrToTpEventoNFCom(out ok: boolean; const s: string): TpcnTpEvento;

implementation

uses
  typinfo,
  ACBrBase;

function VersaoNFComToStr(const t: TVersaoNFCom): string;
begin
  result := TVersaoNFComArrayStrings[t];
end;

function StrToVersaoNFCom(const s: string): TVersaoNFCom;
var
  idx: TVersaoNFCom;
begin
  for idx := Low(TVersaoNFComArrayStrings) to High(TVersaoNFComArrayStrings) do
  begin
    if (TVersaoNFComArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TVersaoNFCom: %s', [s]);
end;

 function DblToVersaoNFCom(out ok: Boolean; const d: Double): TVersaoNFCom;
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

 function VersaoNFComToDbl(const t: TVersaoNFCom): Double;
 begin
  result := TVersaoNFComArrayDouble[t];
 end;

function SchemaNFComToStr(const t: TSchemaNFCom): string;
begin
  Result := GetEnumName(TypeInfo(TSchemaNFCom), Integer(t));
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

  CodSchema := GetEnumValue(TypeInfo(TSchemaNFCom), SchemaStr);

  if CodSchema = -1 then
  begin
    raise EACBrException.CreateFmt('"%s" não é um valor TSchemaNFCom válido.', [SchemaStr]);
  end;

  Result := TSchemaNFCom(CodSchema);
end;

function SchemaEventoToStr(const t: TSchemaNFCom): string;
begin
  result := TSchemaNFComArrayStrings[t];
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
  result := TLayOutNFComArrayStrings[t];
end;

function ServicoToLayOutNFCom(const s: string): TLayOutNFCom;
var
  idx: TLayOutNFCom;
begin
  for idx := Low(TLayOutNFComArrayStrings) to High(TLayOutNFComArrayStrings) do
  begin
    if (TLayOutNFComArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TLayOutNFCom: %s', [s]);
end;

function VersaoQrCodeToStr(const t: TVersaoQrCode): string;
begin
  result := TVersaoQrCodeArrayStrings[t];
end;

function StrToVersaoQrCode(const s: string): TVersaoQrCode;
var
  idx: TVersaoQrCode;
begin
  for idx := Low(TVersaoQrCodeArrayStrings) to High(TVersaoQrCodeArrayStrings) do
  begin
    if (TVersaoQrCodeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TVersaoQrCode: %s', [s]);
end;

function VersaoQrCodeToDbl(const t: TVersaoQrCode): Double;
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
  result := TSiteAutorizadorArrayStrings[t];
end;

function StrToSiteAutorizator(const s: string): TSiteAutorizador;
var
  idx: TSiteAutorizador;
begin
  for idx := Low(TSiteAutorizadorArrayStrings) to High(TSiteAutorizadorArrayStrings) do
  begin
    if (TSiteAutorizadorArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TSiteAutorizador: %s', [s]);
end;

function TIndicadorToStr(const t: TIndicador): string;
begin
  result := TIndicadorArrayStrings[t];
end;

function StrToTIndicador(const s: string): TIndicador;
var
  idx: TIndicador;
begin
  for idx := Low(TIndicadorArrayStrings) to High(TIndicadorArrayStrings) do
  begin
    if (TIndicadorArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TIndicador: %s', [s]);
end;

function finNFComToStr(const t: TFinalidadeNFCom): string;
begin
  result := TFinalidadeNFComArrayStrings[t];
end;

function StrTofinNFCom(const s: string): TFinalidadeNFCom;
var
  idx: TFinalidadeNFCom;
begin
  for idx := Low(TFinalidadeNFComArrayStrings) to High(TFinalidadeNFComArrayStrings) do
  begin
    if (TFinalidadeNFComArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TFinalidadeNFCom: %s', [s]);
end;

function indIEDestToStr(const t: TindIEDest): string;
begin
  result := TindIEDestArrayStrings[t];
end;

function StrToindIEDest(const s: string): TindIEDest;
var
  idx: TindIEDest;
begin
  for idx := Low(TindIEDestArrayStrings) to High(TindIEDestArrayStrings) do
  begin
    if (TindIEDestArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TindIEDest: %s', [s]);
end;

function TipoFaturamentoToStr(const t: TTipoFaturamento): string;
begin
  result := TTipoFaturamentoArrayStrings[t];
end;

function StrToTipoFaturamento(const s: string): TTipoFaturamento;
var
  idx: TTipoFaturamento;
begin
  for idx := Low(TTipoFaturamentoArrayStrings) to High(TTipoFaturamentoArrayStrings) do
  begin
    if (TTipoFaturamentoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TTipoFaturamento: %s', [s]);
end;

function tpAssinanteToStr(const t: TtpAssinante): string;
begin
  result := TtpAssinanteArrayStrings[t];
end;

function StrTotpAssinante(const s: string): TtpAssinante;
var
  idx: TtpAssinante;
begin
  for idx := Low(TtpAssinanteArrayStrings) to High(TtpAssinanteArrayStrings) do
  begin
    if (TtpAssinanteArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpAssinante: %s', [s]);
end;

function tpServUtilToStr(const t: TtpServUtil): string;
begin
  result := TtpServUtilArrayStrings[t];
end;

function StrTotpServUtil(const s: string): TtpServUtil;
var
  idx: TtpServUtil;
begin
  for idx := Low(TtpServUtilArrayStrings) to High(TtpServUtilArrayStrings) do
  begin
    if (TtpServUtilArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpServUtil: %s', [s]);
end;

function MotSubToStr(const t: TmotSub): string;
begin
  result := TmotSubArrayStrings[t];
end;

function StrToMotSub(const s: string): TmotSub;
var
  idx: TmotSub;
begin
  for idx := Low(TmotSubArrayStrings) to High(TmotSubArrayStrings) do
  begin
    if (TmotSubArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TmotSub: %s', [s]);
end;

function uMedToStr(const t: TuMed): string;
begin
  result := TuMedArrayStrings[t];
end;

function StrTouMed(const s: string): TuMed;
var
  idx: TuMed;
begin
  for idx := Low(TuMedArrayStrings) to High(TuMedArrayStrings) do
  begin
    if (TuMedArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TuMed: %s', [s]);
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

function CSTPISToStr(const t: TCSTPIS): string;
begin
  result := TCSTPISArrayStrings[t];
end;

function StrToCSTPIS(const s: string): TCSTPIS;
var
  idx: TCSTPIS;
begin
  for idx := Low(TCSTPISArrayStrings) to High(TCSTPISArrayStrings) do
  begin
    if (TCSTPISArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TCSTPIS: %s', [s]);
end;

function CSTCOFINSToStr(const t: TCSTCofins): string;
begin
  result := TCSTCofinsArrayStrings[t];
end;

function StrToCSTCOFINS(const s: string): TCSTCofins;
var
  idx: TCSTCofins;
begin
  for idx := Low(TCSTCofinsArrayStrings) to High(TCSTCofinsArrayStrings) do
  begin
    if (TCSTCofinsArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TCSTCofins: %s', [s]);
end;

function tpProcToStr(const t: TtpProc): string;
begin
  result := TtpProcArrayStrings[t];
end;

function StrTotpProc(const s: string): TtpProc;
var
  idx: TtpProc;
begin
  for idx := Low(TtpProcArrayStrings) to High(TtpProcArrayStrings) do
  begin
    if (TtpProcArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpProc: %s', [s]);
end;

function tpRessarcToStr(const t: TtpRessarc): string;
begin
  result := TtpRessarcArrayStrings[t];
end;

function StrTotpRessarc(const s: string): TtpRessarc;
var
  idx: TtpRessarc;
begin
  for idx := Low(TtpRessarcArrayStrings) to High(TtpRessarcArrayStrings) do
  begin
    if (TtpRessarcArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpRessarc: %s', [s]);
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

