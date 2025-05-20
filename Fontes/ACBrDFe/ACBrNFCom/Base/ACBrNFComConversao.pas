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
  TStatusNFCom = (stNFComIdle, stNFComStatusServico, stNFComRecepcao,
                  stNFComRetRecepcao, stNFComRecibo, stNFComConsulta,
                  stNFComEvento, stNFComDistDFeInt, stNFComEnvioWebService,
                  stNFComEmail);

type
  TVersaoNFCom = (ve100);

const
  TVersaoNFComArrayStrings: array[TVersaoNFCom] of string = ('1.00');
  TVersaoNFComArrayDouble: array[TVersaoNFCom] of Double = (1.00);

type
  TSchemaNFCom = (schErroNFCom, schconsStatServNFCom, schNFCom, schconsReciNFCom,
                  schconsSitNFCom, schEventoNFCom, schdistDFeInt,
                  schevCancNFCom);

const
  TSchemaNFComArrayStrings: array[TSchemaNFCom] of string = ('', '', '', '',
    '', '', '', 'evCancNFCom');

type
  TLayOutNFCom = (LayNFComStatusServico, LayNFComRecepcao, LayNFComRetRecepcao,
                  LayNFComConsulta, LayNFComEvento, LayDistDFeInt,
                  LayNFComURLQRCode, LayURLConsultaNFCom);

const
  TLayOutNFComArrayStrings: array[TLayOutNFCom] of string = ('NFComStatusServico',
    'NFComRecepcao', 'NFComRetRecepcao', 'NFComConsulta', 'NFComRecepcaoEvento',
    'NFComDistribuicaoDFe', 'URL-QRCode', 'URL-ConsultaNFCom');

type
  TVersaoQrCode = (veqr000, veqr100, veqr200);

const
  TVersaoQrCodeArrayStrings: array[TVersaoQrCode] of string = ('0', '1', '2');
  TVersaoQrCodeArrayDouble: array[TVersaoQrCode] of Double = (0, 1, 2);

type
  TSiteAutorizador = (sa0, sa1, sa2, sa3, sa4, sa5, sa6, sa7, sa8, sa9);

const
  TSiteAutorizadorArrayStrings: array[TSiteAutorizador] of string = ('0','1', '2',
    '3', '4', '5', '6', '7', '8', '9');

type
  TIndicador = (tiSim, tiNao);

const
  TIndicadorArrayStrings: array[TIndicador] of string = ('1', '0');

type
  TFinalidadeNFCom = (fnNormal, fnSubstituicao, fnAjuste);

const
  TFinalidadeNFComArrayStrings: array[TFinalidadeNFCom] of string = ('0', '3',
    '4');

type
  TindIEDest = (inContribuinte, inIsento, inNaoContribuinte);

const
  TindIEDestArrayStrings: array[TindIEDest] of string = ('1', '2', '9');

type
  TTipoFaturamento = (tfNormal, tfCentralizado, tfCofaturamento);

const
  TTipoFaturamentoArrayStrings: array[TTipoFaturamento] of string = ('0', '1',
    '2');

type
  TtpAssinante = (taComercial, taIndustrial, taResidencial, taProdutorRural,
                  taOrgaoPublico, taPrestadorServicoTeleCom,
                  taMissoesDiplomaticas, taIgrejasTemplos, taOutros);

const
  TtpAssinanteArrayStrings: array[TtpAssinante] of string = ('1', '2', '3', '4',
    '5', '6', '7', '8', '99');

type
  TtpServUtil = (suTelefonia, suComunicacaoDados, suTVAssinatura,
                 suAcessoInternet, suMultimidia, suOutros, suCombo);

const
  TtpServUtilArrayStrings: array[TtpServUtil] of string = ('1', '2', '3', '4',
    '5', '6', '7');

type
  TmotSub = (msErroPreco, msErroCadastral, msDecisaoJudicial,
             msErroTributacao, msDescontServico, msComplValores);

const
  TmotSubArrayStrings: array[TmotSub] of string = ('01', '02', '03', '04', '05',
    '06');

type
  TuMed = (umMinuto, umMB, umGB, umUN);

const
  TuMedArrayStrings: array[TuMed] of string = ('1', '2', '3', '4');

type
  TCSTIcms = (cst00, cst20, cst40, cst41, cst51, cst90, cstICMSSN);

const
  TCSTIcmsArrayStrings: array[TCSTIcms] of string = ('00', '20', '40', '41', '51',
    '90', 'SN');

type
  TCSTPis = (pis01, pis02, pis06, pis07, pis08, pis09, pis49);

const
  TCSTPisArrayStrings: array[TCSTPis] of string = ('01', '02', '06', '07', '08',
    '09', '49');

type
  TCSTCofins = (cof01, cof02, cof06, cof07, cof08, cof09, cof49);

const
  TCSTCofinsArrayStrings: array[TCSTCofins] of string = ('01', '02', '06', '07',
    '08', '09', '49');

type
  TtpProc = (tpSEFAZ, tpJusticaFederal, tpJusticaEstadual);

const
  TtpProcArrayStrings: array[TtpProc] of string = ('0', '1', '2');

type
  TtpRessarc = (tpCobrancaIndevida, tpInterrupcao, tpOutros);

const
  TtpRessarcArrayStrings: array[TtpRessarc] of string = ('1', '2', '99');

type
  // Futuramente deve ir para a unit ACBrDFeConversao
  TCRT = (crtSimplesNacional, crtSimplesExcessoReceita, crtRegimeNormal);

const
  // Futuramente deve ir para a unit ACBrDFeConversao
  TCRTArrayStrings: array[TCRT] of string = ('1', '2', '3');

// Reforma Tributaria
type
  TtpEnteGov  = (tcgUniao, tcgEstados, tcgDistritoFederal, tcgMunicipios);

const
  TtpEnteGovArrayStrings: array[TtpEnteGov] of string = ('1', '2', '3', '4');

type
  TCSTIBSCBS  = (cst000, cst010, cst011, cst200, cst210, cst220, cst221, cst400,
                 cst410, cst510, cst550, cst620, cst800, cst810, cst820);

const
  TCSTIBSCBSArrayStrings: array[TCSTIBSCBS] of string = ('000', '010', '011',
    '200', '210', '220', '221', '400', '410', '510', '550', '620', '800', '810',
    '820');

type
  TcClassTrib  = (ct000001, ct000002, ct000003, ct000004, ct010001, ct011001,
                  ct011002, ct011003, ct011004, ct011005, ct200001, ct200002,
                  ct200003, ct200004, ct200005, ct200006, ct200007, ct200008,
                  ct200009, ct200010, ct200011, ct200012, ct200013, ct200014,
                  ct200015, ct200016, ct200017, ct200018, ct200019, ct200020,
                  ct200021, ct200022, ct200023, ct200024, ct200025, ct200026,
                  ct200027, ct200028, ct200029, ct200030, ct200031, ct200032,
                  ct200033, ct200034, ct200035, ct200036, ct200037, ct200038,
                  ct200039, ct200040, ct200041, ct200042, ct200043, ct200044,
                  ct200045, ct200046, ct200047, ct200048, ct200049, ct200450,
                  ct200051, ct200052, ct210001, ct210002, ct210003, ct220001,
                  ct220002, ct220003, ct221001, ct400001, ct410001, ct410002,
                  ct410003, ct410004, ct410005, ct410006, ct410007, ct410008,
                  ct410009, ct410010, ct410011, ct410012, ct410013, ct410014,
                  ct410015, ct410016, ct410017, ct410018, ct410019, ct410020,
                  ct510001, ct510002, ct550001, ct550002, ct550003, ct550004,
                  ct550005, ct550006, ct550007, ct550008, ct550009, ct550010,
                  ct550011, ct550012, ct550013, ct550014, ct550015, ct550016,
                  ct550017, ct550018, ct550019, ct550020, ct620001, ct620002,
                  ct620003, ct620004, ct620005, ct800001, ct800002, ct810001,
                  ct820001, ct820002, ct820003, ct820004, ct820005);

const
  TcClassTribArrayStrings: array[TcClassTrib] of string = ('000001', '000002',
         '000003', '000004', '010001', '011001', '011002', '011003', '011004',
         '011005', '200001', '200002', '200003', '200004', '200005', '200006',
         '200007', '200008', '200009', '200010', '200011', '200012', '200013',
         '200014', '200015', '200016', '200017', '200018', '200019', '200020',
         '200021', '200022', '200023', '200024', '200025', '200026', '200027',
         '200028', '200029', '200030', '200031', '200032', '200033', '200034',
         '200035', '200036', '200037', '200038', '200039', '200040', '200041',
         '200042', '200043', '200044', '200045', '200046', '200047', '200048',
         '200049', '200450', '200051', '200052', '210001', '210002', '210003',
         '220001', '220002', '220003', '221001', '400001', '410001', '410002',
         '410003', '410004', '410005', '410006', '410007', '410008', '410009',
         '410010', '410011', '410012', '410013', '410014', '410015', '410016',
         '410017', '410018', '410019', '410020', '510001', '510002', '550001',
         '550002', '550003', '550004', '550005', '550006', '550007', '550008',
         '550009', '550010', '550011', '550012', '550013', '550014', '550015',
         '550016', '550017', '550018', '550019', '550020', '620001', '620002',
         '620003', '620004', '620005', '800001', '800002', '810001', '820001',
         '820002', '820003', '820004', '820005');

{
  Declaração das funções de conversão
}
function StrToTpEventoNFCom(out ok: boolean; const s: string): TpcnTpEvento;

function VersaoNFComToStr(const t: TVersaoNFCom): string;
function StrToVersaoNFCom(const s: string): TVersaoNFCom;

function DblToVersaoNFCom(const d: Double): TVersaoNFCom;
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

// Reforma Tributária
function tpEnteGovToStr(const t: TtpEnteGov): string;
function StrTotpEnteGov(const s: string): TtpEnteGov;

function CSTIBSCBSToStr(const t: TCSTIBSCBS): string;
function StrToCSTIBSCBS(const s: string): TCSTIBSCBS;

function cClassTribToStr(const t: TcClassTrib): string;
function StrTocClassTrib(const s: string): TcClassTrib;

implementation

uses
  typinfo,
  ACBrBase;

function StrToTpEventoNFCom(out ok: boolean; const s: string): TpcnTpEvento;
begin
  Result := StrToEnumerado(ok, s,
            ['-99999', '110111', '240140', '240150', '240170'],
            [teNaoMapeado, teCancelamento, teAutorizadoSubstituicao,
             teAutorizadoAjuste, teLiberacaoPrazoCancelado]);
end;

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

 function DblToVersaoNFCom(const d: Double): TVersaoNFCom;
var
  idx: TVersaoNFCom;
begin
  for idx := Low(TVersaoNFComArrayDouble) to High(TVersaoNFComArrayDouble) do
  begin
    if (TVersaoNFComArrayDouble[idx] = d) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TVersaoNFCom: %s',
    [FormatFloat('0.00', d)]);
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
  result := TVersaoQrCodeArrayDouble[t];
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

function tpEnteGovToStr(const t: TtpEnteGov): string;
begin
  Result := TtpEnteGovArrayStrings[t];
end;

function StrTotpEnteGov(const s: string): TtpEnteGov;
var
  idx: TtpEnteGov;
begin
  for idx:= Low(TtpEnteGovArrayStrings) to High(TtpEnteGovArrayStrings)do
  begin
    if(TtpEnteGovArrayStrings[idx] = s)then
    begin
      Result := idx;
      exit;
    end;
  end;
  raise EACBrException.CreateFmt('Valor string inválido para TtpEnteGov: %s', [s]);
end;

function CSTIBSCBSToStr(const t: TCSTIBSCBS): string;
begin
  Result := TCSTIBSCBSArrayStrings[t];
end;

function StrToCSTIBSCBS(const s: string): TCSTIBSCBS;
var
  idx: TCSTIBSCBS;
begin
  for idx:= Low(TCSTIBSCBSArrayStrings) to High(TCSTIBSCBSArrayStrings) do
  begin
    if(TCSTIBSCBSArrayStrings[idx] = s)then
    begin
      Result := idx;
      exit;
    end;
  end;
  raise EACBrException.CreateFmt('Valor string inválido para TCSTIBSCBS: %s', [s]);
end;

function cClassTribToStr(const t: TcClassTrib): string;
begin
  Result := TcClassTribArrayStrings[t];
end;

function StrTocClassTrib(const s: string): TcClassTrib;
var
  idx: TcClassTrib;
begin
  for idx:= Low(TcClassTribArrayStrings) to High(TcClassTribArrayStrings) do
  begin
    if(TcClassTribArrayStrings[idx] = s)then
    begin
      Result := idx;
      exit;
    end;
  end;
  raise EACBrException.CreateFmt('Valor string inválido para TcClassTrib: %s', [s]);
end;

initialization
  RegisterStrToTpEventoDFe(StrToTpEventoNFCom, 'NFCom');

end.

