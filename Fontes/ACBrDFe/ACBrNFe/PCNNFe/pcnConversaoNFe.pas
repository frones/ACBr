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

unit pcnConversaoNFe;

interface

uses
  SysUtils, StrUtils, Classes,
  pcnConversao;

type

  TStatusACBrNFe = (stIdle, stNFeStatusServico, stNFeRecepcao, stNFeRetRecepcao,
    stNFeConsulta, stNFeCancelamento, stNFeInutilizacao, stNFeRecibo,
    stNFeCadastro, stNFeEmail, stNFeCCe, stNFeEvento, stConsNFeDest,
    stDownloadNFe, stAdmCSCNFCe, stDistDFeInt, stEnvioWebService);

  TpcnInformacoesDePagamento = (eipNunca, eipAdicionais, eipQuadro);

  TpcnVersaoDF = (ve200, ve300, ve310, ve400);

const
  TVersaoDFArrayStrings: array[TpcnVersaoDF] of string = ('2.00', '3.00', '3.10',
    '4.00');
  TVersaoDFArrayDouble: array[TpcnVersaoDF] of Double = (2.00, 3.00, 3.10, 4.00);

type
  TSchemaNFe = (schErro, schNfe, schcancNFe, schInutNFe, schEnvCCe,
                schEnvEventoCancNFe, schEnvConfRecebto, schEnvEPEC,
                schconsReciNFe, schconsSitNFe, schconsStatServ, schconsCad,
                schenvEvento, schconsNFeDest, schdownloadNFe, schretEnviNFe,
                schadmCscNFCe, schdistDFeInt, scheventoEPEC, schCancSubst,
                schPedProrrog1, schPedProrrog2, schCanPedProrrog1,
                schCanPedProrrog2, schManifDestConfirmacao,
                schManifDestCiencia, schManifDestDesconhecimento,
                schManifDestOperNaoRealizada, schCompEntrega, schCancCompEntrega,
                schAtorInteressadoNFe);

const
  TSchemaNFeArrayStrings: array[TSchemaNFe] of string = ('Erro', 'Nfe',
    'cancNFe', 'InutNFe', 'EnvCCe', 'EnvEventoCancNFe', 'EnvConfRecebto',
    'EnvEPEC', 'consReciNFe', 'consSitNFe', 'consStatServ', 'consCad',
    'envEvento', 'consNFeDest', 'downloadNFe', 'retEnviNFe', 'admCscNFCe',
    'distDFeInt', 'eventoEPEC', 'CancSubst', 'PedProrrog1', 'PedProrrog2',
    'CanPedProrrog1', 'CanPedProrrog2', 'ManifDestConfirmacao',
    'ManifDestCiencia', 'ManifDestDesconhecimento', 'ManifDestOperNaoRealizada',
    'CompEntrega', 'CancCompEntrega', 'AtorInteressadoNFe');

  TEventoArrayStrings: array[TSchemaNFe] of string = ('', '', 'e110111', '',
    'e110110', '', '', 'e110140', '', '', '', '', '', '', '', '', '', '', '',
    'e110112', 'e111500', 'e111501', 'e111502', 'e111503', 'e210200', 'e210210',
    'e210220', 'e210240', 'e110130', 'e110131', 'e110150');

type
  TLayOut = (LayNfeRecepcao, LayNfeRetRecepcao, LayNfeCancelamento,
    LayNfeInutilizacao, LayNfeConsulta, LayNfeStatusServico,
    LayNfeCadastro, LayNFeCCe, LayNFeEvento, LayNFeEventoAN, LayNFeConsNFeDest,
    LayNFeDownloadNFe, LayNfeAutorizacao, LayNfeRetAutorizacao,
    LayAdministrarCSCNFCe, LayDistDFeInt, LayNFCeEPEC, LayNFeURLQRCode,
    LayURLConsultaNFe, LayURLConsultaNFCe);

const
  TLayOutArrayStrings: array[TLayOut] of string = ('NfeRecepcao',
    'NfeRetRecepcao', 'NfeCancelamento', 'NfeInutilizacao',
    'NfeConsultaProtocolo', 'NfeStatusServico', 'NfeConsultaCadastro',
    'RecepcaoEvento', 'RecepcaoEvento', 'RecepcaoEvento', 'NfeConsultaDest',
    'NfeDownloadNF', 'NfeAutorizacao', 'NfeRetAutorizacao', 'AdministrarCSCNFCe',
    'NFeDistribuicaoDFe', 'EventoEPEC', 'URL-QRCode', 'URL-ConsultaNFe',
    'URL-ConsultaNFCe');

type
  TpcnFinalidadeNFe = (fnNormal, fnComplementar, fnAjuste, fnDevolucao);

const
  TFinalidadeNFeArrayStrings: array[TpcnFinalidadeNFe] of string = ('1', '2', '3',
    '4');

type
  TpcnModeloDF = (moNFe, moNFCe);

const
  TModeloDFArrayStrings: array[TpcnModeloDF] of string = ('55', '65');

type
  TpcnIndicadorNFe = (inTodas, inSemManifestacaoComCiencia,
                      inSemManifestacaoSemCiencia);

const
  TIndicadorNFeArrayStrings: array[TpcnIndicadorNFe] of string = ('0', '1', '2');

type
  TpcnVersaoQrCode = (veqr000, veqr100, veqr200);

const
  TVersaoQrCodeArrayStrings: array[TpcnVersaoQrCode] of string = ('0', '1', '2');
  TVersaoQrCodeArrayDouble: array[TpcnVersaoQrCode] of Double = (0, 1.00, 2.00);

type
  TpcnTipoOperacao = (toVendaConcessionaria, toFaturamentoDireto, toVendaDireta,
                      toOutros);

const
  TTipoOperacaoArrayStrings: array[TpcnTipoOperacao] of string = ('1', '2', '3',
    '0');
  TTipoOperacaoDescArrayStrings: array[TpcnTipoOperacao]
    of string = ('1-VENDA CONCESSIONÁRIA', '2-FAT. DIRETO CONS. FINAL',
      '3-VENDA DIRETA', '0-OUTROS');

type
  TpcnCondicaoVeiculo = (cvAcabado, cvInacabado, cvSemiAcabado);

const
  TCondicaoVeiculoArrayStrings: array[TpcnCondicaoVeiculo] of string = ('1', '2',
    '3');
  TCondicaoVeiculoDescArrayStrings: array[TpcnCondicaoVeiculo]
     of string = ('1-ACABADO', '2-INACABADO', '3-SEMI-ACABADO');

type
  TpcnTipoArma = (taUsoPermitido, taUsoRestrito);

const
  TTipoArmaArrayStrings: array[TpcnTipoArma] of string = ('0', '1');
  TTipoArmaDescArrayStrings: array[TpcnTipoArma] of string = ('0-USO PERMITIDO',
    '1-USO RESTRITO');

type
  TpcnIndEscala = (ieRelevante, ieNaoRelevante, ieNenhum);

const
  TIndEscalaArrayStrings: array[TpcnIndEscala] of string = ('S', 'N', '');

type
  TpcnModalidadeFrete = (mfContaEmitente, mfContaDestinatario, mfContaTerceiros,
                         mfProprioRemetente, mfProprioDestinatario, mfSemFrete);

const
  TModalidadeFreteArrayStrings: array[TpcnModalidadeFrete] of string = ('0', '1',
    '2', '3', '4', '9');

type
  TAutorizacao = (taNaoPermite, taPermite);

const
  TAutorizacaoArrayStrings: array[TAutorizacao] of string = ('0', '1');

type
  TindIntermed = (iiSemOperacao, iiOperacaoSemIntermediador,
                  iiOperacaoComIntermediador);

const
  TindIntermedArrayStrings: array[TindIntermed] of string = ('', '0', '1');

type
  TtpAto = (taNenhum, taTermoAcordo, taRegimeEspecial, taAutorizacaoEspecifica,
            taAjusteSNIEF, taConvenioICMS);

const
  TtpAtoArrayStrings: array[TtpAto] of string = ('', '08', '10', '12', '14',
    '15');

type
  TindImport = (iiNacional, iiImportado);

const
  TindImportArrayStrings: array[TindImport] of string = ('0', '1');

type
  TmotRedAdRem = (motTranspColetivo, motOutros);

const
  TmotRedAdRemArrayStrings: array[TmotRedAdRem] of string = ('1', '9');

{
  Declaração das funções de conversão
}
function StrToTpEventoNFe(out ok: boolean; const s: string): TpcnTpEvento;

function LayOutToServico(const t: TLayOut): string;
function ServicoToLayOut(out ok: Boolean; const s: string): TLayOut;

function LayOutToSchema(const t: TLayOut): TSchemaNFe;

function SchemaNFeToStr(const t: TSchemaNFe): string;
function StrToSchemaNFe(const s: string): TSchemaNFe;
function SchemaEventoToStr(const t: TSchemaNFe): string;

function FinNFeToStr(const t: TpcnFinalidadeNFe): string;
function StrToFinNFe(out ok: Boolean; const s: string): TpcnFinalidadeNFe;

function IndicadorNFeToStr(const t: TpcnIndicadorNFe): string;
function StrToIndicadorNFe(out ok: Boolean; const s: string): TpcnIndicadorNFe;

function VersaoQrCodeToStr(const t: TpcnVersaoQrCode): string;
function StrToVersaoQrCode(out ok: Boolean; const s: string): TpcnVersaoQrCode;
function VersaoQrCodeToDbl(const t: TpcnVersaoQrCode): Double;

function ModeloDFToStr(const t: TpcnModeloDF): string;
function StrToModeloDF(out ok: Boolean; const s: string): TpcnModeloDF;
function ModeloDFToPrefixo(const t: TpcnModeloDF): string;

function StrToVersaoDF(out ok: Boolean; const s: string): TpcnVersaoDF;
function VersaoDFToStr(const t: TpcnVersaoDF): string;

function DblToVersaoDF(out ok: Boolean; const d: Double): TpcnVersaoDF;
function VersaoDFToDbl(const t: TpcnVersaoDF): Double;

function tpOPToStr(const t: TpcnTipoOperacao): string;
function StrTotpOP(out ok: boolean; const s: string): TpcnTipoOperacao;

function condVeicToStr(const t: TpcnCondicaoVeiculo): string;
function StrTocondVeic(out ok: boolean; const s: string): TpcnCondicaoVeiculo;

function tpArmaToStr(const t: TpcnTipoArma): string;
function StrTotpArma(out ok: boolean; const s: string): TpcnTipoArma;

function VeiculosRestricaoStr( const iRestricao :Integer ): string;
function VeiculosCorDENATRANStr( const sCorDENATRAN : string ): string;
function VeiculosCondicaoStr( const condVeic: TpcnCondicaoVeiculo ): string;
function VeiculosVinStr( const sVin: string ): string;
function VeiculosEspecieStr( const iEspecie : Integer ): string;
function VeiculosTipoStr( const iTipoVeic : Integer ): string;
function VeiculosCombustivelStr( const sTpComb : string ): string;
function VeiculosTipoOperStr( const TtpOP : TpcnTipoOperacao ): string;

function ArmaTipoStr( const TtpArma : TpcnTipoArma ): string;

function IndEscalaToStr(const t: TpcnIndEscala): string;
function StrToIndEscala(out ok: Boolean; const s: string): TpcnIndEscala;
function modFreteToStr(const t: TpcnModalidadeFrete): string;
function StrTomodFrete(out ok: boolean; const s: string): TpcnModalidadeFrete;
function modFreteToDesStr(const t: TpcnModalidadeFrete; versao: TpcnVersaoDF): string;

function AutorizacaoToStr(const t: TAutorizacao): string;
function StrToAutorizacao(out ok: boolean; const s: string): TAutorizacao;

function IndIntermedToStr(const t: TindIntermed): string;
function StrToIndIntermed(out ok: boolean; const s: string): TindIntermed;

function tpAtoToStr(const t: TtpAto): string;
function StrTotpAto(out ok: boolean; const s: string): TtpAto;

function indImportToStr(const t: TindImport): string;
function StrToindImport(out ok: boolean; const s: string): TindImport;

function motRedAdRemToStr(const t: TmotRedAdRem): string;
function StrTomotRedAdRem(out ok: boolean; const s: string): TmotRedAdRem;

implementation

uses
  typinfo,
  ACBrBase;

function StrToTpEventoNFe(out ok: boolean; const s: string): TpcnTpEvento;
begin
  Result := StrToEnumerado(ok, s,
            ['-99999', '110110', '110111', '110112', '110140', '111500',
             '111501', '111502', '111503', '210200', '210210', '210220',
             '210240', '610600', '610614', '790700', '990900', '990910',
             '110180', '610554', '610510', '610615', '610610', '110130',
             '110131', '110150', '610130', '610131', '610601'],
            [teNaoMapeado, teCCe, teCancelamento, teCancSubst, teEPECNFe,
             tePedProrrog1, tePedProrrog2, teCanPedProrrog1, teCanPedProrrog2,
             teManifDestConfirmacao, teManifDestCiencia,
             teManifDestDesconhecimento, teManifDestOperNaoRealizada,
             teRegistroCTe, teMDFeAutorizadoComCTe, teAverbacaoExportacao,
             teVistoriaSuframa, teConfInternalizacao, teComprEntrega,
             teRegPasAutMDFeComCte, teRegPasNfeProMDFe,
             teCancelamentoMDFeAutComCTe, teMDFeAutorizado,
             teComprEntregaNFe, teCancComprEntregaNFe, teAtorInteressadoNFe,
             teComprEntregaCTe, teCancComprEntregaCTe, teCTeCancelado]);
end;

function SchemaEventoToStr(const t: TSchemaNFe): string;
begin
  result := TEventoArrayStrings[t];
end;

function LayOutToServico(const t: TLayOut): string;
begin
  result := TLayOutArrayStrings[t];
end;

function ServicoToLayOut(out ok: Boolean; const s: string): TLayOut;
var
  idx: TLayOut;
begin
  ok := True;

  for idx := Low(TLayOutArrayStrings) to High(TLayOutArrayStrings) do
  begin
    if (TLayOutArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TLayOut: %s', [s]);
end;

function LayOutToSchema(const t: TLayOut): TSchemaNFe;
begin
  case t of
    LayNfeRecepcao:       Result := schNfe;
    LayNfeRetRecepcao:    Result := schconsReciNFe;
    LayNfeCancelamento:   Result := schcancNFe;
    LayNfeInutilizacao:   Result := schInutNFe;
    LayNfeConsulta:       Result := schconsSitNFe;
    LayNfeStatusServico:  Result := schconsStatServ;
    LayNfeCadastro:       Result := schconsCad;
    LayNFeCCe,
    LayNFeEvento,
    LayNFeEventoAN:       Result := schenvEvento;
    LayNFeConsNFeDest:    Result := schconsNFeDest;
    LayNFeDownloadNFe:    Result := schdownloadNFe;
    LayNfeAutorizacao:    Result := schNfe;
    LayNfeRetAutorizacao: Result := schretEnviNFe;
    LayAdministrarCSCNFCe: Result := schadmCscNFCe;
    LayDistDFeInt:        Result := schdistDFeInt;
    LayNFCeEPEC:          Result := scheventoEPEC;
  else
    Result := schErro;
  end;
end;

function SchemaNFeToStr(const t: TSchemaNFe): string;
begin
  Result := GetEnumName(TypeInfo(TSchemaNFe), Integer( t ) );
  Result := copy(Result, 4, Length(Result)); // Remove prefixo "sch"
end;

function StrToSchemaNFe(const s: string): TSchemaNFe;
var
  P: Integer;
  SchemaStr: string;
  CodSchema: Integer;
begin
  P := pos('_',s);
  if p > 0 then
    SchemaStr := copy(s,1,P-1)
  else
    SchemaStr := s;

  if LeftStr(SchemaStr,3) <> 'sch' then
    SchemaStr := 'sch'+SchemaStr;

  CodSchema := GetEnumValue(TypeInfo(TSchemaNFe), SchemaStr );

  if CodSchema = -1 then
  begin
    raise Exception.Create(Format('"%s" não é um valor TSchemaNFe válido.',[SchemaStr]));
  end;

  Result := TSchemaNFe( CodSchema );
end;

// B25 - Finalidade de emissão da NF-e *****************************************
function FinNFeToStr(const t: TpcnFinalidadeNFe): string;
begin
  result := TFinalidadeNFeArrayStrings[t];
end;

function StrToFinNFe(out ok: Boolean; const s: string): TpcnFinalidadeNFe;
var
  idx: TpcnFinalidadeNFe;
begin
  ok := True;

  for idx := Low(TFinalidadeNFeArrayStrings) to High(TFinalidadeNFeArrayStrings) do
  begin
    if (TFinalidadeNFeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcnFinalidadeNFe: %s', [s]);
end;

function IndicadorNFeToStr(const t: TpcnIndicadorNFe): string;
begin
  result := TIndicadorNFeArrayStrings[t];
end;

function StrToIndicadorNFe(out ok: Boolean; const s: string): TpcnIndicadorNFe;
var
  idx: TpcnIndicadorNFe;
begin
  ok := True;

  for idx := Low(TIndicadorNFeArrayStrings) to High(TIndicadorNFeArrayStrings) do
  begin
    if (TIndicadorNFeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcnIndicadorNFe: %s', [s]);
end;

function VersaoQrCodeToStr(const t: TpcnVersaoQrCode): string;
begin
  result := TVersaoQrCodeArrayStrings[t];
end;

function StrToVersaoQrCode(out ok: Boolean; const s: string): TpcnVersaoQrCode;
var
  idx: TpcnVersaoQrCode;
begin
  ok := True;

  for idx := Low(TVersaoQrCodeArrayStrings) to High(TVersaoQrCodeArrayStrings) do
  begin
    if (TVersaoQrCodeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcnVersaoQrCode: %s', [s]);
end;

function VersaoQrCodeToDbl(const t: TpcnVersaoQrCode): Double;
begin
   result := TVersaoQrCodeArrayDouble[t];
end;

function ModeloDFToStr(const t: TpcnModeloDF): string;
begin
  result := TModeloDFArrayStrings[t];
end;

function StrToModeloDF(out ok: Boolean; const s: string): TpcnModeloDF;
var
  idx: TpcnModeloDF;
begin
  ok := True;

  for idx := Low(TModeloDFArrayStrings) to High(TModeloDFArrayStrings) do
  begin
    if (TModeloDFArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcnModeloDF: %s', [s]);
end;

function ModeloDFToPrefixo(const t: TpcnModeloDF): string;
begin
  Case t of
    moNFCe: Result := 'NFCe';
  else
    Result := 'NFe';
  end;
end;

function StrToVersaoDF(out ok: Boolean; const s: string): TpcnVersaoDF;
var
  idx: TpcnVersaoDF;
begin
  ok := True;

  for idx := Low(TVersaoDFArrayStrings) to High(TVersaoDFArrayStrings) do
  begin
    if (TVersaoDFArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcnVersaoDF: %s', [s]);
end;

function VersaoDFToStr(const t: TpcnVersaoDF): string;
begin
  result := TVersaoDFArrayStrings[t];
end;

 function DblToVersaoDF(out ok: Boolean; const d: Double): TpcnVersaoDF;
var
  idx: TpcnVersaoDF;
begin
  ok := True;

  for idx := Low(TVersaoDFArrayDouble) to High(TVersaoDFArrayDouble) do
  begin
    if (TVersaoDFArrayDouble[idx] = d) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcnVersaoDF: %s',
    [FormatFloat('0.00', d)]);
 end;

 function VersaoDFToDbl(const t: TpcnVersaoDF): Double;
 begin
   result := TVersaoDFArrayDouble[t];
 end;

// J02 - Tipo da operação ******************************************************
 function tpOPToStr(const t: TpcnTipoOperacao): string;
begin
  result := TTipoOperacaoArrayStrings[t];
end;

function StrTotpOP(out ok: boolean; const s: string): TpcnTipoOperacao;
var
  idx: TpcnTipoOperacao;
begin
  ok := True;

  for idx := Low(TTipoOperacaoArrayStrings) to High(TTipoOperacaoArrayStrings) do
  begin
    if (TTipoOperacaoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcnTipoOperacao: %s', [s]);
end;

// J22 - Condição do Veículo ***************************************************
function condVeicToStr(const t: TpcnCondicaoVeiculo): string;
begin
  result := TCondicaoVeiculoArrayStrings[t];
end;

function StrTocondVeic(out ok: boolean; const s: string): TpcnCondicaoVeiculo;
var
  idx: TpcnCondicaoVeiculo;
begin
  ok := True;

  for idx := Low(TCondicaoVeiculoArrayStrings) to High(TCondicaoVeiculoArrayStrings) do
  begin
    if (TCondicaoVeiculoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcnCondicaoVeiculo: %s', [s]);
end;

// L02 - Indicador do tipo de arma de fogo *************************************
function tpArmaToStr(const t: TpcnTipoArma): string;
begin
  result := TTipoArmaArrayStrings[t];
end;

function StrTotpArma(out ok: boolean; const s: string): TpcnTipoArma;
var
  idx: TpcnTipoArma;
begin
  ok := True;

  for idx := Low(TTipoArmaArrayStrings) to High(TTipoArmaArrayStrings) do
  begin
    if (TTipoArmaArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcnTipoArma: %s', [s]);
end;

function VeiculosRestricaoStr( const iRestricao : Integer ): string;
begin
  case iRestricao of
    0: result := '0-NÃO HÁ';
    1: result := '1-ALIENAÇÃO FIDUCIÁRIA';
    2: result := '2-RESERVA DE DOMICÍLIO';
    3: result := '3-RESERVA DE DOMÍNIO';
    4: result := '4-PENHOR DE VEÍCULOS';
    9: result := '9-OUTRAS'
    else
      result := IntToStr(iRestricao)+ 'NÃO DEFINIDO' ;
  end;
end;

function VeiculosCorDENATRANStr( const sCorDENATRAN : string ): string;
begin
  case StrToIntDef( sCorDENATRAN, 0 ) of
     1: result := '01-AMARELO';
     2: result := '02-AZUL';
     3: result := '03-BEGE';
     4: result := '04-BRANCA';
     5: result := '05-CINZA';
     6: result := '06-DOURADA';
     7: result := '07-GRENÁ';
     8: result := '08-LARANJA';
     9: result := '09-MARROM';
    10: result := '10-PRATA';
    11: result := '11-PRETA';
    12: result := '12-ROSA';
    13: result := '13-ROXA';
    14: result := '14-VERDE';
    15: result := '15-VERMELHA';
    16: result := '16-FANTASIA'
    else
      result := sCorDENATRAN + 'NÃO DEFINIDA' ;
  end;
end;

function VeiculosCondicaoStr( const condVeic: TpcnCondicaoVeiculo ): string;
begin
  result := TCondicaoVeiculoDescArrayStrings[condVeic];
end;

function VeiculosVinStr( const sVin: string ): string;
begin
  // Enumerar Vim no futuro ?
  if sVIN = 'R' then
      result := 'R-REMARCADO'
  else
    if sVIN = 'N' then
      result:= 'N-NORMAL'
    else
      result := 'NÃO DEFINIDA' ;
end;

function VeiculosEspecieStr( const iEspecie : Integer ): string;
begin
  case iEspecie of
    1: result := '01-PASSAGEIRO';
    2: result := '02-CARGA';
    3: result := '03-MISTO';
    4: result := '04-CORRIDA';
    5: result := '05-TRAÇÃO';
    6: result := '06-ESPECIAL';
    7: result := '07-COLEÇÃO'
    else
      result := IntToStr(iEspecie ) + 'NÃO DEFINIDA' ;
    end;
end;

function VeiculosTipoStr( const iTipoVeic : Integer ): string;
begin
  case iTipoVeic of
     1: result := '01-BICICLETA';
     2: result := '02-CICLOMOTOR';
     3: result := '03-MOTONETA';
     4: result := '04-MOTOCICLETA';
     5: result := '05-TRICICLO';
     6: result := '06-AUTOMÓVEL';
     7: result := '07-MICROONIBUS';
     8: result := '08-ONIBUS';
     9: result := '09-BONDE';
    10: result := '10-REBOQUE';
    11: result := '11-SEMI-REBOQUE';
    12: result := '12-CHARRETE';
    13: result := '13-CAMIONETA';
    14: result := '14-CAMINHÃO';
    15: result := '15-CARROÇA';
    16: result := '16-CARRO DE MÃO';
    17: result := '17-CAMINHÃO TRATOR';
    18: result := '18-TRATOR DE RODAS';
    19: result := '19-TRATOR DE ESTEIRAS';
    20: result := '20-TRATOR MISTO';
    21: result := '21-QUADRICICLO';
    22: result := '22-CHASSI/PLATAFORMA';
    23: result := '23-CAMINHONETE';
    24: result := '24-SIDE-CAR';
    25: result := '25-UTILITÁRIO';
    26: result := '26-MOTOR-CASA'
    else
      result := IntToStr(iTipoVeic)+'NÃO DEFINIDO' ;
    end;
end;

function VeiculosCombustivelStr( const sTpComb : string ): string;
begin
  case StrToIntDef( stpComb, 0) of
     1: result := '01-ÁLCOOL';
     2: result := '02-GASOLINA';
     3: result := '03-DIESEL';
     4: result := '04-GASOGÊNIO';
     5: result := '05-GÁS METANO';
     6: result := '06-ELETRICO/F INTERNA';
     7: result := '07-ELETRICO/F EXTERNA';
     8: result := '08-GASOLINA/GNC';
     9: result := '09-ÁLCOOL/GNC';
    10: result := '10-DIESEL / GNC';
    11: result := '11-VIDE CAMPO OBSERVAÇÃO';
    12: result := '12-ÁLCOOL/GNV';
    13: result := '13-GASOLINA/GNV';
    14: result := '14-DIESEL/GNV';
    15: result := '15-GÁS NATURAL VEICULAR';
    16: result := '16-ÁLCOOL/GASOLINA';
    17: result := '17-GASOLINA/ÁLCOOL/GNV';
    18: result := '18-GASOLINA/ELÉTRICO'
    else
      result := stpComb +'NÃO DEFINIDO' ;
    end;
end;

function VeiculosTipoOperStr( const TtpOP : TpcnTipoOperacao ): string;
begin
  result := TTipoOperacaoDescArrayStrings[TtpOP];
end;

function ArmaTipoStr( const TtpArma : TpcnTipoArma ): string;
begin
  result := TTipoArmaDescArrayStrings[TtpArma];
end;

function IndEscalaToStr(const t: TpcnIndEscala): string;
begin
  result := TIndEscalaArrayStrings[t];
end;

function StrToIndEscala(out ok: Boolean; const s: string): TpcnIndEscala;
var
  idx: TpcnIndEscala;
begin
  ok := True;

  for idx := Low(TIndEscalaArrayStrings) to High(TIndEscalaArrayStrings) do
  begin
    if (TIndEscalaArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcnIndEscala: %s', [s]);
end;

function modFreteToStr(const t: TpcnModalidadeFrete): string;
begin
  result := TModalidadeFreteArrayStrings[t];
end;

function StrTomodFrete(out ok: boolean; const s: string): TpcnModalidadeFrete;
var
  idx: TpcnModalidadeFrete;
begin
  ok := True;

  for idx := Low(TModalidadeFreteArrayStrings) to High(TModalidadeFreteArrayStrings) do
  begin
    if (TModalidadeFreteArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TpcnModalidadeFrete: %s', [s]);
end;

function modFreteToDesStr(const t: TpcnModalidadeFrete; versao: TpcnVersaoDF): string;
begin
  case versao of
    ve200,
    ve300,
    ve310:
      case t  of
        mfContaEmitente       : result := '0 - EMITENTE';
        mfContaDestinatario   : result := '1 - DEST/REM';
        mfContaTerceiros      : result := '2 - TERCEIROS';
        mfProprioRemetente    : result := '3 - PROP/REMT';
        mfProprioDestinatario : result := '4 - PROP/DEST';
        mfSemFrete            : result := '9 - SEM FRETE';
      end;
    ve400:
      case t  of
        mfContaEmitente       : result := '0 - REMETENTE';
        mfContaDestinatario   : result := '1 - DESTINATARIO';
        mfContaTerceiros      : result := '2 - TERCEIROS';
        mfProprioRemetente    : result := '3 - PROP/REMT';
        mfProprioDestinatario : result := '4 - PROP/DEST';
        mfSemFrete            : result := '9 - SEM FRETE';
      end;
  end;
end;

function AutorizacaoToStr(const t: TAutorizacao): string;
begin
  result := TAutorizacaoArrayStrings[t];
end;

function StrToAutorizacao(out ok: boolean; const s: string): TAutorizacao;
var
  idx: TAutorizacao;
begin
  ok := True;

  for idx := Low(TAutorizacaoArrayStrings) to High(TAutorizacaoArrayStrings) do
  begin
    if (TAutorizacaoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TAutorizacao: %s', [s]);
end;

function IndIntermedToStr(const t: TindIntermed): string;
begin
  result := TindIntermedArrayStrings[t];
end;

function StrToIndIntermed(out ok: boolean; const s: string): TindIntermed;
var
  idx: TindIntermed;
begin
  ok := True;

  for idx := Low(TindIntermedArrayStrings) to High(TindIntermedArrayStrings) do
  begin
    if (TindIntermedArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TindIntermed: %s', [s]);
end;

function tpAtoToStr(const t: TtpAto): string;
begin
  result := TtpAtoArrayStrings[t];
end;

function StrTotpAto(out ok: boolean; const s: string): TtpAto;
var
  idx: TtpAto;
begin
  ok := True;

  for idx := Low(TtpAtoArrayStrings) to High(TtpAtoArrayStrings) do
  begin
    if (TtpAtoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpAto: %s', [s]);
end;

function indImportToStr(const t: TindImport): string;
begin
  result := TindImportArrayStrings[t];
end;

function StrToindImport(out ok: boolean; const s: string): TindImport;
var
  idx: TindImport;
begin
  ok := True;

  for idx := Low(TindImportArrayStrings) to High(TindImportArrayStrings) do
  begin
    if (TindImportArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TindImport: %s', [s]);
end;

function motRedAdRemToStr(const t: TmotRedAdRem): string;
begin
  result := TmotRedAdRemArrayStrings[t];
end;

function StrTomotRedAdRem(out ok: boolean; const s: string): TmotRedAdRem;
var
  idx: TmotRedAdRem;
begin
  ok := True;

  for idx := Low(TmotRedAdRemArrayStrings) to High(TmotRedAdRemArrayStrings) do
  begin
    if (TmotRedAdRemArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TmotRedAdRem: %s', [s]);
end;

initialization
  RegisterStrToTpEventoDFe(StrToTpEventoNFe, 'NFe');

end.

