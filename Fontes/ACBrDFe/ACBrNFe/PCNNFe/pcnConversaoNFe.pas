////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar NFe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da NFe          //
//                                                                            //
//        site: www.projetocooperar.org/nfe                                   //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_nfe/        //
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
//              "PCN  -  Projeto  Cooperar  NFe", não  podendo o mesmo ser    //
//              utilizado sem previa autorização.                             //
//                                                                            //
//    Nota (2): - O uso integral (ou parcial) das units do projeto esta       //
//              condicionado a manutenção deste cabeçalho junto ao código     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

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
                schAtorInteressadoNFe, schInsucessoEntregaNFe,
                schCancInsucessoEntregaNFe, schConcFinanceira,
                schCancConcFinanceira);

const
  TSchemaNFeArrayStrings: array[TSchemaNFe] of string = ('Erro', 'Nfe',
    'cancNFe', 'InutNFe', 'EnvCCe', 'EnvEventoCancNFe', 'EnvConfRecebto',
    'EnvEPEC', 'consReciNFe', 'consSitNFe', 'consStatServ', 'consCad',
    'envEvento', 'consNFeDest', 'downloadNFe', 'retEnviNFe', 'admCscNFCe',
    'distDFeInt', 'eventoEPEC', 'CancSubst', 'PedProrrog1', 'PedProrrog2',
    'CanPedProrrog1', 'CanPedProrrog2', 'ManifDestConfirmacao',
    'ManifDestCiencia', 'ManifDestDesconhecimento', 'ManifDestOperNaoRealizada',
    'CompEntrega', 'CancCompEntrega', 'AtorInteressadoNFe',
    'InsucessoEntrega', 'CancInsucessoEntrega', 'ConcFinanceira',
    'CancConcFinanceira');

  TEventoArrayStrings: array[TSchemaNFe] of string = ('', '', 'e110111', '',
    'e110110', '', '', 'e110140', '', '', '', '', '', '', '', '', '', '', '',
    'e110112', 'e111500', 'e111501', 'e111502', 'e111503', 'e210200', 'e210210',
    'e210220', 'e210240', 'e110130', 'e110131', 'e110150', 'e110192', 'e110193',
    'e110750', 'e110751');

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
  TAutorizacao = (taNaoPermite, taPermite, taNaoInformar);

const
  TAutorizacaoArrayStrings: array[TAutorizacao] of string = ('0', '1', '');

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

type
  TtpMotivo = (tmNaoEncontrado, tmRecusa, tmInexistente, tmOutro);

const
  TtpMotivoArrayStrings: array[TtpMotivo] of string = ('1', '2', '3', '4');

{
  Declaração das funções de conversão
}
function StrToTpEventoNFe(out ok: boolean; const s: string): TpcnTpEvento;

function LayOutToServico(const t: TLayOut): String;
function ServicoToLayOut(out ok: Boolean; const s: String): TLayOut;

function LayOutToSchema(const t: TLayOut): TSchemaNFe;

function SchemaNFeToStr(const t: TSchemaNFe): String;
function StrToSchemaNFe(const s: String): TSchemaNFe;
function SchemaEventoToStr(const t: TSchemaNFe): String;

function FinNFeToStr(const t: TpcnFinalidadeNFe): String;
function StrToFinNFe(out ok: Boolean; const s: String): TpcnFinalidadeNFe;

function IndicadorNFeToStr(const t: TpcnIndicadorNFe): String;
function StrToIndicadorNFe(out ok: Boolean; const s: String): TpcnIndicadorNFe;

function VersaoQrCodeToStr(const t: TpcnVersaoQrCode): String;
function StrToVersaoQrCode(out ok: Boolean; const s: String): TpcnVersaoQrCode;
function VersaoQrCodeToDbl(const t: TpcnVersaoQrCode): Real;

function ModeloDFToStr(const t: TpcnModeloDF): String;
function StrToModeloDF(out ok: Boolean; const s: String): TpcnModeloDF;
function ModeloDFToPrefixo(const t: TpcnModeloDF): String;

function StrToVersaoDF(out ok: Boolean; const s: String): TpcnVersaoDF;
function VersaoDFToStr(const t: TpcnVersaoDF): String;

function DblToVersaoDF(out ok: Boolean; const d: Real): TpcnVersaoDF;
function VersaoDFToDbl(const t: TpcnVersaoDF): Real;

function tpOPToStr(const t: TpcnTipoOperacao): string;
function StrTotpOP(out ok: boolean; const s: string): TpcnTipoOperacao;

function condVeicToStr(const t: TpcnCondicaoVeiculo): string;
function StrTocondVeic(out ok: boolean; const s: string): TpcnCondicaoVeiculo;

function tpArmaToStr(const t: TpcnTipoArma): string;
function StrTotpArma(out ok: boolean; const s: string): TpcnTipoArma;

function VeiculosRestricaoStr( const iRestricao :Integer ): String;
function VeiculosCorDENATRANStr( const sCorDENATRAN : String ): String;
function VeiculosCondicaoStr( const condVeic: TpcnCondicaoVeiculo ): String;
function VeiculosVinStr( const sVin: String ): String;
function VeiculosEspecieStr( const iEspecie : Integer ): String;
function VeiculosTipoStr( const iTipoVeic : Integer ): String;
function VeiculosCombustivelStr( const sTpComb : String ): String;
function VeiculosTipoOperStr( const TtpOP : TpcnTipoOperacao ): String;

function ArmaTipoStr( const TtpArma : TpcnTipoArma ): String;

function IndEscalaToStr(const t: TpcnIndEscala): String;
function StrToIndEscala(out ok: Boolean; const s: String): TpcnIndEscala;
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

function tpMotivoToStr(const t: TtpMotivo): string;
function StrTotpMotivo(out ok: boolean; const s: string): TtpMotivo;

implementation

uses
  typinfo;

function StrToTpEventoNFe(out ok: boolean; const s: string): TpcnTpEvento;
begin
  Result := StrToEnumerado(ok, s,
            ['-99999', '110110', '110111', '110112', '110140', '111500',
             '111501', '111502', '111503', '210200', '210210', '210220',
             '210240', '610600', '610614', '790700', '990900', '990910',
             '110180', '610554', '610510', '610615', '610610', '110130',
             '110131', '110150', '610130', '610131', '610601',
             '110192', '110193', '610514', '610500'],
            [teNaoMapeado, teCCe, teCancelamento, teCancSubst, teEPECNFe,
             tePedProrrog1, tePedProrrog2, teCanPedProrrog1, teCanPedProrrog2,
             teManifDestConfirmacao, teManifDestCiencia,
             teManifDestDesconhecimento, teManifDestOperNaoRealizada,
             teRegistroCTe, teMDFeAutorizadoComCTe, teAverbacaoExportacao,
             teVistoriaSuframa, teConfInternalizacao, teComprEntrega,
             teRegPasAutMDFeComCte, teRegPasNfeProMDFe,
             teCancelamentoMDFeAutComCTe, teMDFeAutorizado,
             teComprEntregaNFe, teCancComprEntregaNFe, teAtorInteressadoNFe,
             teComprEntregaCTe, teCancComprEntregaCTe, teCTeCancelado,
             teInsucessoEntregaNFe, teCancInsucessoEntregaNFe,
             teRegPasNfeProMDFeCte, teRegistroPassagemNFe]);
end;

function LayOutToServico(const t: TLayOut): String;
begin
  Result := EnumeradoToStr(t,
    ['NfeRecepcao', 'NfeRetRecepcao', 'NfeCancelamento', 'NfeInutilizacao',
     'NfeConsultaProtocolo', 'NfeStatusServico', 'NfeConsultaCadastro',
     'RecepcaoEvento', 'RecepcaoEvento', 'RecepcaoEvento', 'NfeConsultaDest',
     'NfeDownloadNF', 'NfeAutorizacao', 'NfeRetAutorizacao', 'AdministrarCSCNFCe',
     'NFeDistribuicaoDFe', 'EventoEPEC'],
    [ LayNfeRecepcao, LayNfeRetRecepcao, LayNfeCancelamento, LayNfeInutilizacao,
      LayNfeConsulta, LayNfeStatusServico, LayNfeCadastro,
      LayNFeCCe, LayNFeEvento, LayNFeEventoAN, LayNFeConsNFeDest,
      LayNFeDownloadNFe, LayNfeAutorizacao, LayNfeRetAutorizacao,
      LayAdministrarCSCNFCe, LayDistDFeInt, LayNFCeEPEC ] );
end;

function ServicoToLayOut(out ok: Boolean; const s: String): TLayOut;
begin
  Result := StrToEnumerado(ok, s,
  ['NfeRecepcao', 'NfeRetRecepcao', 'NfeCancelamento', 'NfeInutilizacao',
   'NfeConsultaProtocolo', 'NfeStatusServico', 'NfeConsultaCadastro',
   'RecepcaoEvento', 'RecepcaoEvento', 'RecepcaoEvento', 'NfeConsultaDest',
   'NfeDownloadNF', 'NfeAutorizacao', 'NfeRetAutorizacao', 'AdministrarCSCNFCe',
   'NFeDistribuicaoDFe', 'EventoEPEC'],
  [ LayNfeRecepcao, LayNfeRetRecepcao, LayNfeCancelamento, LayNfeInutilizacao,
    LayNfeConsulta, LayNfeStatusServico, LayNfeCadastro,
    LayNFeCCe, LayNFeEvento, LayNFeEventoAN, LayNFeConsNFeDest,
    LayNFeDownloadNFe, LayNfeAutorizacao, LayNfeRetAutorizacao,
    LayAdministrarCSCNFCe, LayDistDFeInt, LayNFCeEPEC ] );
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

function SchemaNFeToStr(const t: TSchemaNFe): String;
begin
  Result := GetEnumName(TypeInfo(TSchemaNFe), Integer( t ) );
  Result := copy(Result, 4, Length(Result)); // Remove prefixo "sch"
end;

function StrToSchemaNFe(const s: String): TSchemaNFe;
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

  CodSchema := GetEnumValue(TypeInfo(TSchemaNFe), SchemaStr );

  if CodSchema = -1 then
  begin
    raise Exception.Create(Format('"%s" não é um valor TSchemaNFe válido.',[SchemaStr]));
  end;

  Result := TSchemaNFe( CodSchema );
end;

// B25 - Finalidade de emissão da NF-e *****************************************
function FinNFeToStr(const t: TpcnFinalidadeNFe): String;
begin
  Result := EnumeradoToStr(t, ['1', '2', '3', '4'],
    [fnNormal, fnComplementar, fnAjuste, fnDevolucao]);
end;

function StrToFinNFe(out ok: Boolean; const s: String): TpcnFinalidadeNFe;
begin
  Result := StrToEnumerado(ok, s, ['1', '2', '3', '4'],
    [fnNormal, fnComplementar, fnAjuste, fnDevolucao]);
end;

function IndicadorNFeToStr(const t: TpcnIndicadorNFe): String;
begin
  Result := EnumeradoToStr(t, ['0', '1', '2'],
    [inTodas, inSemManifestacaoComCiencia, inSemManifestacaoSemCiencia]);
end;

function StrToIndicadorNFe(out ok: Boolean; const s: String): TpcnIndicadorNFe;
begin
  Result := StrToEnumerado(ok, s, ['0', '1', '2'],
    [inTodas, inSemManifestacaoComCiencia, inSemManifestacaoSemCiencia]);
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

function ModeloDFToStr(const t: TpcnModeloDF): String;
begin
  Result := EnumeradoToStr(t, ['55', '65'], [moNFe, moNFCe]);
end;

function StrToModeloDF(out ok: Boolean; const s: String): TpcnModeloDF;
begin
  Result := StrToEnumerado(ok, s, ['55', '65'], [moNFe, moNFCe]);
end;

function ModeloDFToPrefixo(const t: TpcnModeloDF): String;
begin
  Case t of
    moNFCe: Result := 'NFCe';
  else
    Result := 'NFe';
  end;
end;

function StrToVersaoDF(out ok: Boolean; const s: String): TpcnVersaoDF;
begin
  Result := StrToEnumerado(ok, s, ['2.00', '3.00', '3.10', '4.00'], [ve200, ve300, ve310, ve400]);
end;

function VersaoDFToStr(const t: TpcnVersaoDF): String;
begin
  Result := EnumeradoToStr(t, ['2.00', '3.00', '3.10', '4.00'], [ve200, ve300, ve310, ve400]);
end;

 function DblToVersaoDF(out ok: Boolean; const d: Real): TpcnVersaoDF;
 begin
   ok := True;

   if (d = 2.0) or (d < 3.0)  then
     Result := ve200
   else if (d >= 3.0) and (d < 3.1) then
     Result := ve300
   else if (d >= 3.10) and (d < 4) then
     Result := ve310
   else if (d >= 4) then
     Result := ve400
   else
   begin
     Result := ve310;
     ok := False;
   end;
 end;

 function VersaoDFToDbl(const t: TpcnVersaoDF): Real;
 begin
   case t of
     ve200: Result := 2.00;
     ve300: Result := 3.00;
     ve310: Result := 3.10;
     ve400: Result := 4.00;
   else
     Result := 0;
   end;
 end;

// J02 - Tipo da operação ******************************************************
 function tpOPToStr(const t: TpcnTipoOperacao): string;
begin
  result := EnumeradoToStr(t, ['1', '2', '3', '0'], [toVendaConcessionaria, toFaturamentoDireto, toVendaDireta, toOutros]);
end;

function StrTotpOP(out ok: boolean; const s: string): TpcnTipoOperacao;
begin
  result := StrToEnumerado(ok, s, ['1', '2', '3', '0'], [toVendaConcessionaria, toFaturamentoDireto, toVendaDireta, toOutros]);
end;

// J22 - Condição do Veículo ***************************************************
function condVeicToStr(const t: TpcnCondicaoVeiculo): string;
begin
  result := EnumeradoToStr(t, ['1', '2', '3'], [cvAcabado, cvInacabado, cvSemiAcabado]);
end;

function StrTocondVeic(out ok: boolean; const s: string): TpcnCondicaoVeiculo;
begin
  result := StrToEnumerado(ok, s, ['1', '2', '3'], [cvAcabado, cvInacabado, cvSemiAcabado]);
end;

// L02 - Indicador do tipo de arma de fogo *************************************
function tpArmaToStr(const t: TpcnTipoArma): string;
begin
  result := EnumeradoToStr(t, ['0', '1'], [taUsoPermitido, taUsoRestrito]);
end;

function StrTotpArma(out ok: boolean; const s: string): TpcnTipoArma;
begin
  result := StrToEnumerado(ok, s, ['0', '1'], [taUsoPermitido, taUsoRestrito]);
end;

function VeiculosRestricaoStr( const iRestricao : Integer ): String;
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

function VeiculosCorDENATRANStr( const sCorDENATRAN : String ): String;
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

function VeiculosCondicaoStr( const condVeic: TpcnCondicaoVeiculo ): String;
begin
  case condVeic of
    cvAcabado     : result := '1-ACABADO';
    cvInacabado   : result := '2-INACABADO';
    cvSemiAcabado : result := '3-SEMI-ACABADO';
  end;
end;

function VeiculosVinStr( const sVin: String ): String;
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

function VeiculosEspecieStr( const iEspecie : Integer ): String;
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

function VeiculosTipoStr( const iTipoVeic : Integer ): String;
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

function VeiculosCombustivelStr( const sTpComb : String ): String;
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

function VeiculosTipoOperStr( const TtpOP : TpcnTipoOperacao ): String;
begin
  case TtpOP of
    toVendaConcessionaria : result := '1-VENDA CONCESSIONÁRIA';
    toFaturamentoDireto   : result := '2-FAT. DIRETO CONS. FINAL';
    toVendaDireta         : result := '3-VENDA DIRETA';
    toOutros              : result := '0-OUTROS';
  end;

end;

function ArmaTipoStr( const TtpArma : TpcnTipoArma ): String;
begin
  case TtpArma of
    taUsoPermitido: result := '0-USO PERMITIDO';
    taUsoRestrito : result := '1-USO RESTRITO';
  end;
end;

function IndEscalaToStr(const t: TpcnIndEscala): String;
begin
  result := EnumeradoToStr(t, ['S', 'N', ''],
                              [ieRelevante, ieNaoRelevante, ieNenhum]);
end;

function StrToIndEscala(out ok: Boolean; const s: String): TpcnIndEscala;
begin
  result := StrToEnumerado(ok, s, ['S', 'N', ''],
                                  [ieRelevante, ieNaoRelevante, ieNenhum]);
end;

// ??? - Modalidade do frete ***************************************************
function modFreteToStr(const t: TpcnModalidadeFrete): string;
begin
  result := EnumeradoToStr(t, ['0', '1', '2', '3', '4', '9'],
    [mfContaEmitente, mfContaDestinatario, mfContaTerceiros, mfProprioRemetente, mfProprioDestinatario, mfSemFrete]);
end;

function StrTomodFrete(out ok: boolean; const s: string): TpcnModalidadeFrete;
begin
  result := StrToEnumerado(ok, s, ['0', '1', '2',  '3', '4', '9'],
    [mfContaEmitente, mfContaDestinatario, mfContaTerceiros, mfProprioRemetente, mfProprioDestinatario, mfSemFrete]);
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

function SchemaEventoToStr(const t: TSchemaNFe): String;
begin
  result := EnumeradoToStr(t, ['e110110', 'e110111', 'e110112', 'e110140',
                               'e111500', 'e111501', 'e111502', 'e111503',
                               'e210200', 'e210210', 'e210220', 'e210240',
                               'e110130', 'e110131', 'e110150', 'e110192',
                               'e110193', 'e110750', 'e110751'],
    [schEnvCCe, schcancNFe, schCancSubst, schEnvEPEC,
     schPedProrrog1, schPedProrrog2, schCanPedProrrog1, schCanPedProrrog2,
     schManifDestConfirmacao, schManifDestCiencia, schManifDestDesconhecimento,
     schManifDestOperNaoRealizada, schCompEntrega, schCancCompEntrega,
     schAtorInteressadoNFe, schInsucessoEntregaNFe, schCancInsucessoEntregaNFe,
     schConcFinanceira, schCancConcFinanceira]);
end;

function AutorizacaoToStr(const t: TAutorizacao): string;
begin
  result := EnumeradoToStr(t, ['0', '1', ''],
                              [taNaoPermite, taPermite, taNaoInformar]);
end;

function StrToAutorizacao(out ok: boolean; const s: string): TAutorizacao;
begin
  result := StrToEnumerado(ok, s, ['0', '1', ''],
                                  [taNaoPermite, taPermite, taNaoInformar]);
end;

function IndIntermedToStr(const t: TindIntermed): string;
begin
  Result := EnumeradoToStr(t, ['', '0', '1'],
       [iiSemOperacao, iiOperacaoSemIntermediador, iiOperacaoComIntermediador]);
end;

function StrToIndIntermed(out ok: boolean; const s: string): TindIntermed;
begin
  Result := StrToEnumerado(ok, s, ['', '0', '1'],
       [iiSemOperacao, iiOperacaoSemIntermediador, iiOperacaoComIntermediador]);
end;

function tpAtoToStr(const t: TtpAto): string;
begin
  Result := EnumeradoToStr(t, ['', '08', '10', '12', '14', '15'],
       [taNenhum, taTermoAcordo, taRegimeEspecial, taAutorizacaoEspecifica,
            taAjusteSNIEF, taConvenioICMS]);
end;

function StrTotpAto(out ok: boolean; const s: string): TtpAto;
begin
  Result := StrToEnumerado(ok, s, ['', '08', '10', '12', '14', '15'],
       [taNenhum, taTermoAcordo, taRegimeEspecial, taAutorizacaoEspecifica,
            taAjusteSNIEF, taConvenioICMS]);
end;

function indImportToStr(const t: TindImport): string;
begin
  Result := EnumeradoToStr(t, ['0', '1'],
       [iiNacional, iiImportado]);
end;

function StrToindImport(out ok: boolean; const s: string): TindImport;
begin
  Result := StrToEnumerado(ok, s, ['0', '1'],
       [iiNacional, iiImportado]);
end;

function motRedAdRemToStr(const t: TmotRedAdRem): string;
begin
  Result := EnumeradoToStr(t, ['1', '9'],
       [motTranspColetivo, motOutros]);
end;

function StrTomotRedAdRem(out ok: boolean; const s: string): TmotRedAdRem;
begin
  Result := StrToEnumerado(ok, s, ['1', '9'],
       [motTranspColetivo, motOutros]);
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

initialization
  RegisterStrToTpEventoDFe(StrToTpEventoNFe, 'NFe');

end.

