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

unit pmdfeConversaoMDFe;

interface

uses
  SysUtils, StrUtils, Classes,
  pcnConversao;

type
  TStatusACBrMDFe = (stMDFeIdle, stMDFeStatusServico, stMDFeRecepcao, stMDFeRetRecepcao,
                     stMDFeConsulta, stMDFeRecibo, stMDFeEmail, stMDFeEvento,
                     stMDFeDistDFeInt, stMDFeEnvioWebService);

  TVersaoMDFe     = (ve100, ve300);

const
  TVersaoMDFeArrayStrings: array[TVersaoMDFe] of string = ('1.00', '3.00');
  TVersaoMDFeArrayDouble: array[TVersaoMDFe] of Double = (1.00, 3.00);

type
  TSchemaMDFe     = (schErro, schMDFe, schEventoMDFe,
                     schconsReciMDFe, schconsSitMDFe, schconsStatServMDFe,
                     schmdfeModalAereo, schmdfeModalAquaviario,
                     schmdfeModalFerroviario, schmdfeModalRodoviario,
                     schevCancMDFe, schevEncMDFe, schevIncCondutorMDFe,
                     schdistDFeInt, schconsMDFeNaoEnc, schevInclusaoDFeMDFe,
                     schevPagtoOperMDFe, schevConfirmaServMDFe,
                     schevAlteracaoPagtoServMDFe);

const
  TSchemaMDFeArrayStrings: array[TSchemaMDFe] of string = ('', '', '', '',
    '', '', '', '', '', '', 'evCancMDFe', 'evEncMDFe', 'evIncCondutorMDFe',
    'distDFeInt', 'consMDFeNaoEnc', 'evInclusaoDFeMDFe', 'evPagtoOperMDFe',
    'evConfirmaServMDFe', 'evAlteracaoPagtoServMDFe');

type
  TLayOutMDFe     = (LayMDFeRecepcao, LayMDFeRetRecepcao, LayMDFeConsulta,
                     LayMDFeStatusServico, LayMDFeEvento, LayMDFeConsNaoEnc,
                     LayMDFeDistDFeInt, LayMDFeRecepcaoSinc,
                     LayMDFeURLQRCode, LayURLConsultaMDFe);

const
  TLayOutMDFeArrayStrings: array[TLayOutMDFe] of string = ('MDFeRecepcao',
    'MDFeRetRecepcao', 'MDFeConsultaProtocolo', 'MDFeStatusServico',
    'RecepcaoEvento', 'MDFeConsNaoEnc', 'MDFeDistDFeInt', 'MDFeRecepcaoSinc',
    'URL-QRCode', 'URL-ConsultaMDFe');

type
  TTpEmitenteMDFe = (teTransportadora, teTranspCargaPropria,
                     teTranspCTeGlobalizado);

const
  TTpEmitenteMDFeArrayStrings: array[TTpEmitenteMDFe] of string = ('1', '2', '3');

type
  TModalMDFe      = (moRodoviario, moAereo, moAquaviario, moFerroviario);

const
  TModalMDFeArrayStrings: array[TModalMDFe] of string = ('1', '2', '3', '4');

type
  TTransportadorMDFe = (ttNenhum, ttETC, ttTAC, ttCTC);

const
  TTransportadorMDFeArrayStrings: array[TTransportadorMDFe] of string = ('',
    '1', '2', '3');

type
  TRspSegMDFe = (rsEmitente, rsTomadorServico);

const
  TRspSegMDFeArrayStrings: array[TRspSegMDFe] of string = ('1', '2');
  TRspSegMDFeDescArrayStrings: array[TRspSegMDFe] of string = ('EMITENTE',
    'TOMADOR SERVICO');

type
  TCarga = (tcGranelSolido, tcGranelLiquido, tcFrigorificada, tcConteinerizada,
            tcCargaGeral, tcNeogranel, tcPerigosaGranelSolido,
            tcPerigosaGranelLiquido, tcPerigosaCargaFrigorificada,
            tcPerigosaConteinerizada, tcPerigosaCargaGeral);

const
  TCargaArrayStrings: array[TCarga] of string = ('01', '02', '03', '04', '05',
    '06', '07', '08', '09', '10', '11');

type
  TIndPag = (ipVista, ipPrazo);

const
  TIndPagArrayStrings: array[TIndPag] of string = ('0', '1');

type
  TComp = (tcValePedagio, tcImpostos, tcDespesas, tcOutros);

const
  TCompArrayStrings: array[TComp] of string = ('01', '02', '03', '99');

type
  TtpValePed = (tvpNenhum, tvpTAG, tvpCupom, tvpCartao);

const
  TtpValePedArrayStrings: array[TtpValePed] of string = ('', '01', '02', '03');
  TtpValePedDescArrayStrings: array[TtpValePed] of string = ('Nenhum', 'TAG',
    'Cupom', 'Cartão');

type
  TcategCombVeic = (tcNenhum, tcVeicCom2Eixos, tcVeicCom3Eixos, tcVeicCom4Eixos,
                    tcVeicCom5Eixos, tcVeicCom6Eixos, tcVeicCom7Eixos,
                    tcVeicCom8Eixos, tcVeicCom9Eixos, tcVeicCom10Eixos,
                    tcVeicComAcima10Eixos);

const
  TcategCombVeicArrayStrings: array[TcategCombVeic] of string = ('', '02', '04',
    '06', '07', '08', '10', '11', '12', '13', '14');
  TcategCombVeicDescArrayStrings: array[TcategCombVeic] of string = ('',
    'Veículo Comercial 2 eixos', 'Veículo Comercial 3 eixos',
    'Veículo Comercial 4 eixos', 'Veículo Comercial 5 eixos',
    'Veículo Comercial 6 eixos', 'Veículo Comercial 7 eixos',
    'Veículo Comercial 8 eixos', 'Veículo Comercial 9 eixos',
    'Veículo Comercial 10 eixos', 'Veículo Comercial Acima de 10 eixos');

type
  TtpAntecip = (taNenhum, taNaoPermiteAntecipar, taPermiteAntecipar,
                taPermiteAnteciparComConfirmacao);

const
  TtpAntecipArrayStrings: array[TtpAntecip] of string = ('', '0', '1', '2');

const
  TIndicadorArrayStrings: array[TIndicador] of string = ('1', '');

{
  Declaração das funções de conversão
}
function StrToTpEventoMDFe(out ok: boolean; const s: string): TpcnTpEvento;

function TpEmitenteToStr(const t: TTpEmitenteMDFe): string;
function StrToTpEmitente(out ok: Boolean; const s: string): TTpEmitenteMDFe;

function LayOutToSchema(const t: TLayOutMDFe): TSchemaMDFe;

function ModalToStr(const t: TModalMDFe): string;
function StrToModal(out ok: Boolean; const s: string): TModalMDFe;

function GetVersaoModalMDFe(AVersaoDF: TVersaoMDFe; AModal: TModalMDFe): string;

function LayOutToServico(const t: TLayOutMDFe): string;
function ServicoToLayOut(out ok: Boolean; const s: string): TLayOutMDFe;

function SchemaMDFeToStr(const t: TSchemaMDFe): string;
function StrToSchemaMDFe(const s: string): TSchemaMDFe;

function StrToVersaoMDFe(out ok: Boolean; const s: string): TVersaoMDFe;
function VersaoMDFeToStr(const t: TVersaoMDFe): string;

function DblToVersaoMDFe(out ok: Boolean; const d: Double): TVersaoMDFe;
function VersaoMDFeToDbl(const t: TVersaoMDFe): Double;

function TTransportadorToStr(const t: TTransportadorMDFe): string;
function StrToTTransportador(out ok: Boolean; const s: string): TTransportadorMDFe;

function RspSeguroMDFeToStr(const t: TRspSegMDFe): string;
function RspSeguroMDFeToStrText(const t: TRspSegMDFe): string;
function StrToRspSeguroMDFe(out ok: boolean; const s: string ): TRspSegMDFe;

function TCargaToStr(const t: TCarga): string;
function StrToTCarga(out ok: Boolean; const s: string): TCarga;

function TIndPagToStr(const t: TIndPag): string;
function StrToTIndPag(out ok: Boolean; const s: string): TIndPag;

function TCompToStr(const t: TComp): string;
function StrToTComp(out ok: Boolean; const s: string): TComp;

function indAltoDesempToStr(const t: TIndicador): string;
function StrToindAltoDesemp(out ok: Boolean; const s: string): TIndicador;

function tpValePedToStr(const t: TtpValePed): string;
function tpValePedToStrText(const t: TtpValePed): string;
function StrTotpValePed(out ok: Boolean; const s: string): TtpValePed;

function categCombVeicToStr(const t: TcategCombVeic): string;
function categCombVeicToStrText(const t: TcategCombVeic): string;
function StrTocategCombVeic(out ok: Boolean; const s: string): TcategCombVeic;

function tpAntecipToStr(const t: TtpAntecip): string;
function StrTotpAntecip(out ok: Boolean; const s: string): TtpAntecip;

implementation

uses
  typinfo,
  ACBrBase;

function StrToTpEventoMDFe(out ok: boolean; const s: string): TpcnTpEvento;
begin
  Result := StrToEnumerado(ok, s,
            ['-99999', '110111', '110112', '110114', '110115', '110116',
             '310112', '510620', '110117', '110118'],
            [teNaoMapeado, teCancelamento, teEncerramento, teInclusaoCondutor,
             teInclusaoDFe, tePagamentoOperacao, teEncerramentoFisco,
             teRegistroPassagemBRId, teConfirmaServMDFe,
             teAlteracaoPagtoServMDFe]);
end;

// Tipo de Emitente*************************************************************

function TpEmitenteToStr(const t: TTpEmitenteMDFe): string;
begin
  result := TTpEmitenteMDFeArrayStrings[t];
end;

function StrToTpEmitente(out ok: Boolean; const s: string): TTpEmitenteMDFe;
var
  idx: TTpEmitenteMDFe;
begin
  ok := True;

  for idx := Low(TTpEmitenteMDFeArrayStrings) to High(TTpEmitenteMDFeArrayStrings) do
  begin
    if (TTpEmitenteMDFeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TTpEmitenteMDFe: %s', [s]);
end;

function LayOutToSchema(const t: TLayOutMDFe): TSchemaMDFe;
begin
  case t of
    LayMDFeRecepcao,
    LayMDFeRecepcaoSinc:   Result := schMDFe;
    LayMDFeRetRecepcao:    Result := schconsReciMDFe;
    LayMDFeConsulta:       Result := schconsSitMDFe;
    LayMDFeStatusServico:  Result := schconsStatServMDFe;
    LayMDFeEvento:         Result := schEventoMDFe;
    LayMDFeConsNaoEnc:     Result := schconsMDFeNaoEnc;
    LayMDFeDistDFeInt:     Result := schdistDFeInt;
  else
    Result := schErro;
  end;
end;

// Modal************************************************************************

function ModalToStr(const t: TModalMDFe): string;
begin
  result := TModalMDFeArrayStrings[t];
end;

function StrToModal(out ok: Boolean; const s: string): TModalMDFe;
var
  idx: TModalMDFe;
begin
  ok := True;

  for idx := Low(TModalMDFeArrayStrings) to High(TModalMDFeArrayStrings) do
  begin
    if (TModalMDFeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TModalMDFe: %s', [s]);
end;

function GetVersaoModalMDFe(AVersaoDF: TVersaoMDFe; AModal: TModalMDFe): string;
begin
  result := '';

  case AVersaoDF of
    ve100: begin
             case AModal of
               moRodoviario:  result := '1.00';
               moAereo:       result := '1.00';
               moAquaviario:  result := '1.00';
               moFerroviario: result := '1.00';
             end;
           end;
    ve300: begin
             case AModal of
               moRodoviario:  result := '3.00';
               moAereo:       result := '3.00';
               moAquaviario:  result := '3.00';
               moFerroviario: result := '3.00';
             end;
           end;
  end;
end;

function LayOutToServico(const t: TLayOutMDFe): string;
begin
  result := TLayOutMDFeArrayStrings[t];
end;

function ServicoToLayOut(out ok: Boolean; const s: string): TLayOutMDFe;
var
  idx: TLayOutMDFe;
begin
  ok := True;

  for idx := Low(TLayOutMDFeArrayStrings) to High(TLayOutMDFeArrayStrings) do
  begin
    if (TLayOutMDFeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TLayOutMDFe: %s', [s]);
end;

function SchemaMDFeToStr(const t: TSchemaMDFe): string;
begin
  Result := GetEnumName(TypeInfo(TSchemaMDFe), Integer( t ) );
  Result := copy(Result, 4, Length(Result)); // Remove prefixo "sch"
end;

function StrToSchemaMDFe(const s: string): TSchemaMDFe;
var
  P: Integer;
  SchemaStr: string;
  CodSchema: Integer;
begin
  P := pos('_', s);
  if P > 0 then
    SchemaStr := copy(s, 1, P-1)
  else
    SchemaStr := s;

  if LeftStr(SchemaStr, 3) <> 'sch' then
    SchemaStr := 'sch' + SchemaStr;

  CodSchema := GetEnumValue(TypeInfo(TSchemaMDFe), SchemaStr );

  if CodSchema = -1 then
  begin
    raise Exception.Create(Format('"%s" não é um valor TSchemaMDFe válido.',[SchemaStr]));
  end;

  Result := TSchemaMDFe( CodSchema );
end;

function StrToVersaoMDFe(out ok: Boolean; const s: string): TVersaoMDFe;
var
  idx: TVersaoMDFe;
begin
  ok := True;

  for idx := Low(TVersaoMDFeArrayStrings) to High(TVersaoMDFeArrayStrings) do
  begin
    if (TVersaoMDFeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TVersaoMDFe: %s', [s]);
end;

function VersaoMDFeToStr(const t: TVersaoMDFe): string;
begin
  result := TVersaoMDFeArrayStrings[t];
end;

function DblToVersaoMDFe(out ok: Boolean; const d: Double): TVersaoMDFe;
var
  idx: TVersaoMDFe;
begin
  ok := True;

  for idx := Low(TVersaoMDFeArrayDouble) to High(TVersaoMDFeArrayDouble) do
  begin
    if (TVersaoMDFeArrayDouble[idx] = d) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TVersaoMDFe: %s',
    [FormatFloat('0.00', d)]);
end;

function VersaoMDFeToDbl(const t: TVersaoMDFe): Double;
begin
  result := TVersaoMDFeArrayDouble[t];
end;

function TTransportadorToStr(const t: TTransportadorMDFe): string;
begin
  result := TTransportadorMDFeArrayStrings[t];
end;

function StrToTTransportador(out ok: Boolean; const s: string): TTransportadorMDFe;
var
  idx: TTransportadorMDFe;
begin
  ok := True;

  for idx := Low(TTransportadorMDFeArrayStrings) to High(TTransportadorMDFeArrayStrings) do
  begin
    if (TTransportadorMDFeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TTransportadorMDFe: %s', [s]);
end;

function RspSeguroMDFeToStr(const t: TRspSegMDFe): string;
begin
  result := TRspSegMDFeArrayStrings[t];
end;

function RspSeguroMDFeToStrText(const t: TRspSegMDFe): string;
begin
  result := TRspSegMDFeDescArrayStrings[t];
end;

function StrToRspSeguroMDFe(out ok: boolean; const s: string ): TRspSegMDFe;
var
  idx: TRspSegMDFe;
begin
  ok := True;

  for idx := Low(TRspSegMDFeArrayStrings) to High(TRspSegMDFeArrayStrings) do
  begin
    if (TRspSegMDFeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TRspSegMDFe: %s', [s]);
end;

function TCargaToStr(const t: TCarga): string;
begin
  result := TCargaArrayStrings[t];
end;

function StrToTCarga(out ok: Boolean; const s: string): TCarga;
var
  idx: TCarga;
begin
  ok := True;

  for idx := Low(TCargaArrayStrings) to High(TCargaArrayStrings) do
  begin
    if (TCargaArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TCarga: %s', [s]);
end;

function TIndPagToStr(const t: TIndPag): string;
begin
  result := TIndPagArrayStrings[t];
end;

function StrToTIndPag(out ok: Boolean; const s: string): TIndPag;
var
  idx: TIndPag;
begin
  ok := True;

  for idx := Low(TIndPagArrayStrings) to High(TIndPagArrayStrings) do
  begin
    if (TIndPagArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TIndPag: %s', [s]);
end;

function TCompToStr(const t: TComp): string;
begin
  result := TCompArrayStrings[t];
end;

function StrToTComp(out ok: Boolean; const s: string): TComp;
var
  idx: TComp;
begin
  ok := True;

  for idx := Low(TCompArrayStrings) to High(TCompArrayStrings) do
  begin
    if (TCompArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TComp: %s', [s]);
end;

function indAltoDesempToStr(const t: TIndicador): string;
begin
  result := TIndicadorArrayStrings[t];
end;

function StrToindAltoDesemp(out ok: Boolean; const s: string): TIndicador;
var
  idx: TIndicador;
begin
  ok := True;

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

function tpValePedToStr(const t: TtpValePed): string;
begin
  result := TtpValePedArrayStrings[t];
end;

function tpValePedToStrText(const t: TtpValePed): string;
begin
  result := TtpValePedDescArrayStrings[t];
end;

function StrTotpValePed(out ok: Boolean; const s: string): TtpValePed;
var
  idx: TtpValePed;
begin
  ok := True;

  for idx := Low(TtpValePedArrayStrings) to High(TtpValePedArrayStrings) do
  begin
    if (TtpValePedArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpValePed: %s', [s]);
end;

function categCombVeicToStrText(const t: TcategCombVeic): string;
begin
  result := TcategCombVeicDescArrayStrings[t];
end;

function categCombVeicToStr(const t: TcategCombVeic): string;
begin
  result := TcategCombVeicArrayStrings[t];
end;

function StrTocategCombVeic(out ok: Boolean; const s: string): TcategCombVeic;
var
  idx: TcategCombVeic;
begin
  ok := True;

  for idx := Low(TcategCombVeicArrayStrings) to High(TcategCombVeicArrayStrings) do
  begin
    if (TcategCombVeicArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TcategCombVeic: %s', [s]);
end;

function tpAntecipToStr(const t: TtpAntecip): string;
begin
  result := TtpAntecipArrayStrings[t];
end;

function StrTotpAntecip(out ok: Boolean; const s: string): TtpAntecip;
var
  idx: TtpAntecip;
begin
  ok := True;

  for idx := Low(TtpAntecipArrayStrings) to High(TtpAntecipArrayStrings) do
  begin
    if (TtpAntecipArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpAntecip: %s', [s]);
end;

initialization
  RegisterStrToTpEventoDFe(StrToTpEventoMDFe, 'MDFe');

end.

