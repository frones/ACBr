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

unit ACBrDCeConversao;

interface

uses
  SysUtils, StrUtils, Classes,
  pcnConversao;

type
  TStatusDCe = (stDCeIdle, stDCeStatusServico, stDCeRecepcao, stDCeRetRecepcao,
                stDCeConsulta, stDCeRecibo, stDCeEmail, stDCeEvento,
                stDCeDistDFeInt, stDCeEnvioWebService);

  TVersaoDCe = (ve100);

const
  TVersaoDCeArrayStrings: array[TVersaoDCe] of string = ('1.00');
  TVersaoDCeArrayDouble: array[TVersaoDCe] of Double = (1.00);

type
  TSchemaDCe = (schErroDCe, schDCe, schEventoDCe, schconsReciDCe,
                schconsSitDCe, schconsStatServDCe, schevCancDCe,
                schdistDFeInt);

const
  TSchemaDCeArrayStrings: array[TSchemaDCe] of string = ('', '', '', '', '', '',
    'evCancDCe', 'distDFeInt');

type
  TLayOutDCe = (LayDCeRecepcao, LayDCeRetRecepcao, LayDCeConsulta,
                LayDCeStatusServico, LayDCeEvento, LayDCeConsNaoEnc,
                LayDCeDistDFeInt, LayDCeRecepcaoSinc);

const
  TLayOutDCeArrayStrings: array[TLayOutDCe] of string = ('DCeRecepcao',
    'DCeRetRecepcao', 'DCeConsultaProtocolo', 'DCeStatusServico',
    'RecepcaoEvento', 'DCeConsNaoEnc', 'DCeDistDFeInt', 'DCeRecepcaoSinc');

type
  TEmitenteDCe = (teFisco, teMarketplace, teEmissorProprio, teTransportadora);

const
  TEmitenteDCeArrayStrings: array[TEmitenteDCe] of string = ('0', '1', '2', '3');

type
  TModTrans = (mtCorreios, mtPropria, mtTransportadora);

const
  TModTransArrayStrings: array[TModTrans] of string = ('0', '1', '2');

{
  Declaração das funções de conversão
}
function StrTotpEventoDCe(out ok: boolean; const s: string): TpcnTpEvento;

function StrToVersaoDCe(out ok: Boolean; const s: String): TVersaoDCe;
function VersaoDCeToStr(const t: TVersaoDCe): String;

function DblToVersaoDCe(out ok: Boolean; const d: Double): TVersaoDCe;
function VersaoDCeToDbl(const t: TVersaoDCe): Double;

function SchemaDCeToStr(const t: TSchemaDCe): String;
function StrToSchemaDCe(const s: String): TSchemaDCe;

function LayOutDCeToSchema(const t: TLayOutDCe): TSchemaDCe;

function LayOutDCeToServico(const t: TLayOutDCe): String;
function ServicoToLayOutDCe(out ok: Boolean; const s: String): TLayOutDCe;

function EmitenteDCeToStr(const t: TEmitenteDCe): String;
function StrToEmitenteDCe(out ok: Boolean; const s: String): TEmitenteDCe;

function ModTransToStr(const t: TModTrans): String;
function StrToModTrans(out ok: Boolean; const s: String): TModTrans;

implementation

uses
  typinfo,
  ACBrBase;

function StrTotpEventoDCe(out ok: boolean; const s: string): TpcnTpEvento;
begin
  Result := StrToEnumerado(ok, s,
            ['-99999', '110111', '110112', '110114', '110115', '110116',
             '310112', '510620'],
            [teNaoMapeado, teCancelamento, teEncerramento, teInclusaoCondutor,
             teInclusaoDFe, tePagamentoOperacao, teEncerramentoFisco,
             teRegistroPassagemBRId]);
end;

function StrToVersaoDCe(out ok: Boolean; const s: String): TVersaoDCe;
var
  idx: TVersaoDCe;
begin
  for idx := Low(TVersaoDCeArrayStrings) to High(TVersaoDCeArrayStrings) do
  begin
    if (TVersaoDCeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TVersaoDCe: %s', [s]);
end;

function VersaoDCeToStr(const t: TVersaoDCe): String;
begin
  result := TVersaoDCeArrayStrings[t];
end;

function DblToVersaoDCe(out ok: Boolean; const d: Double): TVersaoDCe;
var
  idx: TVersaoDCe;
begin
  for idx := Low(TVersaoDCeArrayDouble) to High(TVersaoDCeArrayDouble) do
  begin
    if (TVersaoDCeArrayDouble[idx] = d) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TVersaoDCe: %s',
    [FormatFloat('0.00', d)]);
end;

function VersaoDCeToDbl(const t: TVersaoDCe): Double;
begin
  result := TVersaoDCeArrayDouble[t];
end;

function SchemaDCeToStr(const t: TSchemaDCe): String;
begin
  Result := GetEnumName(TypeInfo(TSchemaDCe), Integer(t));
  Result := copy(Result, 4, Length(Result)); // Remove prefixo "sch"
end;

function StrToSchemaDCe(const s: String): TSchemaDCe;
var
  P: Integer;
  SchemaStr: String;
  CodSchema: Integer;
begin
  P := pos('_', s);

  if P > 0 then
    SchemaStr := copy(s, 1, P-1)
  else
    SchemaStr := s;

  if LeftStr(SchemaStr, 3) <> 'sch' then
    SchemaStr := 'sch' + SchemaStr;

  CodSchema := GetEnumValue(TypeInfo(TSchemaDCe), SchemaStr);

  if CodSchema = -1 then
    raise Exception.Create(Format('"%s" não é um valor TSchemaDCe válido.', [SchemaStr]));

  Result := TSchemaDCe(CodSchema);
end;

function LayOutDCeToSchema(const t: TLayOutDCe): TSchemaDCe;
begin
  case t of
    LayDCeRecepcao,
    LayDCeRecepcaoSinc:  Result := schDCe;
    LayDCeRetRecepcao:   Result := schconsReciDCe;
    LayDCeConsulta:      Result := schconsSitDCe;
    LayDCeStatusServico: Result := schconsStatServDCe;
    LayDCeEvento:        Result := schEventoDCe;
    LayDCeDistDFeInt:    Result := schdistDFeInt;
  else
    Result := schErroDCe;
  end;
end;

function LayOutDCeToServico(const t: TLayOutDCe): String;
begin
  result := TLayOutDCeArrayStrings[t];
end;

function ServicoToLayOutDCe(out ok: Boolean; const s: String): TLayOutDCe;
var
  idx: TLayOutDCe;
begin
  for idx := Low(TLayOutDCeArrayStrings) to High(TLayOutDCeArrayStrings) do
  begin
    if (TLayOutDCeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TLayOutDCe: %s', [s]);
end;

function EmitenteDCeToStr(const t: TEmitenteDCe): String;
begin
  result := TEmitenteDCeArrayStrings[t];
end;

function StrToEmitenteDCe(out ok: Boolean; const s: String): TEmitenteDCe;
var
  idx: TEmitenteDCe;
begin
  for idx := Low(TEmitenteDCeArrayStrings) to High(TEmitenteDCeArrayStrings) do
  begin
    if (TEmitenteDCeArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TEmitenteDCe: %s', [s]);
end;

function ModTransToStr(const t: TModTrans): String;
begin
  result := TModTransArrayStrings[t];
end;

function StrToModTrans(out ok: Boolean; const s: String): TModTrans;
var
  idx: TModTrans;
begin
  for idx := Low(TModTransArrayStrings) to High(TModTransArrayStrings) do
  begin
    if (TModTransArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TModTrans: %s', [s]);
end;

initialization
  RegisterStrToTpEventoDFe(StrTotpEventoDCe, 'DCe');

end.

