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
  TTpEmitenteMDFe = (teTransportadora, teTranspCargaPropria,
                     teTranspCTeGlobalizado);

  TModalMDFe      = (moRodoviario, moAereo, moAquaviario, moFerroviario);

  TVersaoMDFe     = (ve100, ve300);

  TLayOutMDFe     = (LayMDFeRecepcao, LayMDFeRetRecepcao, LayMDFeConsulta,
                     LayMDFeStatusServico, LayMDFeEvento, LayMDFeConsNaoEnc,
                     LayMDFeDistDFeInt, LayMDFeRecepcaoSinc);

  TSchemaMDFe     = (schErro, schMDFe, schEventoMDFe,
                 //    schresMDFe, schresEvento, schprocMDFe, schprocEventoMDFe,
                     schconsReciMDFe, schconsSitMDFe, schconsStatServMDFe,
                     schmdfeModalAereo, schmdfeModalAquaviario,
                     schmdfeModalFerroviario, schmdfeModalRodoviario,
                     schevCancMDFe, schevEncMDFe, schevIncCondutorMDFe,
                     schdistDFeInt, schconsMDFeNaoEnc, schevInclusaoDFeMDFe,
                     schevPagtoOperMDFe);

  TStatusACBrMDFe = (stMDFeIdle, stMDFeStatusServico, stMDFeRecepcao, stMDFeRetRecepcao,
                     stMDFeConsulta, stMDFeRecibo, stMDFeEmail, stMDFeEvento,
                     stMDFeDistDFeInt, stMDFeEnvioWebService);

  TTransportadorMDFe = (ttNenhum, ttETC, ttTAC, ttCTC);

  TRspSegMDFe = (rsEmitente, rsTomadorServico);

  TCarga = (tcGranelSolido, tcGranelLiquido, tcFrigorificada, tcConteinerizada,
            tcCargaGeral, tcNeogranel, tcPerigosaGranelSolido,
            tcPerigosaGranelLiquido, tcPerigosaCargaFrigorificada,
            tcPerigosaConteinerizada, tcPerigosaCargaGeral);

  TIndPag = (ipVista, ipPrazo, ipNenhum);

  TComp = (tcValePedagio, tcImpostos, tcDespesas, tcOutros);

function StrToEnumerado(out ok: boolean; const s: string; const AString: array of string;
  const AEnumerados: array of variant): variant;
function EnumeradoToStr(const t: variant; const AString:
  array of string; const AEnumerados: array of variant): variant;

function TpEmitenteToStr(const t: TTpEmitenteMDFe): String;
function StrToTpEmitente(out ok: Boolean; const s: String): TTpEmitenteMDFe;

function LayOutToSchema(const t: TLayOutMDFe): TSchemaMDFe;

function ModalToStr(const t: TModalMDFe): String;
function StrToModal(out ok: Boolean; const s: String): TModalMDFe;

function GetVersaoModalMDFe(AVersaoDF: TVersaoMDFe; AModal: TModalMDFe): string;

function LayOutToServico(const t: TLayOutMDFe): String;
function ServicoToLayOut(out ok: Boolean; const s: String): TLayOutMDFe;

function SchemaMDFeToStr(const t: TSchemaMDFe): String;
function StrToSchemaMDFe(const s: String): TSchemaMDFe;

function StrToVersaoMDFe(out ok: Boolean; const s: String): TVersaoMDFe;
function VersaoMDFeToStr(const t: TVersaoMDFe): String;

function DblToVersaoMDFe(out ok: Boolean; const d: Double): TVersaoMDFe;
function VersaoMDFeToDbl(const t: TVersaoMDFe): Double;

function TTransportadorToStr(const t: TTransportadorMDFe): String;
function StrToTTransportador(out ok: Boolean; const s: String): TTransportadorMDFe;

function RspSeguroMDFeToStr(const t: TRspSegMDFe): String;
function RspSeguroMDFeToStrText(const t: TRspSegMDFe): String;
function StrToRspSeguroMDFe(out ok: boolean; const s: String ): TRspSegMDFe;

function StrToTpEventoMDFe(out ok: boolean; const s: string): TpcnTpEvento;

function TCargaToStr(const t: TCarga): String;
function StrToTCarga(out ok: Boolean; const s: String): TCarga;

function TIndPagToStr(const t: TIndPag): String;
function StrToTIndPag(out ok: Boolean; const s: String): TIndPag;

function TCompToStr(const t: TComp): String;
function StrToTComp(out ok: Boolean; const s: String): TComp;

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

// Tipo de Emitente*************************************************************

function TpEmitenteToStr(const t: TTpEmitenteMDFe): String;
begin
  result := EnumeradoToStr(t,
                           ['1', '2', '3'],
                           [teTransportadora, teTranspCargaPropria,
                            teTranspCTeGlobalizado]);
end;

function StrToTpEmitente(out ok: Boolean; const s: String): TTpEmitenteMDFe;
begin
  result := StrToEnumerado(ok, s,
                           ['1', '2', '3'],
                           [teTransportadora, teTranspCargaPropria,
                            teTranspCTeGlobalizado]);
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

function ModalToStr(const t: TModalMDFe): String;
begin
  result := EnumeradoToStr(t,
                           ['1', '2', '3', '4'],
                           [moRodoviario, moAereo, moAquaviario, moFerroviario]);
end;

function StrToModal(out ok: Boolean; const s: String): TModalMDFe;
begin
  result := StrToEnumerado(ok, s,
                           ['1', '2', '3', '4'],
                           [moRodoviario, moAereo, moAquaviario, moFerroviario]);
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

function LayOutToServico(const t: TLayOutMDFe): String;
begin
  Result := EnumeradoToStr(t,
    ['MDFeRecepcao', 'MDFeRetRecepcao', 'MDFeConsultaProtocolo',
     'MDFeStatusServico', 'RecepcaoEvento', 'MDFeConsNaoEnc',
     'MDFeDistDFeInt', 'MDFeRecepcaoSinc'],
    [ LayMDFeRecepcao, LayMDFeRetRecepcao, LayMDFeConsulta,
      LayMDFeStatusServico, LayMDFeEvento, LayMDFeConsNaoEnc,
      LayMDFeDistDFeInt, LayMDFeRecepcaoSinc ] );
end;

function ServicoToLayOut(out ok: Boolean; const s: String): TLayOutMDFe;
begin
  Result := StrToEnumerado(ok, s,
  ['MDFeRecepcao', 'MDFeRetRecepcao', 'MDFeConsultaProtocolo',
   'MDFeStatusServico', 'RecepcaoEvento', 'MDFeConsNaoEnc',
   'MDFeDistDFeInt', 'MDFeRecepcaoSinc'],
  [ LayMDFeRecepcao, LayMDFeRetRecepcao, LayMDFeConsulta,
    LayMDFeStatusServico, LayMDFeEvento, LayMDFeConsNaoEnc,
    LayMDFeDistDFeInt, LayMDFeRecepcaoSinc ] );
end;

function SchemaMDFeToStr(const t: TSchemaMDFe): String;
begin
  Result := GetEnumName(TypeInfo(TSchemaMDFe), Integer( t ) );
  Result := copy(Result, 4, Length(Result)); // Remove prefixo "sch"
end;

function StrToSchemaMDFe(const s: String): TSchemaMDFe;
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

  CodSchema := GetEnumValue(TypeInfo(TSchemaMDFe), SchemaStr );

  if CodSchema = -1 then
  begin
    raise Exception.Create(Format('"%s" não é um valor TSchemaMDFe válido.',[SchemaStr]));
  end;

  Result := TSchemaMDFe( CodSchema );
end;

function StrToVersaoMDFe(out ok: Boolean; const s: String): TVersaoMDFe;
begin
  Result := StrToEnumerado(ok, s, ['1.00', '3.00'], [ve100, ve300]);
end;

function VersaoMDFeToStr(const t: TVersaoMDFe): String;
begin
  Result := EnumeradoToStr(t, ['1.00', '3.00'], [ve100, ve300]);
end;

function DblToVersaoMDFe(out ok: Boolean; const d: Double): TVersaoMDFe;
begin
  ok := True;

  if d = 1.0 then
    Result := ve100
  else
  if d = 3.0 then
    Result := ve300
  else
  begin
    Result := ve100;
    ok := False;
  end;
end;

function VersaoMDFeToDbl(const t: TVersaoMDFe): Double;
begin
  case t of
    ve100: Result := 1.0;
    ve300: Result := 3.0;
  else
    Result := 0;
  end;
end;

function TTransportadorToStr(const t: TTransportadorMDFe): String;
begin
  Result := EnumeradoToStr(t, ['0', '1', '2', '3'], [ttNenhum, ttETC, ttTAC, ttCTC]);
end;

function StrToTTransportador(out ok: Boolean; const s: String): TTransportadorMDFe;
begin
  Result := StrToEnumerado(ok, s, ['0', '1', '2', '3'], [ttNenhum, ttETC, ttTAC, ttCTC]);
end;

function RspSeguroMDFeToStr(const t: TRspSegMDFe): String;
begin
  result := EnumeradoToStr(t, ['1', '2'],
                              [rsEmitente, rsTomadorServico]);
end;

function RspSeguroMDFeToStrText(const t: TRspSegMDFe): String;
begin
  result := EnumeradoToStr(t, ['EMITENTE', 'TOMADOR SERVICO'],
                              [rsEmitente, rsTomadorServico]);
end;

function StrToRspSeguroMDFe(out ok: boolean; const s: String ): TRspSegMDFe;
begin
  result := StrToEnumerado(ok, s, ['1', '2'],
                                  [rsEmitente, rsTomadorServico]);
end;

function TCargaToStr(const t: TCarga): String;
begin
  Result := EnumeradoToStr(t, ['01', '02', '03', '04', '05', '06', '07', '08',
                               '09', '10', '11'],
            [tcGranelSolido, tcGranelLiquido, tcFrigorificada, tcConteinerizada,
             tcCargaGeral, tcNeogranel, tcPerigosaGranelSolido,
             tcPerigosaGranelLiquido, tcPerigosaCargaFrigorificada,
             tcPerigosaConteinerizada, tcPerigosaCargaGeral]);
end;

function StrToTCarga(out ok: Boolean; const s: String): TCarga;
begin
  Result := StrToEnumerado(ok, s, ['01', '02', '03', '04', '05', '06', '07',
                                   '08', '09', '10', '11'],
            [tcGranelSolido, tcGranelLiquido, tcFrigorificada, tcConteinerizada,
             tcCargaGeral, tcNeogranel, tcPerigosaGranelSolido,
             tcPerigosaGranelLiquido, tcPerigosaCargaFrigorificada,
             tcPerigosaConteinerizada, tcPerigosaCargaGeral]);
end;

function TIndPagToStr(const t: TIndPag): String;
begin
  Result := EnumeradoToStr(t, ['0', '1', ''],
                              [ipVista, ipPrazo, ipNenhum]);
end;

function StrToTIndPag(out ok: Boolean; const s: String): TIndPag;
begin
  Result := StrToEnumerado(ok, s, ['0', '1', ''],
                                  [ipVista, ipPrazo, ipNenhum]);
end;

function TCompToStr(const t: TComp): String;
begin
  Result := EnumeradoToStr(t, ['01', '02', '03', '99'],
                             [tcValePedagio, tcImpostos, tcDespesas, tcOutros]);
end;

function StrToTComp(out ok: Boolean; const s: String): TComp;
begin
  Result := StrToEnumerado(ok, s, ['01', '02', '03', '99'],
                             [tcValePedagio, tcImpostos, tcDespesas, tcOutros]);
end;

function StrToTpEventoMDFe(out ok: boolean; const s: string): TpcnTpEvento;
begin
  Result := StrToEnumerado(ok, s,
            ['-99999', '110111', '110112', '110114', '110115', '110116'],
            [teNaoMapeado, teCancelamento, teEncerramento, teInclusaoCondutor,
             teInclusaoDFe, tePagamentoOperacao]);
end;

initialization
  RegisterStrToTpEventoDFe(StrToTpEventoMDFe, 'MDFe');

end.

