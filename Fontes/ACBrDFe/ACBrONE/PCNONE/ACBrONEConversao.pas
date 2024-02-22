{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrONEConversao;

interface

uses
  SysUtils, StrUtils, Classes,
  pcnConversao;

type
  TStatusACBrONE = (stONEIdle, stManutencao, stRecepcaoLeitura,
                    stDistLeitura, stConsFoto, stConsPlaca);

  TSchemaONE = (schErro, schONEManutencaoEQP, schONERecepcaoLeitura,
                schONEDistLeitura, schONEConsFoto, schONEConsPorPlaca);

  TVersaoONE = (ve200);

const
  TVersaoONEArrayStrings: array[TVersaoONE] of string = ('2.00');

type
  TLayOutONE = (LayManutencao, LayRecepcaoLeitura, LayDistLeitura, LayConsFoto,
                LayConsPlaca);

const
  TLayOutONEArrayStrings: array[TLayOutONE] of string = ('ONEManutencaoEQP',
    'ONERecepcaoLeitura', 'ONEDistLeitura', 'ONEConsFoto', 'ONEConsPorPlaca');

type
  TtpMan = (tmCadastramento, tmAlteracao, tmDesativacao, tmReativacao);

const
  TtpManArrayStrings: array[TtpMan] of string = ('1', '2', '3', '4');

type
  TtpSentido = (tsEntrada, tsSaida, tsIndeterminado);

const
  TtpSentidoArrayStrings: array[TtpSentido] of string = ('E', 'S', 'I');

type
  TtpEQP = (teSLD, teOCR);

const
  TtpEQPArrayStrings: array[TtpEQP] of string = ('1', '2');

type
  TtpTransm = (ttNormal, ttRetransmissao, ttAtrasoProcessamento);

const
  TtpTransmArrayStrings: array[TtpTransm] of string = ('N', 'R', 'A');

type
  TtpVeiculo = (tvCarga, tvPassageiro, tvPasseio);

const
  TtpVeiculoArrayStrings: array[TtpVeiculo] of string = ('1', '2', '3');

type
  TtpDist = (tdUFMDFe, tdEquipamento, tdOperador, tdUFCaptura);

const
  TtpDistArrayStrings: array[TtpDist] of string = ('1', '2', '3', '4');

type
  TtpLeitura = (tlSLD, tlOCR);

const
  TtpLeituraArrayStrings: array[TtpLeitura] of string = ('1', '2');

{
  Declaração das funções de conversão
}
function StrToVersaoONE(const s: string): TVersaoONE;
function VersaoONEToStr(const t: TVersaoONE): string;

function DblToVersaoONE(const d: Real): TVersaoONE;
function VersaoONEToDbl(const t: TVersaoONE): Real;

function LayOutToSchema(const t: TLayOutONE): TSchemaONE;

function SchemaONEToStr(const t: TSchemaONE): string;
function StrToSchemaONE(const s: string): TSchemaONE;

function LayOutONEToServico(const t: TLayOutONE): string;
function ServicoToLayOutONE(const s: string): TLayOutONE;

function StrToTpEventoONE(out ok: boolean; const s: string): TpcnTpEvento;

function tpManToStr(const t: TtpMan): string;
function StrTotpMan(const s: string): TtpMan;

function tpSentidoToStr(const t: TtpSentido): string;
function StrTotpSentido(const s: string): TtpSentido;

function tpEQPToStr(const t: TtpEQP): string;
function StrTotpEQP(const s: string): TtpEQP;

function tpTransmToStr(const t: TtpTransm): string;
function StrTotpTransm(const s: string): TtpTransm;

function tpVeiculoToStr(const t: TtpVeiculo): string;
function StrTotpVeiculo(const s: string): TtpVeiculo;

function tpDistToStr(const t: TtpDist): string;
function StrTotpDist(const s: string): TtpDist;

function tpLeituraToStr(const t: TtpLeitura): string;
function StrTotpLeitura(const s: string): TtpLeitura;

implementation

uses
  typinfo,
  ACBrBase;

function StrToVersaoONE(const s: string): TVersaoONE;
var
  idx: TVersaoONE;
begin
  for idx := Low(TVersaoONEArrayStrings) to High(TVersaoONEArrayStrings) do
  begin
    if (TVersaoONEArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TVersaoONE: %s', [s]);
end;

function VersaoONEToStr(const t: TVersaoONE): string;
begin
  result := TVersaoONEArrayStrings[t];
end;

function DblToVersaoONE(const d: Real): TVersaoONE;
begin
  if (d = 2.0)  then
    Result := ve200
  else
  begin
    Result := ve200;
  end;
end;

function VersaoONEToDbl(const t: TVersaoONE): Real;
begin
  case t of
    ve200: Result := 2.00;
  else
    Result := 0;
  end;
end;

function LayOutToSchema(const t: TLayOutONE): TSchemaONE;
begin
  case t of
    LayManutencao:       Result := schONEManutencaoEQP;
    LayRecepcaoLeitura:  Result := schONERecepcaoLeitura;
    LayDistLeitura:      Result := schONEDistLeitura;
    LayConsFoto:         Result := schONEConsFoto;
    LayConsPlaca:        Result := schONEConsPorPlaca;
  else
    Result := schErro;
  end;
end;

function SchemaONEToStr(const t: TSchemaONE): string;
begin
  Result := GetEnumName(TypeInfo(TSchemaONE), Integer( t ) );
  Result := copy(Result, 4, Length(Result)); // Remove prefixo "sch"
end;

function StrToSchemaONE(const s: string): TSchemaONE;
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

  CodSchema := GetEnumValue(TypeInfo(TSchemaONE), SchemaStr );

  if CodSchema = -1 then
  begin
    raise Exception.Create(Format('"%s" não é um valor TSchemaANe válido.',[SchemaStr]));
  end;

  Result := TSchemaONE( CodSchema );
end;

function LayOutONEToServico(const t: TLayOutONE): string;
begin
  result := TLayOutONEArrayStrings[t];
end;

function ServicoToLayOutONE(const s: string): TLayOutONE;
var
  idx: TLayOutONE;
begin
  for idx := Low(TLayOutONEArrayStrings) to High(TLayOutONEArrayStrings) do
  begin
    if (TLayOutONEArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TLayOutONE: %s', [s]);
end;

function StrToTpEventoONE(out ok: boolean; const s: string): TpcnTpEvento;
begin
  Result := StrToEnumerado(ok, s,
            ['-99999'],
            [teNaoMapeado]);
end;

function tpManToStr(const t: TtpMan): string;
begin
  result := TtpManArrayStrings[t];
end;

function StrTotpMan(const s: string): TtpMan;
var
  idx: TtpMan;
begin
  for idx := Low(TtpManArrayStrings) to High(TtpManArrayStrings) do
  begin
    if (TtpManArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpMan: %s', [s]);
end;

function tpSentidoToStr(const t: TtpSentido): string;
begin
  result := TtpSentidoArrayStrings[t];
end;

function StrTotpSentido(const s: string): TtpSentido;
var
  idx: TtpSentido;
begin
  for idx := Low(TtpSentidoArrayStrings) to High(TtpSentidoArrayStrings) do
  begin
    if (TtpSentidoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpSentido: %s', [s]);
end;

function tpEQPToStr(const t: TtpEQP): string;
begin
  result := TtpEQPArrayStrings[t];
end;

function StrTotpEQP(const s: string): TtpEQP;
var
  idx: TtpEQP;
begin
  for idx := Low(TtpEQPArrayStrings) to High(TtpEQPArrayStrings) do
  begin
    if (TtpEQPArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpEQP: %s', [s]);
end;

function tpTransmToStr(const t: TtpTransm): string;
begin
  result := TtpTransmArrayStrings[t];
end;

function StrTotpTransm(const s: string): TtpTransm;
var
  idx: TtpTransm;
begin
  for idx := Low(TtpTransmArrayStrings) to High(TtpTransmArrayStrings) do
  begin
    if (TtpTransmArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpTransm: %s', [s]);
end;

function tpVeiculoToStr(const t: TtpVeiculo): string;
begin
  result := TtpVeiculoArrayStrings[t];
end;

function StrTotpVeiculo(const s: string): TtpVeiculo;
var
  idx: TtpVeiculo;
begin
  for idx := Low(TtpVeiculoArrayStrings) to High(TtpVeiculoArrayStrings) do
  begin
    if (TtpVeiculoArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpVeiculo: %s', [s]);
end;

function tpDistToStr(const t: TtpDist): string;
begin
  result := TtpDistArrayStrings[t];
end;

function StrTotpDist(const s: string): TtpDist;
var
  idx: TtpDist;
begin
  for idx := Low(TtpDistArrayStrings) to High(TtpDistArrayStrings) do
  begin
    if (TtpDistArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpDist: %s', [s]);
end;

function tpLeituraToStr(const t: TtpLeitura): string;
begin
  result := TtpLeituraArrayStrings[t];
end;

function StrTotpLeitura(const s: string): TtpLeitura;
var
  idx: TtpLeitura;
begin
  for idx := Low(TtpLeituraArrayStrings) to High(TtpLeituraArrayStrings) do
  begin
    if (TtpLeituraArrayStrings[idx] = s) then
    begin
      result := idx;
      exit;
    end;
  end;

  raise EACBrException.CreateFmt('Valor string inválido para TtpLeitura: %s', [s]);
end;

initialization

  RegisterStrToTpEventoDFe(StrToTpEventoONE, 'ONE');

end.

