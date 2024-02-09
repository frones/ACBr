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

unit pcaConversao;

interface

uses
  SysUtils, StrUtils, Classes;

type
  TVersaoANe     = (ve200);

  TLayOutANe     = (LayANeAverbacao, LayANeRetAverbacao);

  TSchemaANe     = (schErro, schAverbacao, schAverbacaoRetorno);

  TStatusACBrANe = (stANeIdle, stANeAverbacao,stANeRetAverbacao,
                    stANeEmail, stANeEnvioWebService);

  TTipoDoc       = (tdNFe, tdCTe, tdMDFe, tdAddBackMail);

  TSeguradora    = (tsATM, tsELT);

const
  NAME_SPACE_ANe  = 'xmlns:urn="ATMWebSvr"';

function LayOutToSchema(const t: TLayOutANe): TSchemaANe;

function LayOutToServico(const t: TLayOutANe): String;
function ServicoToLayOut(out ok: Boolean; const s: String): TLayOutANe;

function SchemaANeToStr(const t: TSchemaANe): String;
function StrToSchemaANe(const s: String): TSchemaANe;

function StrToVersaoANe(out ok: Boolean; const s: String): TVersaoANe;
function VersaoANeToStr(const t: TVersaoANe): String;

function DblToVersaoANe(out ok: Boolean; const d: Double): TVersaoANe;
function VersaoANeToDbl(const t: TVersaoANe): Double;

implementation

uses
  typinfo, pcnConversao;

function LayOutToSchema(const t: TLayOutANe): TSchemaANe;
begin
  case t of
    LayANeAverbacao:    Result := schAverbacao;
    LayANeRetAverbacao: Result := schAverbacaoRetorno;
  else
    Result := schErro;
  end;
end;

function LayOutToServico(const t: TLayOutANe): String;
begin
  Result := EnumeradoToStr(t,
    ['ANeAverbacao', 'ANeRetAverbacao'],
    [LayANeAverbacao, LayANeRetAverbacao] );
end;

function ServicoToLayOut(out ok: Boolean; const s: String): TLayOutANe;
begin
  Result := StrToEnumerado(ok, s,
  ['ANeAverbacao', 'ANeRetAverbacao'],
  [LayANeAverbacao, LayANeRetAverbacao] );
end;

function SchemaANeToStr(const t: TSchemaANe): String;
begin
  Result := GetEnumName(TypeInfo(TSchemaANe), Integer( t ) );
  Result := copy(Result, 4, Length(Result)); // Remove prefixo "sch"
end;

function StrToSchemaANe(const s: String): TSchemaANe;
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

  CodSchema := GetEnumValue(TypeInfo(TSchemaANe), SchemaStr );

  if CodSchema = -1 then
  begin
    raise Exception.Create(Format('"%s" não é um valor TSchemaANe válido.',[SchemaStr]));
  end;

  Result := TSchemaANe( CodSchema );
end;

function StrToVersaoANe(out ok: Boolean; const s: String): TVersaoANe;
begin
  Result := StrToEnumerado(ok, s, ['2.00'], [ve200]);
end;

function VersaoANeToStr(const t: TVersaoANe): String;
begin
  Result := EnumeradoToStr(t, ['2.00'], [ve200]);
end;

function DblToVersaoANe(out ok: Boolean; const d: Double): TVersaoANe;
begin
  ok := True;

  if d = 2.0 then
    Result := ve200
  else
  begin
    Result := ve200;
    ok := False;
  end;
end;

function VersaoANeToDbl(const t: TVersaoANe): Double;
begin
  case t of
    ve200: Result := 2.0;
  else
    Result := 0;
  end;
end;

end.

