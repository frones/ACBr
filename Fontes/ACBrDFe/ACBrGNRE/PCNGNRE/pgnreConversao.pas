{******************************************************************************}
{ Projeto: Componente ACBrGNRE                                                 }
{  Biblioteca multiplataforma de componentes Delphi/Lazarus para emissão da    }
{  Guia Nacional de Recolhimento de Tributos Estaduais                         }
{  http://www.gnre.pe.gov.br/                                                  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 Claudemir Vitor Pereira                }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                       Juliomar Marchetti                     }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 09/12/2013 - Claudemir Vitor Pereira
|*  - Doação do componente para o Projeto ACBr
******************************************************************************}

{$I ACBr.inc}

unit pgnreConversao;

interface

uses
  SysUtils, StrUtils, Classes, pcnConversao;

type
  TStatusACBrGNRE = ( stGNREIdle, stGNRERecepcao, stGNRERetRecepcao,
                      stGNREConsulta, stGNREConsultaConfigUF, stGNREEmail,
                      stEnvioWebService );
                      
  TLayOutGNRE = ( LayGNRERecepcao, LayGNRERetRecepcao, LayGNREConsultaConfigUF );

  TVersaoGNRE = (ve100, ve200);

  TSchemaGNRE = ( schErro, schGNRE, schretGNRE, schprocGNRE, schconsReciGNRE );

  TTipoGNRE = ( tgSimples, tgMultiplosDoc, tgMultiplasReceitas );

function LayOutToServico(const t: TLayOutGNRE): String;
function ServicoToLayOut(out ok: Boolean; const s: String): TLayOutGNRE;

function LayOutToSchema(const t: TLayOutGNRE): TSchemaGNRE;

function SchemaGNREToStr(const t: TSchemaGNRE): String;

function VersaoGNREToStr(const t: TVersaoGNRE): String;
function VersaoGNREToDbl(const t: TVersaoGNRE): Double;
function StrToVersaoGNRe(out ok: Boolean; const s: String): TVersaoGNRE;

function TipoGNREToStr(const t: TTipoGNRE): String;
function StrToTipoGNRE(out ok: Boolean; const s: String): TTipoGNRE;

implementation

uses
  typinfo, ACBrUtil;

function LayOutToServico(const t: TLayOutGNRE): String;
begin
  Result := EnumeradoToStr(t,
    ['GNRERecepcao', 'GNRERetRecepcao', 'GNREConsultaConfigUF'],
    [LayGNRERecepcao, LayGNRERetRecepcao, LayGNREConsultaConfigUF]);
end;

function ServicoToLayOut(out ok: Boolean; const s: String): TLayOutGNRE;
begin
  Result := StrToEnumerado(ok, s,
    ['GNRERecepcao', 'GNRERetRecepcao', 'GNREConsultaConfigUF'],
    [LayGNRERecepcao, LayGNRERetRecepcao, LayGNREConsultaConfigUF]);
end;

function LayOutToSchema(const t: TLayOutGNRE): TSchemaGNRE;
begin
  case t of
    LayGNRERecepcao:         Result := schGNRE;
    LayGNRERetRecepcao:      Result := schretGNRE;
    LayGNREConsultaConfigUF: Result := schconsReciGNRE;
  else
    Result := schErro;
  end;
end;

function SchemaGNREToStr(const t: TSchemaGNRE): String;
begin
  Result := GetEnumName(TypeInfo(TSchemaGNRE), Integer( t ) );
  Result := copy(Result, 4, Length(Result)); // Remove prefixo "sch"
end;

function VersaoGNREToStr(const t: TVersaoGNRE): String;
begin
  Result := EnumeradoToStr(t, ['1.00', '2.00'], [ve100, ve200]);
end;

function VersaoGNREToDbl(const t: TVersaoGNRE): Double;
begin
  case t of
    ve100: Result := 1.0;
    ve200: Result := 2.0;
  else
    Result := 0;
  end;
end;

function StrToVersaoGNRe(out ok: Boolean; const s: String): TVersaoGNRE;
begin
  Result := StrToEnumerado(ok, s, ['1.00', '2.00'], [ve100, ve200]);
end;

function TipoGNREToStr(const t: TTipoGNRE): String;
begin
  Result := EnumeradoToStr(t,
    ['0', '1', '2'],
    [tgSimples, tgMultiplosDoc, tgMultiplasReceitas]);
end;

function StrToTipoGNRE(out ok: Boolean; const s: String): TTipoGNRE;
begin
  Result := StrToEnumerado(ok, s,
    ['0', '1', '2'],
    [tgSimples, tgMultiplosDoc, tgMultiplasReceitas]);
end;

end.

