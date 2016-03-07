{******************************************************************************}
{ Projeto: Componente ACBrMDFe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
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

{*******************************************************************************
|* Historico
|*
|* 01/08/2012: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit pmdfeConversaoMDFe;

interface

uses
  SysUtils, StrUtils, Classes;

type
  TTpEmitenteMDFe = (teTransportadora, teTranspCargaPropria);
  TModalMDFe      = (moRodoviario, moAereo, moAquaviario, moFerroviario);
  TVersaoMDFe     = (ve100, ve100a);

  TLayOutMDFe     = (LayMDFeRecepcao, LayMDFeRetRecepcao, LayMDFeConsulta,
                     LayMDFeStatusServico, LayMDFeEvento, LayMDFeConsNaoEnc,
                     LayMDFeDistDFeInt);

  TSchemaMDFe     = (schErro, schMDFe, schEventoMDFe,
                     schresMDFe, schresEvento, schprocMDFe, schprocEventoMDFe,
                     schconsReciMDFe, schconsSitMDFe, schconsStatServ,
                     schmdfeModalAereo, schmdfeModalAquaviario,
                     schmdfeModalFerroviario, schmdfeModalRodoviario,
                     schevCancMDFe, schevEncMDFe, schevIncCondutorMDFe,
                     schdistDFeInt, schconsMDFeNaoEnc);

  TStatusACBrMDFe = (stMDFeIdle, stMDFeStatusServico, stMDFeRecepcao, stMDFeRetRecepcao,
                     stMDFeConsulta, stMDFeRecibo, stMDFeEmail, stMDFeEvento,
                     stMDFeDistDFeInt, stMDFeEnvioWebService);

const

  MDFeModalRodo    = '1.00';
  MDFeModalAereo   = '1.00';
  MDFeModalAqua    = '1.00';
  MDFeModalFerro   = '1.00';
  MDFeModalDuto    = '1.00';

  DSC_NMDF        = 'Número do Manifesto';
  DSC_CMDF        = 'Código numérico que compõe a Chave de Acesso';
  DSC_TPEMIT      = 'Tipo do Emitente';
  DSC_CMUNCARREGA = 'Código do Município de Carregamento';
  DSC_XMUNCARREGA = 'Nome do Município de Carregamento';
  DSC_UFPER       = 'Sigla da UF do percurso do veículo';
  DSC_SEGCODBARRA = 'Segundo código de barras';
  DSC_NCT         = 'Número do CT';
  DSC_SUBSERIE    = 'Subsérie do CT';
  DSC_PIN         = 'PIN SUFRAMA';
  DSC_QCTE        = 'Quantidade total de CTe relacionados no Manifesto';
  DSC_QCT         = 'Quantidade total de CT relacionados no Manifesto';
  DSC_QNFE        = 'Quantidade total de NFe relacionados no Manifesto';
  DSC_QNF         = 'Quantidade total de NF relacionados no Manifesto';
  DSC_QCARGA      = 'Peso Bruto Total da Carga / Mercadoria Transportada';
  DSC_DHINIVIAGEM = 'Data e Hora previstas de Inicio da Viagem';

  // Rodoviário
  DSC_CIOT        = 'Código Identificador da Operação de Transporte';
  DSC_CINTV       = 'Código interno do veículo';
  DSC_TARA        = 'Tara em KG';
  DSC_CAPKG       = 'Capacidade em KG';
  DSC_CAPM3       = 'Capacidade em m3';
  DSC_CNPJFORN    = 'CNPJ da empresa fornecedora do Vale-Pedágio';
  DSC_CNPJPG      = 'CNPJ do responsável pelo pagamento do Vale-Pedágio';
  DSC_NCOMPRA     = 'Número do comprovante de compra';
  DSC_CODAGPORTO  = 'Código de Agendamento no Porto';

  // Aéreo
  DSC_NAC         = 'Marca da Nacionalidade da Aeronave';
  DSC_MATR        = 'Marca da Matricula da Aeronave';
  DSC_NVOO        = 'Número do Vôo';
  DSC_CAEREMB     = 'Aeródromo de Embarque';
  DSC_CAERDES     = 'Aeródromo de Destino';
  DSC_DVOO        = 'Data do Vôo';

  // Aquaviário
  DSC_CNPJAGENAV  = 'CNPJ da Agência de Navegação';
  DSC_TPEMB       = 'Tipo de Embarcação';
  DSC_CEMBAR      = 'Código da Embarcação';
  DSC_XEMBAR      = 'Nome da Embarcação';
  DSC_NVIAG       = 'Número da Viagem';
  DSC_CPRTEMB     = 'Código do Porto de Embarque';
  DSC_CPRTDEST    = 'Código do Porto de Destino';
  DSC_CTERMCARREG = 'Código do Terminal de Carregamento';
  DSC_XTERMCARREG = 'Nome do Terminal de Carregamento';
  DSC_CTERMDESCAR = 'Código do Terminal de Descarregamento';
  DSC_XTERMDESCAR = 'Nome do Terminal de Descarregamento';
  DSC_CEMBCOMB    = 'Código da Embarcação do comboio';
  
  // Ferroviário
  DSC_XPREF       = 'Prefixo do Trem';
  DSC_DHTREM      = 'Data e Hora de liberação do Trem na origem';
  DSC_XORI        = 'Origem do Trem';
  DSC_XDEST       = 'Destino do Trem';
  DSC_QVAG        = 'Quantidade de vagões carregados';
  DSC_NVAG        = 'Número de Identificação do vagão';
  DSC_NSEQ        = 'Sequência do vagão na composição';
  DSC_TU          = 'Tonelada Útil';


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
function StrToSchemaMDFe(out ok: Boolean; const s: String): TSchemaMDFe;

function StrToVersaoMDFe(out ok: Boolean; const s: String): TVersaoMDFe;
function VersaoMDFeToStr(const t: TVersaoMDFe): String;

function DblToVersaoMDFe(out ok: Boolean; const d: Double): TVersaoMDFe;
function VersaoMDFeToDbl(const t: TVersaoMDFe): Double;

implementation

uses
  pcnConversao, typinfo;

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
                           ['1', '2'],
                           [teTransportadora, teTranspCargaPropria]);
end;

function StrToTpEmitente(out ok: Boolean; const s: String): TTpEmitenteMDFe;
begin
  result := StrToEnumerado(ok, s,
                           ['1', '2'],
                           [teTransportadora, teTranspCargaPropria]);
end;

function LayOutToSchema(const t: TLayOutMDFe): TSchemaMDFe;
begin
  case t of
    LayMDFeRecepcao:       Result := schMDFe;
    LayMDFeRetRecepcao:    Result := schconsReciMDFe;
    LayMDFeConsulta:       Result := schconsSitMDFe;
    LayMDFeStatusServico:  Result := schconsStatServ;
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
    ve100,
    ve100a: begin
              case AModal of
                moRodoviario:  result := '1.00';
                moAereo:       result := '1.00';
                moAquaviario:  result := '1.00';
                moFerroviario: result := '1.00';
              end;
            end;
  end;
end;

function LayOutToServico(const t: TLayOutMDFe): String;
begin
  Result := EnumeradoToStr(t,
    ['MDFeRecepcao', 'MDFeRetRecepcao', 'MDFeConsultaProtocolo',
     'MDFeStatusServico', 'RecepcaoEvento', 'MDFeConsNaoEnc',
     'MDFeDistDFeInt'],
    [ LayMDFeRecepcao, LayMDFeRetRecepcao, LayMDFeConsulta,
      LayMDFeStatusServico, LayMDFeEvento, LayMDFeConsNaoEnc,
      LayMDFeDistDFeInt ] );
end;

function ServicoToLayOut(out ok: Boolean; const s: String): TLayOutMDFe;
begin
  Result := StrToEnumerado(ok, s,
  ['MDFeRecepcao', 'MDFeRetRecepcao', 'MDFeConsultaProtocolo',
   'MDFeStatusServico', 'RecepcaoEvento', 'MDFeConsNaoEnc',
   'MDFeDistDFeInt'],
  [ LayMDFeRecepcao, LayMDFeRetRecepcao, LayMDFeConsulta,
    LayMDFeStatusServico, LayMDFeEvento, LayMDFeConsNaoEnc,
    LayMDFeDistDFeInt ] );
end;

function SchemaMDFeToStr(const t: TSchemaMDFe): String;
begin
  Result := GetEnumName(TypeInfo(TSchemaMDFe), Integer( t ) );
  Result := copy(Result, 4, Length(Result)); // Remove prefixo "sch"
end;

function StrToSchemaMDFe(out ok: Boolean; const s: String): TSchemaMDFe;
var
  P: Integer;
  SchemaStr: String;
begin
  P := pos('_', s);
  if P > 0 then
    SchemaStr := copy(s, 1, P-1)
  else
    SchemaStr := s;

  if LeftStr(SchemaStr, 3) <> 'sch' then
    SchemaStr := 'sch' + SchemaStr;

  Result := TSchemaMDFe( GetEnumValue(TypeInfo(TSchemaMDFe), SchemaStr ) );
end;

function StrToVersaoMDFe(out ok: Boolean; const s: String): TVersaoMDFe;
begin
  Result := StrToEnumerado(ok, s, ['1.00', '1.00'], [ve100, ve100a]);
end;

function VersaoMDFeToStr(const t: TVersaoMDFe): String;
begin
  Result := EnumeradoToStr(t, ['1.00', '1.00'], [ve100, ve100a]);
end;

function DblToVersaoMDFe(out ok: Boolean; const d: Double): TVersaoMDFe;
begin
  ok := True;

  if d = 1.0 then
    Result := ve100
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
    ve100a: Result := 1.0;
  else
    Result := 0;
  end;
end;

end.

