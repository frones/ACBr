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

{******************************************************************************
|* Historico
|*
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
|* 28/08/2017: Leivio Fontenele - leivio@yahoo.com.br
|*  - Implementação comunicação, envelope, status e retorno do componente com webservice.
******************************************************************************}

{$I ACBr.inc}

unit ACBreSocialLoteEventos;

interface

uses
  Classes, 
	SysUtils, 
	StrUtils, 
	synautil,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
		System.Generics.Collections, 
		System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
		System.Contnrs,
  {$ELSE}
		Contnrs,
  {$IFEND}
  ACBrBase,
  pcnConversao, 
	pcnAuxiliar, 
	pcnLeitor, 
	pcnGerador,
  ACBreSocialConfiguracoes, 
	ACBreSocialEventos,
  pcesCommon, 
	pcesConversaoeSocial;

type

  TItemLoteEventos = class(TObject)
  private
    FACBreSocial : TComponent;
    FTipoEvento : TTipoEvento;
    FXML : AnsiString;
    FNomeArq : string;
    FLeitor : TLeitor;

    procedure SetXML(const Value: AnsiString);
    function GetIDEvento: string;
  public
    constructor Create(AOwner: TComponent); reintroduce; //overload;

    property IDEvento: string read GetIDEvento;
    property XML : AnsiString read FXML write SetXML;
    property Leitor : TLeitor read FLeitor write FLeitor;
    property TipoEvento: TTipoEvento read FTipoEvento write FTipoEvento;
    property NomeArq: string read FNomeArq write FNomeArq;

  end;

  TLoteEventos = class(TeSocialCollection)
  private
    FIdeEmpregador: TIdeEmpregador;
    FIdeTransmissor: TIdeTransmissor;
    FGerador: TGerador;
    FXML: string;

    function GetItem(Index: integer): TItemLoteEventos;
    procedure SetItem(Index: integer; const Value: TItemLoteEventos);
    procedure CarregarXmlEventos;
    function Validar: Boolean;
  public
    constructor Create(AACBreSocial: TComponent); override;
    destructor Destroy; override;

    function Add: TItemLoteEventos; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TItemLoteEventos;

    function LoadFromFile(const CaminhoArquivo: String): Boolean;
    function LoadFromStream(AStream: TStringStream): Boolean;
    function LoadFromString(AXMLString: String): Boolean;
    procedure GerarXML(const AGrupo: TeSocialGrupo);

    property Items[Index: Integer] : TItemLoteEventos read GetItem write SetItem;
    property IdeEmpregador : TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeTransmissor : TIdeTransmissor read FIdeTransmissor write FIdeTransmissor;
    property XML : String read FXML write FXML;
  end;

implementation

uses
  DateUtils,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  ACBreSocial;

{ TLoteEventos }

function TLoteEventos.Add: TItemLoteEventos;
begin
  Result := Self.New;
end;

procedure TLoteEventos.CarregarXmlEventos;
var
  i: Integer;
  FEventos: TEventos;
begin
  // Limpando
  Clear;

  FEventos := TACBreSocial(FACBreSocial).Eventos;

{Iniciais}
  {S1000}
  for i := 0 to FEventos.Iniciais.S1000.Count - 1 do
    LoadFromString(FEventos.Iniciais.S1000[i].evtInfoEmpregador.XML);
  {S1005}
  for i := 0 to FEventos.Iniciais.S1005.Count - 1 do
    LoadFromString(FEventos.Iniciais.S1005[i].evtTabEstab.XML);
{Iniciais}

{Tabelas}
  {S1010}
  for i := 0 to FEventos.Tabelas.S1010.Count - 1 do
    LoadFromString(FEventos.Tabelas.S1010[i].EvtTabRubrica.XML);
  {S1020}
  for i := 0 to FEventos.Tabelas.S1020.Count - 1 do
    LoadFromString(FEventos.Tabelas.S1020[i].EvtTabLotacao.XML);

  {S1030}
  for i := 0 to FEventos.Tabelas.S1030.Count - 1 do
    LoadFromString(FEventos.Tabelas.S1030[i].EvtTabCargo.XML);
  {S1035}
  for i := 0 to FEventos.Tabelas.S1035.Count - 1 do
    LoadFromString(FEventos.Tabelas.S1035[i].evtTabCarreira.XML);
  {S1040}
  for i := 0 to FEventos.Tabelas.S1040.Count - 1 do
    LoadFromString(FEventos.Tabelas.S1040[i].EvtTabFuncao.XML);
  {S1050}
  for i := 0 to FEventos.Tabelas.S1050.Count - 1 do
    LoadFromString(FEventos.Tabelas.S1050[i].EvtTabHorContratual.XML);
  {S1060}
  for i := 0 to FEventos.Tabelas.S1060.Count - 1 do
    LoadFromString(FEventos.Tabelas.S1060[i].EvtTabAmbiente.XML);
  {S1070}
  for i := 0 to FEventos.Tabelas.S1070.Count - 1 do
    LoadFromString(FEventos.Tabelas.S1070[i].EvtTabProcesso.XML);
  {S1080}
  for i := 0 to FEventos.Tabelas.S1080.Count - 1 do
    LoadFromString(FEventos.Tabelas.S1080[i].EvtTabOperPortuario.XML);
{Tabelas}

{NaoPeriodicos}
  {S2190}
  for i := 0 to FEventos.NaoPeriodicos.S2190.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2190[i].EvtAdmPrelim.XML);
  {S2200}
  for i := 0 to FEventos.NaoPeriodicos.S2200.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2200[i].EvtAdmissao.XML);
  {S2205}
  for i := 0 to FEventos.NaoPeriodicos.S2205.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2205[i].EvtAltCadastral.XML);
  {S2206}
  for i := 0 to FEventos.NaoPeriodicos.S2206.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2206[i].EvtAltContratual.XML);
  {S2210}
  for i := 0 to FEventos.NaoPeriodicos.S2210.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2210[i].EvtCAT.XML);
  {S2220}
  for i := 0 to FEventos.NaoPeriodicos.S2220.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2220[i].evtMonit.XML);
  {S2221}
  for i := 0 to FEventos.NaoPeriodicos.S2221.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2221[i].evtToxic.XML);
  {S2230}
  for i := 0 to FEventos.NaoPeriodicos.S2230.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2230[i].EvtAfastTemp.XML);
  {S2231}
  for i := 0 to FEventos.NaoPeriodicos.S2231.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2231[i].EvtCessao.XML);
  {S2240}
  for i := 0 to FEventos.NaoPeriodicos.S2240.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2240[i].EvtExpRisco.XML);
  {S2245}
  for i := 0 to FEventos.NaoPeriodicos.S2245.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2245[i].EvtTreiCap.XML);
  {S2250}
  for i := 0 to FEventos.NaoPeriodicos.S2250.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2250[i].EvtAvPrevio.XML);
  {S2260}
  for i := 0 to FEventos.NaoPeriodicos.S2260.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2260[i].EvtConvInterm.XML);
  {S2298}
  for i := 0 to FEventos.NaoPeriodicos.S2298.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2298[i].EvtReintegr.XML);
  {S2299}
  for i := 0 to FEventos.NaoPeriodicos.S2299.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2299[i].EvtDeslig.XML);
  {S2300}
  for i := 0 to FEventos.NaoPeriodicos.S2300.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2300[i].EvtTSVInicio.XML);
  {S2306}
  for i := 0 to FEventos.NaoPeriodicos.S2306.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2306[i].EvtTSVAltContr.XML);
  {S2399}
  for i := 0 to FEventos.NaoPeriodicos.S2399.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2399[i].EvtTSVTermino.XML);
  {S2400}
  for i := 0 to FEventos.NaoPeriodicos.S2400.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2400[i].evtCdBenefIn.XML);
  {S2405}
  for i := 0 to FEventos.NaoPeriodicos.S2405.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2405[i].EvtCdBenefAlt.XML);
  {S2410}
  for i := 0 to FEventos.NaoPeriodicos.S2410.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2410[i].EvtCdBenIn.XML);
  {S2416}
  for i := 0 to FEventos.NaoPeriodicos.S2416.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2416[i].EvtCdBenAlt.XML);
  {S2418}
  for i := 0 to FEventos.NaoPeriodicos.S2418.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2418[i].EvtReativBen.XML);
  {S2420}
  for i := 0 to FEventos.NaoPeriodicos.S2420.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2420[i].EvtCdBenTerm.XML);
  {S2500}
  for i := 0 to FEventos.NaoPeriodicos.S2500.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2500[i].EvtProcTrab.XML);
  {S2501}
  for i := 0 to FEventos.NaoPeriodicos.S2501.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S2501[i].EvtContProc.XML);
  {S3000}
  for i := 0 to FEventos.NaoPeriodicos.S3000.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S3000[i].EvtExclusao.XML);
  {S3500}
  for i := 0 to FEventos.NaoPeriodicos.S3500.Count - 1 do
    LoadFromString(FEventos.NaoPeriodicos.S3500[i].EvtExcProcTrab.XML);
{NaoPeriodicos}

{Periodicos}
  {S1200}
  for i := 0 to FEventos.Periodicos.S1200.Count - 1 do
    LoadFromString(FEventos.Periodicos.S1200[i].EvtRemun.XML);
  {S1202}
  for i := 0 to FEventos.Periodicos.S1202.Count - 1 do
    LoadFromString(FEventos.Periodicos.S1202[i].EvtRmnRPPS.XML);
  {S1207}
  for i := 0 to FEventos.Periodicos.S1207.Count - 1 do
    LoadFromString(FEventos.Periodicos.S1207[i].EvtBenPrRP.XML);
  {S1210}
  for i := 0 to FEventos.Periodicos.S1210.Count - 1 do
    LoadFromString(FEventos.Periodicos.S1210[i].EvtPgtos.XML);
  {S1220}
  for i := 0 to FEventos.Periodicos.S1220.Count - 1 do
    LoadFromString(FEventos.Periodicos.S1220[i].EvtInfoIR.XML);
  {S1250}
  for i := 0 to FEventos.Periodicos.S1250.Count - 1 do
    LoadFromString(FEventos.Periodicos.S1250[i].EvtAqProd.XML);
  {S1260}
  for i := 0 to FEventos.Periodicos.S1260.Count - 1 do
    LoadFromString(FEventos.Periodicos.S1260[i].EvtComProd.XML);
  {S1270}
  for i := 0 to FEventos.Periodicos.S1270.Count - 1 do
    LoadFromString(FEventos.Periodicos.S1270[i].EvtContratAvNP.XML);
  {S1280}
  for i := 0 to FEventos.Periodicos.S1280.Count - 1 do
    LoadFromString(FEventos.Periodicos.S1280[i].EvtInfoComplPer.XML);
  {S1295}
  for i := 0 to FEventos.Periodicos.S1295.Count - 1 do
    LoadFromString(FEventos.Periodicos.S1295[i].EvtTotConting.XML);
  {S1298}
  for i := 0 to FEventos.Periodicos.S1298.Count - 1 do
    LoadFromString(FEventos.Periodicos.S1298[i].EvtReabreEvPer.XML);
  {S1299}
  for i := 0 to FEventos.Periodicos.S1299.Count - 1 do
    LoadFromString(FEventos.Periodicos.S1299[i].EvtFechaEvPer.XML);
  {S1300}
  for i := 0 to FEventos.Periodicos.S1300.Count - 1 do
    LoadFromString(FEventos.Periodicos.S1300[i].EvtContrSindPatr.XML);
{Periodicos}
end;

procedure TLoteEventos.GerarXML(const AGrupo: TeSocialGrupo);
var
  i: Integer;
  EventosXml: String;
begin
  CarregarXmlEventos;

  EventosXml := EmptyStr;

  FXML :=
  '<eSocial xmlns="http://www.esocial.gov.br/schema/lote/eventos/envio/v1_1_1">'+
    '<envioLoteEventos grupo="' + Inttostr(ord(AGrupo)) + '">'+
      '<ideEmpregador>'+
        '<tpInsc>' + eSTpInscricaoToStr(FIdeEmpregador.TpInsc) + '</tpInsc>'+
        '<nrInsc>' + IIf((FIdeEmpregador.TpInsc <> tiCNPJ) or (FIdeEmpregador.OrgaoPublico), FIdeEmpregador.NrInsc, Copy(FIdeEmpregador.NrInsc, 1, 8)) +'</nrInsc>'+
      '</ideEmpregador>'+
      '<ideTransmissor>'+
        '<tpInsc>' + eSTpInscricaoToStr(FIdeTransmissor.TpInsc) +'</tpInsc>'+
        '<nrInsc>' + FIdeTransmissor.NrInsc +'</nrInsc>'+
    '</ideTransmissor>'+
    '<eventos>';

   for i := 0 to Self.Count - 1 do
     EventosXml := EventosXml +
                   '<evento Id="' + Self.Items[i].IDEvento +'"> ' +
                     RemoverDeclaracaoXML(Self.Items[i].XML) +
                   '</evento>';

  FXML := FXML + EventosXml;
  FXML := FXML +
              '</eventos>'+
            '</envioLoteEventos>'+
          '</eSocial>';

  FXML := AnsiToUtf8(FXML);
  Validar;
end;

function TLoteEventos.GetItem(Index: integer): TItemLoteEventos;
begin
  Result := TItemLoteEventos(inherited Items[Index]);
end;

function TLoteEventos.LoadFromFile(const CaminhoArquivo: String): Boolean;
var
  ArquivoXML: TStringList;
  XMLTemp: String;
  XMLOriginal: AnsiString;
  i: integer;
begin
  Result := True;

  ArquivoXML := TStringList.Create;
  try
    ArquivoXML.LoadFromFile(CaminhoArquivo);
    XMLOriginal := ArquivoXML.Text;

    // Converte de UTF8 para a String nativa da IDE //
    XMLTemp := DecodeToString(XMLOriginal, True);
    LoadFromString(XMLTemp);

    for i := 0 to Self.Count - 1 do
      Self.Items[i].NomeArq := CaminhoArquivo;
  finally
    ArquivoXML.Free;
  end;
end;

function TLoteEventos.LoadFromStream(AStream: TStringStream): Boolean;
var
  XMLOriginal: AnsiString;
begin
  AStream.Position := 0;
  XMLOriginal := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(XMLOriginal));
end;

function TLoteEventos.LoadFromString(AXMLString: String): Boolean;
var
//  AXML: AnsiString;
  AXML: String;
  P: integer;

  function PoseSocial: integer;
  begin
    Result := pos('</eSocial>', AXMLString);
  end;

begin
  P := PoseSocial;

  while P > 0 do
  begin
    AXML := copy(AXMLString, 1, P + 9);
    AXMLString := Trim(copy(AXMLString, P + 10, length(AXMLString)));

    Self.New.FXML := AXML;

    P := PoseSocial;
  end;

  Result := Self.Count > 0;
end;

procedure TLoteEventos.SetItem(Index: integer; const Value: TItemLoteEventos);
begin
  inherited Items[Index] := Value;
end;

function TLoteEventos.Validar: Boolean;
var
  Erro : String;
  EhValido: Boolean;
begin
  with TACBreSocial(FACBreSocial) do
  begin
    EhValido := SSL.Validar(FXML, Configuracoes.Arquivos.PathSchemas+'EnvioLoteEventos-v1_1_1.xsd', Erro);
    if not EhValido and Configuracoes.Geral.ExibirErroSchema then
      raise EACBreSocialException.CreateDef(ACBrStr('Houve erro na validação do Lote: ') + Erro);
  end;
  
  result := EhValido;
end;

function TLoteEventos.New: TItemLoteEventos;
begin
  Result := TItemLoteEventos.Create(FACBreSocial);
  Self.Add(Result);
end;

constructor TLoteEventos.Create(AACBreSocial: TComponent);
begin
  inherited Create(AACBreSocial);

  FIdeEmpregador  := TIdeEmpregador.Create;
  FIdeTransmissor := TIdeTransmissor.Create;
  FGerador        := TGerador.Create;
end;

destructor TLoteEventos.Destroy;
begin
  FIdeEmpregador.Free;
  FIdeTransmissor.Free;
  FGerador.Free;

  inherited;
end;

{ TItemLoteEventos }

constructor TItemLoteEventos.Create(AOwner: TComponent);
begin
  inherited Create;
  FACBreSocial := AOwner;
  FLeitor      := TLeitor.Create;
  FXML         := '';
end;

function TItemLoteEventos.GetIDEvento: string;
var
  Ini: Integer;
begin
  Result := EmptyStr;
  Ini := pos('Id=', XML);
  if ini > 0 then
    Result := 'ID' + OnlyNumber(Copy(XML, Ini + 4, 38));
end;

procedure TItemLoteEventos.SetXML(const Value: AnsiString);
var
  Stream: TStringStream;
begin
  FXML := Value;
  Stream := TStringStream.Create(value);
  try
    FLeitor.CarregarArquivo(Stream);
  finally
    Stream.Free;
  end;
end;

end.
