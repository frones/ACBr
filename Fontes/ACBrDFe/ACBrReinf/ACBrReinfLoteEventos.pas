{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Leivio Ramos de Fontenele                       }
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

unit ACBrReinfLoteEventos;

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
  {$IFEND}
  ACBrBase, 
  ACBrUtil.Strings, 
  ACBrUtil.XMLHTML,
  pcnConversao, 
  pcnAuxiliar, 
  pcnLeitor, 
  pcnGerador,
  ACBrReinfConfiguracoes, 
  ACBrReinfEventos, 
  pcnConsts,
  pcnCommonReinf, 
  pcnConversaoReinf;

type
  TItemLoteEventosClass = class of TItemLoteEventos;

  TItemLoteEventos = class(TObject)
  private
    FACBrReinf: TComponent;
    FTipoEvento: TTipoEvento;
    FXML: String;
    FNomeArq: string;
    FLeitor: TLeitor;

    procedure SetXML(const Value: String);
    function GetIDEvento: string;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    property IDEvento: string read GetIDEvento;
    property XML: String read FXML write SetXML;
    property Leitor: TLeitor read FLeitor write FLeitor;
    property TipoEvento: TTipoEvento read FTipoEvento write FTipoEvento;
    property NomeArq: string read FNomeArq write FNomeArq;
  end;

  TLoteEventos = class(TReinfCollection)
  private
    FIdeEmpregador: TIdeContri;
    FIdeTransmissor: TIdeTransmissor;
    FGerador: TGerador;
    FXML: string;

    function GetItem(Index: integer): TItemLoteEventos;
    procedure SetItem(Index: integer; const Value: TItemLoteEventos);
    procedure CarregarXmlEventos;
    function Validar: Boolean;
  public
    constructor Create(AACBrReinf: TComponent); override;
    destructor Destroy; override;

    function Add: TItemLoteEventos; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TItemLoteEventos;

    function LoadFromFile(const CaminhoArquivo: String): Boolean;
    function LoadFromStream(AStream: TStringStream): Boolean;
    function LoadFromString(const AXMLString: String): Boolean;
    procedure GerarXML;

    property Items[Index: Integer] : TItemLoteEventos read GetItem write SetItem;
    property IdeEmpregador : TIdeContri read FIdeEmpregador write FIdeEmpregador;
    property IdeTransmissor : TIdeTransmissor read FIdeTransmissor write FIdeTransmissor;
    property XML : String read FXML write FXML;
  end;

implementation

uses
  ACBrReinf, DateUtils;

{ TLoteEventos }

function TLoteEventos.Add: TItemLoteEventos;
begin
  Result := Self.New;
end;

constructor TLoteEventos.Create(AACBrReinf: TComponent);
begin
  inherited Create(AACBrReinf);

  FIdeEmpregador  := TIdeContri.Create;
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

procedure TLoteEventos.CarregarXmlEventos;
var
  i: Integer;
  FEventos: TEventos;
begin
  //Limpando
  Clear;

  FEventos := TACBrReinf(FACBrReinf).Eventos;

  {R1000}
  for i := 0 to FEventos.ReinfEventos.R1000.Count - 1 do
    LoadFromString(FEventos.ReinfEventos.R1000[i].evtInfoContri.XML);

  {R1050}
  for i := 0 to FEventos.ReinfEventos.R1050.Count - 1 do
    LoadFromString(FEventos.ReinfEventos.R1050[i].evtTabLig.XML);

  {R1070}
  for i := 0 to FEventos.ReinfEventos.R1070.Count - 1 do
    LoadFromString(FEventos.ReinfEventos.R1070[i].evtTabProcesso.XML);

  {R2010}
  for i := 0 to FEventos.ReinfEventos.R2010.Count - 1 do
    LoadFromString(FEventos.ReinfEventos.R2010[i].evtServTom.XML);

  {R2020}
  for i := 0 to FEventos.ReinfEventos.R2020.Count - 1 do
    LoadFromString(FEventos.ReinfEventos.R2020[i].evtServPrest.XML);

  {R2030}
  for i := 0 to FEventos.ReinfEventos.R2030.Count - 1 do
    LoadFromString(FEventos.ReinfEventos.R2030[i].evtAssocDespRec.XML);

  {R2040}
  for i := 0 to FEventos.ReinfEventos.R2040.Count - 1 do
    LoadFromString(FEventos.ReinfEventos.R2040[i].evtAssocDespRep.XML);

  {R2050}
  for i := 0 to FEventos.ReinfEventos.R2050.Count - 1 do
    LoadFromString(FEventos.ReinfEventos.R2050[i].evtComProd.XML);

  {R2055}
  for i := 0 to FEventos.ReinfEventos.R2055.Count - 1 do
    LoadFromString(FEventos.ReinfEventos.R2055[i].evtAqProd.XML);

  {R2060}
  for i := 0 to FEventos.ReinfEventos.R2060.Count - 1 do
    LoadFromString(FEventos.ReinfEventos.R2060[i].evtCPRB.XML);

  {R2070}
  for i := 0 to FEventos.ReinfEventos.R2070.Count - 1 do
    LoadFromString(FEventos.ReinfEventos.R2070[i].evtPgtosDivs.XML);

  {R2098}
  for i := 0 to FEventos.ReinfEventos.R2098.Count - 1 do
    LoadFromString(FEventos.ReinfEventos.R2098[i].evtReabreEvPer.XML);

  {R2099}
  for i := 0 to FEventos.ReinfEventos.R2099.Count - 1 do
    LoadFromString(FEventos.ReinfEventos.R2099[i].evtFechaEvPer.XML);

  {R3010}
  for i := 0 to FEventos.ReinfEventos.R3010.Count - 1 do
    LoadFromString(FEventos.ReinfEventos.R3010[i].evtEspDesportivo.XML);

  {R4010}
  for i := 0 to FEventos.ReinfEventos.R4010.Count - 1 do
    LoadFromString(FEventos.ReinfEventos.R4010[i].evtRetPF.XML);

  {R4020}
  for i := 0 to FEventos.ReinfEventos.R4020.Count - 1 do
    LoadFromString(FEventos.ReinfEventos.R4020[i].evtRetPJ.XML);

  {R4040}
  for i := 0 to FEventos.ReinfEventos.R4040.Count - 1 do
    LoadFromString(FEventos.ReinfEventos.R4040[i].evtBenefNId.XML);

  {R4080}
  for i := 0 to FEventos.ReinfEventos.R4080.Count - 1 do
    LoadFromString(FEventos.ReinfEventos.R4080[i].evtRetRec.XML);

  {R4099}
  for i := 0 to FEventos.ReinfEventos.R4099.Count - 1 do
    LoadFromString(FEventos.ReinfEventos.R4099[i].evtFech.XML);

  {R9000}
  for i := 0 to FEventos.ReinfEventos.R9000.Count - 1 do
    LoadFromString(FEventos.ReinfEventos.R9000[i].evtExclusao.XML);
end;

procedure TLoteEventos.GerarXML;
var
  i: Integer;
  EventosXml: AnsiString;
  tpInsc, nrInsc: string;
begin
  CarregarXmlEventos;

  EventosXml := EmptyStr;

  if TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF < v2_01_01 then
  begin
    FXML :=
    '<Reinf xmlns="http://www.reinf.esocial.gov.br/schemas/envioLoteEventos/v'+
         VersaoReinfToStr(TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF) + '">'+
      '<loteEventos>';

     for i := 0 to Self.Count - 1 do
       EventosXml := EventosXml +
                     '<evento id="' + Self.Items[i].IDEvento + '"> ' +
                       RemoverDeclaracaoXML(Self.Items[i].XML) +
                     '</evento>';

    FXML := FXML + EventosXml +
              '</loteEventos>' +
            '</Reinf>';
  end
  else
  begin
    nrInsc := TACBrReinf(FACBrReinf).Configuracoes.Geral.IdContribuinte;

    if Length(nrInsc) = 14 then
      tpInsc := '1'
    else
      tpInsc := '2';

    FXML :=
      '<Reinf xmlns="http://www.reinf.esocial.gov.br/schemas/envioLoteEventosAssincrono/v1_00_00">' +
        '<envioLoteEventos>' +
          '<ideContribuinte>' +
            '<tpInsc>' + tpInsc + '</tpInsc>' +
            '<nrInsc>' + Copy(nrInsc, 1, 8) + '</nrInsc>' +
          '</ideContribuinte>' +
          '<eventos>';

    for i := 0 to Self.Count - 1 do
      EventosXml := EventosXml +
                    '<evento Id="' + Self.Items[i].IDEvento +'"> ' +
                      RemoverDeclaracaoXML(Self.Items[i].XML) +
                    '</evento>';

    FXML := FXML + EventosXml +
                   '</eventos>' +
                 '</envioLoteEventos>'+
               '</Reinf>';
  end;

  FXML := AnsiToUtf8(FXML);
//  Validar;
end;

function TLoteEventos.GetItem(Index: integer): TItemLoteEventos;
begin
  Result := TItemLoteEventos(inherited Items[Index]);
end;

function TLoteEventos.LoadFromFile(const CaminhoArquivo: String): Boolean;
var
  ArquivoXML: TStringList;
  sXMLTemp: String;
  XMLOriginal: String;
  i: integer;
begin
  Result := True;

  ArquivoXML := TStringList.Create;
  try
    ArquivoXML.LoadFromFile(CaminhoArquivo);
    XMLOriginal := ArquivoXML.Text;
  finally
    ArquivoXML.Free;
  end;

  // Converte de UTF8 para a String nativa da IDE //
  sXMLTemp := DecodeToString(XMLOriginal, True);
  LoadFromString(sXMLTemp);

  for i := 0 to Self.Count - 1 do
    Self.Items[i].NomeArq := CaminhoArquivo;
end;

function TLoteEventos.LoadFromStream(AStream: TStringStream): Boolean;
var
  XMLOriginal: String;
begin
  AStream.Position := 0;
  XMLOriginal := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(XMLOriginal));
end;

function TLoteEventos.LoadFromString(const AXMLString: String): Boolean;
var
  AXML, AXMLStr: String;
  P: integer;

  function PosReinf: integer;
  begin
    Result := pos('</Reinf>', AXMLStr);
  end;

begin
  AXMLStr := AXMLString;
  P := PosReinf;

  while P > 0 do
  begin
    AXML := copy(AXMLStr, 1, P + 7);
    AXMLStr := Trim(copy(AXMLStr, P + 8, length(AXMLStr)));

    Self.New.FXML := AXML;

    P := PosReinf;
  end;

  Result := Self.Count > 0;
end;

function TLoteEventos.New: TItemLoteEventos;
begin
  Result := TItemLoteEventos.Create(FACBrReinf);
  Self.Add(Result);
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
  with TACBrReinf(FACBrReinf) do
  begin
    EhValido := SSL.Validar(FXML, Configuracoes.Arquivos.PathSchemas +
                'EnvioLoteEventos' + PrefixVersao +
          VersaoReinfToStr(TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF) +
          '.xsd', Erro);

    if not EhValido and Configuracoes.Geral.ExibirErroSchema then
      raise EACBrReinfException.CreateDef(ACBrStr('Houve erro na validação do Lote: ') + Erro);
  end;
  
  result := EhValido;
end;

{ TItemLoteEventos }

constructor TItemLoteEventos.Create(AOwner: TComponent);
begin
  inherited Create;

  FACBrReinf := AOwner;
  FLeitor := TLeitor.Create;
  FXML := '';
end;

destructor TItemLoteEventos.Destroy;
begin
  FLeitor.Free;

  inherited;
end;

function TItemLoteEventos.GetIDEvento: string;
var
  Ini: Integer;
begin
  Result := EmptyStr;
  Ini := pos('id=', XML);
  if ini > 0 then
    Result := 'ID' + OnlyNumber(Copy(XML, Ini + 4, 38));
end;

procedure TItemLoteEventos.SetXML(const Value: String);
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
