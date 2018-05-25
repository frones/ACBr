{******************************************************************************}
{ Projeto: Componente ACBrReinf                                                }
{  Biblioteca multiplataforma de componentes Delphi para envio de eventos do   }
{ Reinf                                                                        }

{ Direitos Autorais Reservados (c) 2017 Leivio Ramos de Fontenele              }
{                                                                              }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Leivio Ramos de Fontenele  -  leivio@yahoo.com.br                            }
{******************************************************************************}
{******************************************************************************
|* Historico
|*
|* 24/10/2017: Renato Rubinho
|*  - Compatibilizado Fonte com Delphi 7
*******************************************************************************}

{$I ACBr.inc}

unit ACBrReinfLoteEventos;

interface

uses
  Classes, SysUtils, Dialogs, StrUtils, synautil,
  ACBrUtil,
  pcnConversao, pcnAuxiliar, pcnLeitor, pcnGerador,
  ACBrReinfConfiguracoes, ACBrReinfEventos,
  pcnCommonReinf, pcnConversaoReinf;

type
  TItemLoteEventosClass = class of TItemLoteEventos;

  TItemLoteEventos = class(TCollectionItem)
  private
    FACBrReinf : TComponent;
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

  TLoteEventos = class(TOwnedCollection)
  private
    FACBrReinf: TComponent;
    FIdeEmpregador: TIdeContri;
    FIdeTransmissor: TIdeTransmissor;
    FGerador: TGerador;
    FXML: string;

    function GetItem(Index: integer): TItemLoteEventos;
    procedure SetItem(Index: integer; const Value: TItemLoteEventos);
    procedure CarregarXmlEventos;
  protected
    procedure GerarCabecalho(Namespace: string);
    procedure GerarRodape;
    function Validar: Boolean;
  public
    constructor Create(AOwner: TComponent); reintroduce;

    function Add : TItemLoteEventos;
    function LoadFromFile(CaminhoArquivo: String): Boolean;
    function LoadFromStream(AStream: TStringStream): Boolean;
    function LoadFromString(AXMLString: String): Boolean;
    procedure GerarXML;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

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
  Result := TItemLoteEventos(inherited Add);
end;

procedure TLoteEventos.AfterConstruction;
begin
  inherited;

  FIdeEmpregador  := TIdeContri.Create;
  FIdeTransmissor := TIdeTransmissor.Create;
  FGerador        := TGerador.Create;
end;

procedure TLoteEventos.BeforeDestruction;
begin
  inherited;

  FIdeEmpregador.Free;
  FIdeTransmissor.Free;
  FGerador.Free;
end;

constructor TLoteEventos.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner, TItemLoteEventos);

  FACBrReinf := AOwner;
end;

procedure TLoteEventos.GerarCabecalho(Namespace: String);
begin

end;

procedure TLoteEventos.GerarRodape;
begin

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

  {R9000}
  for i := 0 to FEventos.ReinfEventos.R9000.Count - 1 do
    LoadFromString(FEventos.ReinfEventos.R9000[i].evtExclusao.XML);
end;

procedure TLoteEventos.GerarXML; //(const AGrupo: TReinfGrupo);
var
  i: Integer;
  Eventosxml: AnsiString;
begin
  CarregarXmlEventos;

  Eventosxml := EmptyStr;
  FXML := EmptyStr;

  FXML :=
  '<Reinf xmlns="http://www.reinf.esocial.gov.br/schemas/envioLoteEventos/v'+
       VersaoReinfToStr(TACBrReinf(FACBrReinf).Configuracoes.Geral.VersaoDF) + '">'+
    '<loteEventos>';

   for i := 0 to Self.Count - 1 do
     Eventosxml := Eventosxml +
                   '<evento id="' + Self.Items[i].IDEvento +'"> ' +
                    StringReplace(Self.Items[i].XML, '<' + ENCODING_UTF8 + '>', '', []) +
                   '</evento>';

  FXML := FXML + Eventosxml;
  FXML := FXML +
            '</loteEventos>'+
          '</Reinf>';

  FXML := AnsiToUtf8(FXML);
  Validar;
end;

function TLoteEventos.GetItem(Index: integer): TItemLoteEventos;
begin
  Result := TItemLoteEventos(inherited GetItem(Index));
end;

function TLoteEventos.LoadFromFile(CaminhoArquivo: String): Boolean;
var
  ArquivoXML: TStringList;
  XML: String;
  XMLOriginal: AnsiString;
  i: integer;
begin
  Result := False;
  
  ArquivoXML := TStringList.Create;
  try
    ArquivoXML.LoadFromFile(CaminhoArquivo);
    XMLOriginal := ArquivoXML.Text;

    // Converte de UTF8 para a String nativa da IDE //
    XML := DecodeToString(XMLOriginal, True);
    LoadFromString(XML);

    for i := 0 to Self.Count - 1 do
      Self.Items[i].NomeArq := CaminhoArquivo;

    Result := True;
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
  AXML: AnsiString;
  P: integer;

  function PosReinf: integer;
  begin
    Result := pos('</Reinf>', AXMLString);
  end;

begin
  P := PosReinf;

  while P > 0 do
  begin
    AXML := copy(AXMLString, 1, P + 7);
    AXMLString := Trim(copy(AXMLString, P + 8, length(AXMLString)));

    Self.Add.FXML := AXML;

    P := PosReinf;
  end;

  Result := Self.Count > 0;
end;

procedure TLoteEventos.SetItem(Index: integer; const Value: TItemLoteEventos);
begin
  inherited SetItem(Index, Value);
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
  FACBrReinf := AOwner;
  FLeitor := TLeitor.Create;
  FXML := '';
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
