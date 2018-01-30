{******************************************************************************}
{ Projeto: Componente ACBreSocial                                              }
{  Biblioteca multiplataforma de componentes Delphi para envio dos eventos do  }
{ eSocial - http://www.esocial.gov.br/                                         }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
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
|* 27/10/2015: Jean Carlo Cantu, Tiago Ravache
|*  - Doação do componente para o Projeto ACBr
******************************************************************************}
{$I ACBr.inc}

unit ACBreSocialLoteEventos;

interface

uses
  Classes, SysUtils, Dialogs, StrUtils, synautil,
  ACBrUtil,
  pcnConversao, pcnAuxiliar, pcnLeitor, pcnGerador,
  ACBreSocialConfiguracoes,
  pcesCommon, pcesConversaoeSocial;

type

  TItemLoteEventosClass = class of TItemLoteEventos;

  TItemLoteEventos = class(TCollectionItem)
  private
    FACBreSocial : TComponent;
    FTipoEvento : TTipoEvento;
    FXML : AnsiString;
    FLeitor : TLeitor;

    procedure SetXML(const Value: AnsiString);
    function GetIDEvento: string;
  public
    constructor Create(AOwner: TComponent); reintroduce; //overload;

    property IDEvento: string read GetIDEvento;
    property XML : AnsiString read FXML write SetXML;
    property Leitor : TLeitor read FLeitor write FLeitor;
    property TipoEvento: TTipoEvento read FTipoEvento write FTipoEvento;
  end;

  TLoteEventos = class(TOwnedCollection)
  private
    FACBreSocial: TComponent;
    FIdeEmpregador: TIdeEmpregador;
    FIdeTransmissor: TIdeTransmissor;
    FGerador: TGerador;
    FXML: string;

    function GetItem(Index: integer): TItemLoteEventos;
    procedure SetItem(Index: integer; const Value: TItemLoteEventos);
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
    procedure GerarXML(const AGrupo: string);
    procedure AfterConstruction; override;

    property Items[Index: Integer] : TItemLoteEventos read GetItem write SetItem;
    property IdeEmpregador : TIdeEmpregador read FIdeEmpregador write FIdeEmpregador;
    property IdeTransmissor : TIdeTransmissor read FIdeTransmissor write FIdeTransmissor;
    property XML : String read FXML write FXML;
  end;

implementation

{ TLoteEventos }

uses
  ACBreSocial, DateUtils;

function TLoteEventos.Add: TItemLoteEventos;
begin
  Result := TItemLoteEventos(inherited Add);
end;

procedure TLoteEventos.AfterConstruction;
begin
  inherited;

  FIdeEmpregador  := TIdeEmpregador.Create;
  FIdeTransmissor := TIdeTransmissor.Create;
  FGerador        := TGerador.Create;
end;

constructor TLoteEventos.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner, TItemLoteEventos);
  FACBreSocial    := AOwner;
end;

procedure TLoteEventos.GerarCabecalho(Namespace: String);
begin

end;

procedure TLoteEventos.GerarRodape;
begin

end;

procedure TLoteEventos.GerarXML(const AGrupo: string);
var
  i: Integer;
  Eventosxml: AnsiString;
  Path: string;
begin
  Eventosxml := EmptyStr;
  FXML := EmptyStr;
  FXML :=
  '<eSocial xmlns="http://www.esocial.gov.br/schema/lote/eventos/envio/v1_1_1">'+
    '<envioLoteEventos grupo="' + AGrupo + '">'+
      '<ideEmpregador>'+
        '<tpInsc>' + Inttostr(ord(FIdeEmpregador.TpInsc) + 1) +'</tpInsc>'+
        '<nrInsc>' + Copy(FIdeEmpregador.NrInsc, 1,8) +'</nrInsc>'+
      '</ideEmpregador>'+
      '<ideTransmissor>'+
        '<tpInsc>' + Inttostr(ord(FIdeTransmissor.TpInsc) + 1) +'</tpInsc>'+
        '<nrInsc>' + FIdeTransmissor.NrInsc +'</nrInsc>'+
    '</ideTransmissor>'+
    '<eventos>';

   for i := 0 to Self.Count - 1 do
     Eventosxml := Eventosxml +
                   '<evento Id="' + Self.Items[i].IDEvento +'"> ' +
                    StringReplace(Self.Items[i].XML, '<' + ENCODING_UTF8 + '>', '', []) +
                   '</evento>';

  FXML := FXML + Eventosxml;
  FXML := FXML +
              '</eventos>'+
            '</envioLoteEventos>'+
          '</eSocial>';

  FXML := AnsiToUtf8(FXML);

  Validar;

  if TACBreSocial(Self.Owner).Configuracoes.Geral.Salvar then
  begin
    Path := TACBreSocial(Self.Owner).Configuracoes.Arquivos.PathSalvar;
    if (Path <> EmptyStr) and not DirectoryExists(Path) then
      ForceDirectories(Path);
    
    with TStringList.Create do
    try
      Text := FXml;
      SaveToFile(Path + '\' + 'EnvioLoteEventos' + '-' + IntTostr(Dayof(Now)) +
                 IntTostr(MonthOf(Now)) + IntTostr(YearOf(Now)) + '_' +
                 IntTostr(HourOf(Now)) + IntTostr(MinuteOf(Now)) +
                 IntTostr(SecondOf(Now)) + '_' + IntTostr(MilliSecondOf(Now)) +
                 '.xml');
    finally
      Free;
    end;
  end;
end;

function TLoteEventos.GetItem(Index: integer): TItemLoteEventos;
begin
  Result := TItemLoteEventos(inherited GetItem(Index));
end;

function TLoteEventos.LoadFromFile(CaminhoArquivo: String): Boolean;
//var
//  ArquivoXML: TStringList;
//  XML: String;
//  XMLOriginal: AnsiString;
//  i: integer;
begin
  // TODO: Não usar leitura de arquivo
  Result := False;
  {
  ArquivoXML := TStringList.Create;
  try
    ArquivoXML.LoadFromFile(CaminhoArquivo);
    XMLOriginal := ArquivoXML.Text;

    // Converte de UTF8 para a String nativa da IDE //
    XML := DecodeToString(XMLOriginal, True);
    LoadFromString(XML);

    for i := 0 to Self.Count - 1 do
      Self.Items[i].NomeArq := CaminhoArquivo; //NomeArq deve pertencer à classe TItemLoteEventos!!!!

    Result := True;
  finally
    ArquivoXML.Free;
  end;
  }
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
  P, N: integer;

  function PoseSocial: integer;
  begin
    Result := pos('</eSocial>', AXMLString);
  end;

begin
  N := PoseSocial;

  while N > 0 do
  begin
    P := pos('</eSocial>', AXMLString);

    if P > 0 then
    begin
      AXML := copy(AXMLString, 1, P + 10);
      AXMLString := Trim(copy(AXMLString, P + 10, length(AXMLString)));
    end
    else
    begin
      AXML := copy(AXMLString, 1, N + 6);
      AXMLString := Trim(copy(AXMLString, N + 6, length(AXMLString)));
    end;

    with Self.Add do
    begin
      FXML := AXML;
    end;

    N := PoseSocial;
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
  with TACBreSocial(FACBreSocial) do
  begin
    EhValido := SSL.Validar(FXML, Configuracoes.Arquivos.PathSchemas+'EnvioLoteEventos-v1_1_1.xsd', Erro);
    if not EhValido then
      raise EACBreSocialException.CreateDef(ACBrStr('Houve erro na validação do Lote: ') + Erro);
  end;
  
  result := EhValido;
end;

{ TItemLoteEventos }

constructor TItemLoteEventos.Create(AOwner: TComponent);
begin
  FACBreSocial := AOwner;
  FLeitor := TLeitor.Create;
  FXML := '';
end;

function TItemLoteEventos.GetIDEvento: string;
var
  Ini: Integer;
begin
  // 	<evtInfoEmpregador Id="ID1012345678900002017071908065532932">
  Result := EmptyStr;
  Ini := pos('Id=', XML);
  if ini > 0 then
  begin
    Result := Copy(XML, Ini + 4, 38);
    Result := StringReplace(Result, '"', '', []);
    Result := StringReplace(Result, '>', '', []);
    Result := StringReplace(Result, '<', '', []);
    Result := StringReplace(Result, '=', '', []);
  end;
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
