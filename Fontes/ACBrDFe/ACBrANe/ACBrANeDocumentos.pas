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

unit ACBrANeDocumentos;

interface

uses
  Classes, SysUtils,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$Else}
   Contnrs,
  {$IfEnd}
  ACBrBase, ACBrDFe, ACBrANeConfiguracoes, ACBrANe.Classes, ACBrANe.Conversao;

type

  { TDocumento }

  TDocumento = class(TCollectionItem)
  private
    FANe: TANe;
    FACBrANe: TACBrDFe;

    FAlertas: string;
    FNomeArq: string;
    FConfirmada: Boolean;
    FXml: string;

    function CalcularNomeArquivo: string;
    function CalcularPathArquivo: string;
    procedure SetXmlANe(const Value: string);
    function GetXmlANe: string;
  public
    constructor Create(AOwner: TACBrDFe);
    destructor Destroy; override;

    function LerXML(const AXML: string): Boolean;

    function GerarXML: string;
    function GravarXML(const NomeArquivo: string = '';
      const PathArquivo: string = ''): Boolean;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(const sPara, sAssunto: string; sMensagem: TStrings = nil;
      EnviaPDF: Boolean = True; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil; ManterPDFSalvo: Boolean = True);

    property NomeArq: string    read FNomeArq    write FNomeArq;

    function CalcularNomeArquivoCompleto(NomeArquivo: string = '';
      PathArquivo: string = ''): string;

    property ANe: TANe read FANe;

    property Xml: string read FXml write FXml;

    property Confirmada: Boolean read FConfirmada write FConfirmada;
    property Alertas: string     read FAlertas;

  end;

  { TDocumentos }

  TDocumentos = class(TACBrObjectList)
  private
    FTransacao: Boolean;
    FNumeroLote: string;
    FACBrANe: TACBrDFe;
    FConfiguracoes: TConfiguracoesANe;
    FXMLLoteOriginal: string;
    FXMLLoteAssinado: string;
    FAlertas: string;

    function GetItem(Index: integer): TDocumento;
    procedure SetItem(Index: integer; const Value: TDocumento);

  public
    constructor Create(AOwner: TACBrDFe);

    procedure GerarANe;

    function New: TDocumento; reintroduce;
    function Add(ANota: TDocumento): Integer; reintroduce;
    Procedure Insert(Index: Integer; ANota: TDocumento); reintroduce;

    property Items[Index: integer]: TDocumento read GetItem write SetItem; default;

    function GetNamePath: string;

    // Incluido o Parametro AGerarANe que determina se após carregar os dados da ANe
    // para o componente, será gerado ou não novamente o XML da ANe.
    function LoadFromFile(const CaminhoArquivo: string; AGerarANe: Boolean = True): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarANe: Boolean = True): Boolean;
    function LoadFromString(const AXMLString: string; AGerarANe: Boolean = True): Boolean;

    function GravarXML(const PathNomeArquivo: string = ''): Boolean;

    property XMLLoteOriginal: string read FXMLLoteOriginal write FXMLLoteOriginal;
    property XMLLoteAssinado: string read FXMLLoteAssinado write FXMLLoteAssinado;
    property NumeroLote: string      read FNumeroLote      write FNumeroLote;
    property Transacao: Boolean      read FTransacao       write FTransacao;
    property Alertas: string         read FAlertas;

    property ACBrANe: TACBrDFe read FACBrANe;
  end;

implementation

uses
  synautil, IniFiles, StrUtilsEx,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.FilesIO, ACBrUtil.XMLHTML,
  ACBrDFeUtil,
  ACBrANe, ACBrANeInterface;

{ TDocumento }

constructor TDocumento.Create(AOwner: TACBrDFe);
begin
  if not (AOwner is TACBrANe) then
    raise EACBrANeException.Create('AOwner deve ser do tipo TACBrANe');

  FACBrANe := TACBrANe(AOwner);
  FANe := TANe.Create;
end;

destructor TDocumento.Destroy;
begin
  FANe.Free;

  inherited Destroy;
end;

function TDocumento.LerXML(const AXML: string): Boolean;
var
  FProvider: IACBrANeProvider;
  XmlTratado: string;
begin
  FProvider := TACBrANe(FACBrANe).Provider;

  if not Assigned(FProvider) then
    raise EACBrANeException.Create(ERR_SEM_Seguradora);

  Result := FProvider.LerXML(AXML, FANe, XmlTratado);

  FXml := XmlTratado
end;

procedure TDocumento.SetXmlANe(const Value: string);
begin
  LerXML(Value);
  FXml := Value;
end;

function TDocumento.GravarXML(const NomeArquivo: string;
  const PathArquivo: string): Boolean;
var
  ConteudoEhXml: Boolean;
begin
  if EstaVazio(FXml) then
    GerarXML;

  {
    Tem provedor que é gerando um JSON em vez de XML e o método Gravar acaba
    incluindo na primeira linha do arquivo o encoding do XML.
    Para contornar isso a variável ConteudoEhXml recebe o valor false quando é
    um JSON e o método Gravar não inclui o encoding.
  }
  ConteudoEhXml := StringIsXML(FXml);
 {
  if aTipo = txmlANe then
  begin
    if EstaVazio(NomeArquivo) then
      FNomeArq := TACBrANe(FACBrANe).GetNumID(ANe) + '-ANe.xml'
    else
    begin
      FNomeArq := NomeArquivo;

      if ExtractFileExt(FNomeArq) = '' then
        FNomeArq := FNomeArq + '.xml';
    end;

    Result := TACBrANe(FACBrANe).Gravar(FNomeArq, FXmlANe, PathArquivo, ConteudoEhXml);
  end
  else
  begin
    FNomeArqRps := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);
    Result := TACBrANe(FACBrANe).Gravar(FNomeArqRps, FXmlRps, '', ConteudoEhXml);
  end;
  }
end;

function TDocumento.GravarStream(AStream: TStream): Boolean;
begin
  if EstaVazio(FXml) then
    GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FXml));
  Result := True;
end;

procedure TDocumento.EnviarEmail(const sPara, sAssunto: string; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings; sReplyTo: TStrings;
  ManterPDFSalvo: Boolean);
var
  NomeArqTemp: string;
  AnexosEmail:TStrings;
  StreamANe: TMemoryStream;
begin
  if not Assigned(TACBrANe(FACBrANe).MAIL) then
    raise EACBrANeException.Create('Componente ACBrMail não associado');

  AnexosEmail := TStringList.Create;
  StreamANe  := TMemoryStream.Create;
  try
    AnexosEmail.Clear;

    if Assigned(Anexos) then
      AnexosEmail.Assign(Anexos);

    with TACBrANe(FACBrANe) do
    begin
      GravarStream(StreamANe);

//      EnviarEmail( sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamANe,
//                   NumID[FANe] +'-ANe.xml', sReplyTo);
    end;
  finally
    if not ManterPDFSalvo then
      DeleteFile(NomeArqTemp);

    AnexosEmail.Free;
    StreamANe.Free;
  end;
end;

function TDocumento.GerarXML: string;
var
  FProvider: IACBrANeProvider;
begin
  FProvider := TACBrANe(FACBrANe).Provider;

  if not Assigned(FProvider) then
    raise EACBrANeException.Create(ERR_SEM_Seguradora);

  FProvider.GerarXml(ANe, FXml, FAlertas);
  Result := FXml;
end;

function TDocumento.GetXmlANe: string;
begin
  if XmlEhUTF8(FXml) then
    Result := FXml
  else
    Result := '<?xml version="1.0" encoding="UTF-8"?>' + FXml;
end;

function TDocumento.CalcularNomeArquivo: string;
var
  xID: string;
begin
//  xID := TACBrANe(FACBrANe).NumID[ANe];

  if EstaVazio(xID) then
    raise EACBrANeException.Create('ID Inválido. Impossível Salvar XML');

  Result := xID + '-rps.xml';
end;

function TDocumento.CalcularPathArquivo: string;
var
  Data: TDateTime;
begin
  with TACBrANe(FACBrANe) do
  begin
    Data := Now;

//    Result := PathWithDelim(Configuracoes.Arquivos.GetPathRPS(Data,
//      FANe.Prestador.IdentificacaoPrestador.CpfCnpj,
//      FANe.Prestador.IdentificacaoPrestador.InscricaoEstadual));
  end;
end;

function TDocumento.CalcularNomeArquivoCompleto(NomeArquivo: string;
  PathArquivo: string): string;
begin
  if EstaVazio(NomeArquivo) then
    NomeArquivo := CalcularNomeArquivo;

  if EstaVazio(PathArquivo) then
    PathArquivo := CalcularPathArquivo
  else
    PathArquivo := PathWithDelim(PathArquivo);

  Result := PathArquivo + NomeArquivo;
end;

{ TDocumentos }

constructor TDocumentos.Create(AOwner: TACBrDFe);
begin
  if not (AOwner is TACBrANe) then
    raise EACBrANeException.Create('AOwner deve ser do tipo TACBrANe');

  inherited Create();

  FACBrANe := TACBrANe(AOwner);
  FConfiguracoes := TACBrANe(FACBrANe).Configuracoes;
end;

function TDocumentos.New: TDocumento;
begin
  Result := TDocumento.Create(FACBrANe);
  Add(Result);
end;

function TDocumentos.Add(ANota: TDocumento): Integer;
begin
  Result := inherited Add(ANota);
end;

procedure TDocumentos.GerarANe;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].GerarXML;
end;

function TDocumentos.GetItem(Index: integer): TDocumento;
begin
  Result := TDocumento(inherited Items[Index]);
end;

function TDocumentos.GetNamePath: string;
begin
  Result := 'Documento';
end;

procedure TDocumentos.Insert(Index: Integer; ANota: TDocumento);
begin
  inherited Insert(Index, ANota);
end;

procedure TDocumentos.SetItem(Index: integer; const Value: TDocumento);
begin
  inherited Items[Index] := Value;
end;

function TDocumentos.LoadFromFile(const CaminhoArquivo: string;
  AGerarANe: Boolean = True): Boolean;
var
  XmlUTF8: AnsiString;
  i, l: integer;
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile(CaminhoArquivo);

    XmlUTF8 := ReadStrFromStream(MS, MS.Size);
  finally
    MS.Free;
  end;

  l := Self.Count; // Indice da última nota já existente

  Result := LoadFromString(XmlUTF8, AGerarANe);

  if Result then
  begin
    // Atribui Nome do arquivo a novas notas inseridas //
    for i := l to Self.Count - 1 do
    begin
      Self.Items[i].NomeArq := CaminhoArquivo;
    end;
  end;
end;

function TDocumentos.LoadFromStream(AStream: TStringStream;
  AGerarANe: Boolean = True): Boolean;
var
  AXML: AnsiString;
begin
  AStream.Position := 0;
  AXML := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(AXML), AGerarANe);
end;

function TDocumentos.LoadFromString(const AXMLString: string;
  AGerarANe: Boolean = True): Boolean;
begin
  with Self.New do
  begin
    LerXML(AXMLString);

    if AGerarANe then
      GerarXML;
  end;

  Result := Self.Count > 0;
end;

function TDocumentos.GravarXML(const PathNomeArquivo: string): Boolean;
var
  i: integer;
  NomeArq, PathArq : string;
begin
  Result := True;
  i := 0;

  while Result and (i < Self.Count) do
  begin
    PathArq := ExtractFilePath(PathNomeArquivo);
    NomeArq := ExtractFileName(PathNomeArquivo);
    Result := Self.Items[i].GravarXML(NomeArq, PathArq);
    Inc(i);
  end;
end;

end.
