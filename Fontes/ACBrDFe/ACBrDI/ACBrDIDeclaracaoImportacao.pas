{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Giovane Preis                          }
{                                       Daniel Simoes de Almeida               }
{                                                                              }
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
{******************************************************************************}

{$I ACBr.inc}

unit ACBrDIDeclaracaoImportacao;

interface

uses
  Classes, SysUtils, Contnrs,
  pcnDI, pcnDIR, pcnLeitor;

type

  { TDeclaracaoImportacao }

  TDeclaracaoImportacao = class(TObject)
  private
    FDI: TDI;
    FDIR: TDIR;

    FXMLOriginal: String;

    FNomeArq: String;

    procedure SetXMLOriginal(const AValue: String);
  public
    constructor Create;
    destructor Destroy; override;

    function LerXML(const AXML: String): Boolean;

    property NomeArq: String read FNomeArq write FNomeArq;

    property DI: TDI read FDI;

    property XMLOriginal: String read FXMLOriginal   write SetXMLOriginal;
  end;

  { TConhecimentos }

  TDeclaracoesImportacao = class(TObjectList)
  private
    function GetItem(Index: Integer): TDeclaracaoImportacao;
    procedure SetItem(Index: Integer; const Value: TDeclaracaoImportacao);
  public
    function New: TDeclaracaoImportacao;

    property Items[Index: Integer]: TDeclaracaoImportacao read GetItem write SetItem; default;

    function LoadFromFile(const CaminhoArquivo: String): Boolean;
    function LoadFromStream(AStream: TStringStream): Boolean;
    function LoadFromString(const AXMLString: String): Boolean;
  end;

implementation

uses
  synautil, ACBrUtil.XMLHTML, ACBrUtil.Strings;

{ TDeclaracaoImportacao }

constructor TDeclaracaoImportacao.Create;
begin
  inherited Create;

  FDI  := TDI.Create;
  FDIR := TDIR.Create(FDI);
end;

destructor TDeclaracaoImportacao.Destroy;
begin
  FDI.Free;
  FDIR.Free;

  inherited Destroy;
end;

function TDeclaracaoImportacao.LerXML(const AXML: String): Boolean;
var
  XMLStr: String;
begin
  XMLOriginal := AXML;  // SetXMLOriginal() irá verificar se AXML está em UTF8

  { Verifica se precisa converter "AXML" de UTF8 para a String nativa da IDE.
    Isso é necessário, para que as propriedades fiquem com a acentuação correta }
  XMLStr := UTF8ToNativeString(ParseText(AXML));

  FDIR.Leitor.Arquivo := XMLStr;
  FDIR.LerXml;

  Result := True;
end;

procedure TDeclaracaoImportacao.SetXMLOriginal(const AValue: String);
var
  XMLUTF8: String;
begin
  { Garante que o XML informado está em UTF8, se ele realmente estiver, nada
    será modificado por "ConverteXMLtoUTF8"  (mantendo-o "original") }
  XMLUTF8 := ConverteXMLtoUTF8(AValue);

  FXMLOriginal := XMLUTF8;
end;

{ TDeclaracoesImportacao }

function TDeclaracoesImportacao.New: TDeclaracaoImportacao;
begin
  Result := TDeclaracaoImportacao.Create;
  Self.Add(Result);
end;

function TDeclaracoesImportacao.GetItem(Index: Integer): TDeclaracaoImportacao;
begin
  Result := TDeclaracaoImportacao(inherited GetItem(Index));
end;

procedure TDeclaracoesImportacao.SetItem(Index: Integer; const Value: TDeclaracaoImportacao);
begin
  inherited SetItem(Index, Value);
end;

function TDeclaracoesImportacao.LoadFromFile(const CaminhoArquivo: String): Boolean;
var
  XMLUTF8: AnsiString;
  i, l: integer;
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile(CaminhoArquivo);
    XMLUTF8 := ReadStrFromStream(MS, MS.Size);
  finally
    MS.Free;
  end;

  l := Self.Count; // Indice do último conhecimento já existente
  Result := LoadFromString(String(XMLUTF8));

  if Result then
  begin
    // Atribui Nome do arquivo a novos Conhecimentos inseridos //
    for i := l to Pred(Self.Count) do
      Self.Items[i].NomeArq := CaminhoArquivo;
  end;
end;

function TDeclaracoesImportacao.LoadFromStream(AStream: TStringStream): Boolean;
var
  AXML: AnsiString;
begin
  AStream.Position := 0;
  AXML := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(AXML));
end;

function TDeclaracoesImportacao.LoadFromString(const AXMLString: String): Boolean;
var
  ADIXML, XMLStr: String;
  N: integer;

  function PosDI: integer;
  begin
    Result := Pos('</declaracaoImportacao>', XMLStr);
  end;

begin
  // Verifica se precisa Converter de UTF8 para a String nativa da IDE //
  XMLStr := ConverteXMLtoNativeString(AXMLString);

  N := PosDI;
  while N > 0 do
  begin
    ADIXML := Copy(XMLStr, 1, N + 23);
    XMLStr := Trim(Copy(XMLStr, N + 23, Length(XMLStr)));

    with Self.New do
      LerXML(ADIXML);

    N := PosDI;
  end;

  Result := (Self.Count > 0);
end;

end.
