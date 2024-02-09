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

unit ACBrANeDocumentos;

interface

uses
  Classes, 
  SysUtils, 
  ACBrANeConfiguracoes, 
  ACBrDFeUtil,
  pcaANe, 
  pcaANeR, 
  pcaANeW, 
  pcnConversao, 
  pcnLeitor;

type

  { Documento }

  Documento = class(TCollectionItem)
  private
    FANe: TANe;
    FANeW: TANeW;
    FANeR: TANeR;

    FXMLAssinado: String;
    FXMLOriginal: String;
    FAlertas: String;
    FNomeArq: String;

    function GetConfirmado: Boolean;
    function GetProcessado: Boolean;

    function GetMsg: String;
    function GetNumID: String;
    function GetXMLAssinado: String;
    procedure SetXML(const AValue: String);
    procedure SetXMLOriginal(const AValue: String);
    function CalcularNomeArquivo: String;
    function CalcularPathArquivo: String;
    function CalcularNomeArquivoCompleto(NomeArquivo: String = '';
      PathArquivo: String = ''): String;
  public
    constructor Create(Collection2: TCollection); override;
    destructor Destroy; override;

    procedure Assinar;

    function LerXML(const AXML: AnsiString): Boolean;
//    function LerArqIni(const AIniString: String): Boolean;

    function GerarXML: String;
    function GravarXML(const NomeArquivo: String = ''; const PathArquivo: String = ''): Boolean;
    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings = nil;
      EnviaPDF: Boolean = True; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil);

    property NomeArq: String read FNomeArq write FNomeArq;
    property ANe: TANe read FANe;

    // Atribuir a "XML", faz o componente transferir os dados lido para as propriedades internas e "XMLAssinado"
    property XML: String         read FXMLOriginal   write SetXML;
    // Atribuir a "XMLOriginal", reflete em XMLAssinado, se existir a tag de assinatura
    property XMLOriginal: String read FXMLOriginal   write SetXMLOriginal;
    property XMLAssinado: String read GetXMLAssinado write FXMLAssinado;
    property Confirmado: Boolean read GetConfirmado;
    property Processado: Boolean read GetProcessado;
    property Msg: String read GetMsg;
    property NumID: String read GetNumID;

    property Alertas: String read FAlertas;
  end;

  { TDocumentos }

  TDocumentos = class(TOwnedCollection)
  private
    FACBrANe: TComponent;
    FConfiguracoes: TConfiguracoesANe;

    function GetItem(Index: integer): Documento;
    procedure SetItem(Index: integer; const Value: Documento);

  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    procedure GerarANe;
    procedure Assinar;

    function Add: Documento;
    function Insert(Index: integer): Documento;

    property Items[Index: integer]: Documento read GetItem write SetItem; default;

    function GetNamePath: String; override;
    // Incluido o Parametro AGerarANe que determina se após carregar os dados do ANe
    // para o componente, será gerado ou não novamente o XML do ANe.
    function LoadFromFile(const CaminhoArquivo: String; AGerarANe: Boolean = True): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarANe: Boolean = True): Boolean;
    function LoadFromString(AXMLString: String; AGerarANe: Boolean = True): Boolean;
//    function LoadFromIni(const AIniString: String): Boolean;

    function GravarXML(const PathNomeArquivo: String = ''): Boolean;

    property ACBrANe: TComponent read FACBrANe;
  end;

implementation

uses
  ACBrANe,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  pcaConversao, synautil;

{ Documento }

constructor Documento.Create(Collection2: TCollection);
begin
  inherited Create(Collection2);
  FANe := TANe.Create;
  FANeW := TANeW.Create(FANe);
  FANeR := TANeR.Create(FANe);

  with TACBrANe(TDocumentos(Collection).ACBrANe) do
  begin
    FANe.usuario := Configuracoes.Geral.Usuario;
    FANe.senha   := Configuracoes.Geral.Senha;
    FANe.codatm  := Configuracoes.Geral.CodATM;
    FANe.CNPJ    := Configuracoes.Geral.CNPJEmitente;
  end;
end;

destructor Documento.Destroy;
begin
  FANeW.Free;
  FANeR.Free;
  FANe.Free;

  inherited Destroy;
end;

procedure Documento.Assinar;
var
  XMLStr: String;
  XMLUTF8: AnsiString;

function RemoverGrupoInfSuplementares(const XML, Grupo: string): string;
var
  IniGrupo, FimGrupo: Integer;
begin
  IniGrupo := Pos('<' + Grupo + '>', XML);
  if IniGrupo > 0 then
  begin
    FimGrupo := Pos('</' + Grupo + '>', XML) + Length(Grupo) + 3;

    Result := Copy(XML, 1, IniGrupo -1) + Copy(XML, FimGrupo, Length(XML));
  end
  else
    Result := XML;
end;

begin
  // Gera novamente, para processar propriedades que podem ter sido modificadas
  XMLStr := GerarXML;

  // XML já deve estar em UTF8, para poder ser assinado //
  XMLUTF8 := ConverteXMLtoUTF8(XMLStr);

  with TACBrANe(TDocumentos(Collection).ACBrANe) do
  begin
    FXMLAssinado := String(XMLUTF8); // SSL.Assinar(String(XMLUTF8), 'ANe', 'infANe');
    // SSL.Assinar() sempre responde em UTF8...
    FXMLOriginal := RemoverDeclaracaoXML(FXMLAssinado);

    case Configuracoes.Geral.Seguradora of
      tsELT: FXMLOriginal := '<ANe xmlns:tem="http://tempuri.org/">' +
                               FXMLOriginal + '</ANe>';
    else
      begin
        case Configuracoes.Geral.TipoDoc of
          tdCTe: FXMLOriginal := '<ANe>' +
                   RemoverGrupoInfSuplementares(FXMLOriginal, 'infCTeSupl') + '</ANe>';

          tdNFe: FXMLOriginal := '<ANe>' + FXMLOriginal + '</ANe>';

          tdMDFe: FXMLOriginal := '<ANe>' +
                    RemoverGrupoInfSuplementares(FXMLOriginal, 'infMDFeSupl') + '</ANe>';

          tdAddBackMail: FXMLOriginal := '<AddBackMail>' + FXMLOriginal + '</AddBackMail>';
        end;
      end;
    end;

    if Configuracoes.Arquivos.Salvar then
    begin
      if NaoEstaVazio(NomeArq) then
        Gravar(NomeArq, FXMLOriginal)
      else
        Gravar(CalcularNomeArquivoCompleto(), FXMLOriginal);
    end;
  end;
end;
{
function Documento.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  sSecao: String;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with FANe do
    begin
      sSecao := 'Ide';
      Usuario := INIRec.ReadString(sSecao, 'Usuario', '');
      Senha   := INIRec.ReadString(sSecao, 'Senha', '');
      codatm  := INIRec.ReadString(sSecao, 'codatm', '');

      sSecao := 'xmlDFe';
      xmlDFe := INIRec.ReadString(sSecao, 'Xml', '');
    end;

    GerarXML;
  finally
    INIRec.Free;
  end;
end;
}
function Documento.LerXML(const AXML: AnsiString): Boolean;
var
  XMLStr: String;
begin
  XMLOriginal := AXML;  // SetXMLOriginal() irá verificar se AXML está em UTF8

  { Verifica se precisa converter "AXML" de UTF8 para a String nativa da IDE.
    Isso é necessário, para que as propriedades fiquem com a acentuação correta }
  XMLStr := ParseText(AXML, True, XmlEhUTF8(AXML));

  FANeR.Leitor.Arquivo := XMLStr;
  FANeR.LerXml;

  Result := True;
end;

function Documento.GravarXML(const NomeArquivo: String; const PathArquivo: String): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  FNomeArq := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);

  Result := TACBrANe(TDocumentos(Collection).ACBrANe).Gravar(FNomeArq, FXMLOriginal);
end;

function Documento.GravarStream(AStream: TStream): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FXMLOriginal));
  Result := True;
end;

procedure Documento.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings; sReplyTo: TStrings);
var
//  NomeArq : String;
  AnexosEmail:TStrings;
  StreamANe : TMemoryStream;
begin
  if not Assigned(TACBrANe(TDocumentos(Collection).ACBrANe).MAIL) then
    raise EACBrANeException.Create('Componente ACBrMail não associado');

  AnexosEmail := TStringList.Create;
  StreamANe := TMemoryStream.Create;
  try
    AnexosEmail.Clear;
    if Assigned(Anexos) then
      AnexosEmail.Assign(Anexos);

    with TACBrANe(TDocumentos(Collection).ACBrANe) do
    begin
      GravarStream(StreamANe);
      (*
      if (EnviaPDF) then
      begin
        if Assigned(DAANe) then
        begin
          DAANe.ImprimirDAANePDF(FANe);
          NomeArq := PathWithDelim(DAANe.PathPDF) + NumID + '-ANe.pdf';
          AnexosEmail.Add(NomeArq);
        end;
      end;
      *)
      EnviarEmail( sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamANe,
                   NumID + '-ANe.xml', sReplyTo);
    end;
  finally
    AnexosEmail.Free;
    StreamANe.Free;
  end;
end;

function Documento.GerarXML: String;
begin
  with TACBrANe(TDocumentos(Collection).ACBrANe) do
  begin
    FANeW.Gerador.Opcoes.FormatoAlerta  := Configuracoes.Geral.FormatoAlerta;
    FANeW.Gerador.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
    FANeW.Gerador.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
    FANeW.Gerador.Opcoes.IdentarXML     := Configuracoes.Geral.IdentarXML;

    FANeW.TipoDoc    := Configuracoes.Geral.TipoDoc;
    FANeW.Seguradora := Configuracoes.Geral.Seguradora;

    TimeZoneConf.Assign( Configuracoes.WebServices.TimeZoneConf );
  end;

  FANeW.GerarXml;
  XMLOriginal := FANeW.Gerador.ArquivoFormatoXML;

  if NaoEstaVazio(FNomeArq) then
    FNomeArq := CalcularNomeArquivoCompleto('', ExtractFilePath(FNomeArq));

  FAlertas := ACBrStr( FANeW.Gerador.ListaDeAlertas.Text );
  Result := FXMLOriginal;
end;

function Documento.CalcularNomeArquivo: String;
var
  xID: String;
begin
  xID := Self.NumID;

  if EstaVazio(xID) then
    raise EACBrANeException.Create('ID Inválido. Impossível Salvar XML');

  Result := xID + '-ANe.xml';
end;

function Documento.CalcularPathArquivo: String;
begin
  with TACBrANe(TDocumentos(Collection).ACBrANe) do
  begin
    Result := PathWithDelim(Configuracoes.Arquivos.GetPathANe(Now, Configuracoes.Geral.CNPJEmitente));
  end;
end;

function Documento.CalcularNomeArquivoCompleto(NomeArquivo: String;
  PathArquivo: String): String;
var
  PathNoArquivo: String;
begin
  if EstaVazio(NomeArquivo) then
    NomeArquivo := CalcularNomeArquivo;

  PathNoArquivo := ExtractFilePath(NomeArquivo);
  if EstaVazio(PathNoArquivo) then
  begin
    if EstaVazio(PathArquivo) then
      PathArquivo := CalcularPathArquivo
    else
      PathArquivo := PathWithDelim(PathArquivo);
  end
  else
    PathArquivo := '';

  Result := PathArquivo + NomeArquivo;
end;

function Documento.GetConfirmado: Boolean;
begin
//  Result := TACBrANe(TDocumentos(Collection).ACBrANe).cStatConfirmado(
//    FANe.procANe.cStat);
  Result := True;
end;

function Documento.GetProcessado: Boolean;
begin
//  Result := TACBrANe(TDocumentos(Collection).ACBrANe).cStatProcessado(
//    FANe.procANe.cStat);
  Result := True;
end;

function Documento.GetMsg: String;
begin
//  Result := FANe.procANe.xMotivo;
  Result := '';
end;

function Documento.GetNumID: String;
begin
  Result := FormatDateTime('yyyymmddhhnnss', Now);
end;

function Documento.GetXMLAssinado: String;
begin
  if EstaVazio(FXMLAssinado) then
    Assinar;

  Result := FXMLAssinado;
end;

procedure Documento.SetXML(const AValue: String);
begin
  LerXML(AValue);
end;

procedure Documento.SetXMLOriginal(const AValue: String);
var
  XMLUTF8: String;
begin
  { Garante que o XML informado está em UTF8, se ele realmente estiver, nada
    será modificado por "ConverteXMLtoUTF8"  (mantendo-o "original") }
  XMLUTF8 := ConverteXMLtoUTF8(AValue);

  FXMLOriginal := XMLUTF8;

  if XmlEstaAssinado(FXMLOriginal) then
    FXMLAssinado := FXMLOriginal
  else
    FXMLAssinado := '';
end;

{ TDocumentos }

constructor TDocumentos.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  if not (AOwner is TACBrANe) then
    raise EACBrANeException.Create('AOwner deve ser do tipo TACBrANe');

  inherited Create(AOwner, ItemClass);

  FACBrANe := TACBrANe(AOwner);
  FConfiguracoes := TACBrANe(FACBrANe).Configuracoes;
end;

function TDocumentos.Add: Documento;
begin
  Result := Documento(inherited Add);
end;

procedure TDocumentos.Assinar;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].Assinar;
end;

procedure TDocumentos.GerarANe;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].GerarXML;
end;

function TDocumentos.GetItem(Index: integer): Documento;
begin
  Result := Documento(inherited Items[Index]);
end;

function TDocumentos.GetNamePath: String;
begin
  Result := 'Documento';
end;

function TDocumentos.Insert(Index: integer): Documento;
begin
  Result := Documento(inherited Insert(Index));
end;

procedure TDocumentos.SetItem(Index: integer; const Value: Documento);
begin
  Items[Index].Assign(Value);
end;

function TDocumentos.LoadFromFile(const CaminhoArquivo: String;
  AGerarANe: Boolean = True): Boolean;
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

  l := Self.Count; // Indice da última nota já existente
  Result := LoadFromString(String(XMLUTF8), AGerarANe);

  if Result then
  begin
    // Atribui Nome do arquivo a novas notas inseridas //
    for i := l to Self.Count - 1 do
      Self.Items[i].NomeArq := CaminhoArquivo;
  end;
end;
{
function TDocumentos.LoadFromIni(const AIniString: String): Boolean;
begin
  with Self.Add do
    LerArqIni(AIniString);

  Result := Self.Count > 0;
end;
}
function TDocumentos.LoadFromStream(AStream: TStringStream;
  AGerarANe: Boolean = True): Boolean;
var
  AXML: AnsiString;
begin
  AStream.Position := 0;
  AXML := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(AXML), AGerarANe);
end;

function TDocumentos.LoadFromString(AXMLString: String;
  AGerarANe: Boolean = True): Boolean;
var
  AXML: AnsiString;
  N: integer;

  function PosANe: integer;
  begin
    Result := pos('</ANe>', AXMLString);
  end;

begin
  N := PosANe;
  while N > 0 do
  begin
    AXML := copy(AXMLString, 1, N + 5);
    AXMLString := Trim(copy(AXMLString, N + 6, length(AXMLString)));

    with Self.Add do
    begin
      LerXML(AXML);

      if AGerarANe then // Recalcula o XML
        GerarXML;
    end;

    N := PosANe;
  end;

  Result := Self.Count > 0;
end;

function TDocumentos.GravarXML(const PathNomeArquivo: String): Boolean;
var
  i: integer;
  NomeArq, PathArq : String;
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
