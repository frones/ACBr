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

unit ACBrCIOTContratos;

interface

uses
  Classes, 
  SysUtils,
  ACBrCIOTConfiguracoes, 
  ACBrDFeUtil,
  pcnCIOT, 
  pcnCIOTR, 
  pcnCIOTW, 
  pcnLeitor;

type

  { TContrato }

  TContrato = class(TCollectionItem)
  private
    FCIOT: TCIOT;
    FCIOTW: TCIOTW;
    FCIOTR: TCIOTR;

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
    function GerarXML: String;
    function GravarXML(const NomeArquivo: String = ''; const PathArquivo: String = ''): Boolean;
    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings = nil;
      EnviaPDF: Boolean = True; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil);

    property NomeArq: String read FNomeArq write FNomeArq;
    property CIOT: TCIOT read FCIOT;

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

  { TContratos }

  TContratos = class(TOwnedCollection)
  private
    FACBrCIOT: TComponent;
    FConfiguracoes: TConfiguracoesCIOT;

    function GetItem(Index: integer): TContrato;
    procedure SetItem(Index: integer; const Value: TContrato);

  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    procedure GerarCIOT;
    procedure Assinar;

    function Add: TContrato;
    function Insert(Index: integer): TContrato;

    property Items[Index: integer]: TContrato read GetItem write SetItem; default;

    function GetNamePath: String; override;
    // Incluido o Parametro AGerarCIOT que determina se após carregar os dados do CIOT
    // para o componente, será gerado ou não novamente o XML do CIOT.
    function LoadFromFile(const CaminhoArquivo: String; AGerarCIOT: Boolean = True): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarCIOT: Boolean = True): Boolean;
    function LoadFromString(AXMLString: String; AGerarCIOT: Boolean = True): Boolean;
    function GravarXML(const PathNomeArquivo: String = ''): Boolean;

    property ACBrCIOT: TComponent read FACBrCIOT;
  end;

implementation

uses
  ACBrCIOT,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  synautil;

{ TContrato }

constructor TContrato.Create(Collection2: TCollection);
begin
  inherited Create(Collection2);
  FCIOT := TCIOT.Create;
  FCIOTW := TCIOTW.Create(FCIOT);
  FCIOTR := TCIOTR.Create(FCIOT);

  with TACBrCIOT(TContratos(Collection).ACBrCIOT) do
  begin
    FCIOTW.Integradora := Configuracoes.Geral.Integradora;

    FCIOT.Integradora.usuario    := Configuracoes.Geral.Usuario;
    FCIOT.Integradora.senha      := Configuracoes.Geral.Senha;
    FCIOT.Integradora.Integrador := Configuracoes.Geral.HashIntegrador;
  end;
end;

destructor TContrato.Destroy;
begin
  FCIOT.Free;
  FCIOTW.Free;
  FCIOTR.Free;

  inherited Destroy;
end;

procedure TContrato.Assinar;
var
  XMLStr: String;
  XMLUTF8: AnsiString;
begin
  // Gera novamente, para processar propriedades que podem ter sido modificadas
  XMLStr := GerarXML;

  // XML já deve estar em UTF8, para poder ser assinado //
  XMLUTF8 := ConverteXMLtoUTF8(XMLStr);

  with TACBrCIOT(TContratos(Collection).ACBrCIOT) do
  begin
    FXMLAssinado := String(XMLUTF8); // SSL.Assinar(String(XMLUTF8), 'CIOT', 'infCIOT');
    // SSL.Assinar() sempre responde em UTF8...
    FXMLOriginal := RemoverDeclaracaoXML(FXMLAssinado);

    if Configuracoes.Arquivos.Salvar then
    begin
      if NaoEstaVazio(NomeArq) then
        Gravar(NomeArq, FXMLOriginal)
      else
        Gravar(CalcularNomeArquivoCompleto(), FXMLOriginal);
    end;
  end;
end;

function TContrato.LerXML(const AXML: AnsiString): Boolean;
var
  XMLStr: String;
begin
  XMLOriginal := AXML;  // SetXMLOriginal() irá verificar se AXML está em UTF8

  { Verifica se precisa converter "AXML" de UTF8 para a String nativa da IDE.
    Isso é necessário, para que as propriedades fiquem com a acentuação correta }
  XMLStr := ParseText(AXML, True, XmlEhUTF8(AXML));

  FCIOTR.Leitor.Arquivo := XMLStr;
  FCIOTR.LerXml;

  Result := True;
end;

function TContrato.GravarXML(const NomeArquivo: String;
  const PathArquivo: String): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  FNomeArq := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);

  Result := TACBrCIOT(TContratos(Collection).ACBrCIOT).Gravar(FNomeArq, FXMLOriginal);
end;

function TContrato.GravarStream(AStream: TStream): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FXMLOriginal));
  Result := True;
end;

procedure TContrato.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings; sReplyTo: TStrings);
var
//  NomeArq : String;
  AnexosEmail:TStrings;
  StreamCIOT : TMemoryStream;
begin
  if not Assigned(TACBrCIOT(TContratos(Collection).ACBrCIOT).MAIL) then
    raise EACBrCIOTException.Create('Componente ACBrMail não associado');

  AnexosEmail := TStringList.Create;
  StreamCIOT := TMemoryStream.Create;
  try
    AnexosEmail.Clear;
    if Assigned(Anexos) then
      AnexosEmail.Assign(Anexos);

    with TACBrCIOT(TContratos(Collection).ACBrCIOT) do
    begin
      GravarStream(StreamCIOT);
      (*
      if (EnviaPDF) then
      begin
        if Assigned(DACIOT) then
        begin
          DACIOT.ImprimirDACIOTPDF(FCIOT);
          NomeArq := PathWithDelim(DACIOT.PathPDF) + NumID + '-CIOT.pdf';
          AnexosEmail.Add(NomeArq);
        end;
      end;
      *)
      EnviarEmail( sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamCIOT,
                   NumID + '-CIOT.xml', sReplyTo);
    end;
  finally
    AnexosEmail.Free;
    StreamCIOT.Free;
  end;
end;

function TContrato.GerarXML: String;
begin
  with TACBrCIOT(TContratos(Collection).ACBrCIOT) do
  begin
    FCIOTW.CIOTWClass.Gerador.Opcoes.FormatoAlerta  := Configuracoes.Geral.FormatoAlerta;
    FCIOTW.CIOTWClass.Gerador.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
    FCIOTW.CIOTWClass.Gerador.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
    FCIOTW.CIOTWClass.Gerador.Opcoes.IdentarXML     := Configuracoes.Geral.IdentarXML;

    FCIOTW.Integradora := Configuracoes.Geral.Integradora;

    TimeZoneConf.Assign( Configuracoes.WebServices.TimeZoneConf );
  end;

  FCIOTW.GerarXml;
  XMLOriginal := FCIOTW.CIOTWClass.Gerador.ArquivoFormatoXML;

  if NaoEstaVazio(FNomeArq) then
    FNomeArq := CalcularNomeArquivoCompleto('', ExtractFilePath(FNomeArq));

  FAlertas := ACBrStr( FCIOTW.CIOTWClass.Gerador.ListaDeAlertas.Text );
  Result := FXMLOriginal;
end;

function TContrato.CalcularNomeArquivo: String;
var
  xID: String;
begin
  xID := Self.NumID;

  if EstaVazio(xID) then
    raise EACBrCIOTException.Create('ID Inválido. Impossível Salvar XML');

  Result := xID + '-CIOT.xml';
end;

function TContrato.CalcularPathArquivo: String;
begin
  with TACBrCIOT(TContratos(Collection).ACBrCIOT) do
  begin
    Result := PathWithDelim(Configuracoes.Arquivos.GetPathCIOT(Now, Configuracoes.Geral.CNPJEmitente));
  end;
end;

function TContrato.CalcularNomeArquivoCompleto(NomeArquivo: String;
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

function TContrato.GetConfirmado: Boolean;
begin
//  Result := TACBrCIOT(TContratos(Collection).ACBrCIOT).cStatConfirmado(
//    FCIOT.procCIOT.cStat);
  Result := True;
end;

function TContrato.GetProcessado: Boolean;
begin
//  Result := TACBrCIOT(TContratos(Collection).ACBrCIOT).cStatProcessado(
//    FCIOT.procCIOT.cStat);
  Result := True;
end;

function TContrato.GetMsg: String;
begin
//  Result := FCIOT.procCIOT.xMotivo;
  Result := '';
end;

function TContrato.GetNumID: String;
begin
  Result := FormatDateTime('yyyymmddhhnnss', Now);
end;

function TContrato.GetXMLAssinado: String;
begin
  if EstaVazio(FXMLAssinado) then
    Assinar;

  Result := FXMLAssinado;
end;

procedure TContrato.SetXML(const AValue: String);
begin
  LerXML(AValue);
end;

procedure TContrato.SetXMLOriginal(const AValue: String);
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

{ TContratos }

constructor TContratos.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  if not (AOwner is TACBrCIOT) then
    raise EACBrCIOTException.Create('AOwner deve ser do tipo TACBrCIOT');

  inherited;

  FACBrCIOT := TACBrCIOT(AOwner);
  FConfiguracoes := TACBrCIOT(FACBrCIOT).Configuracoes;
end;

function TContratos.Add: TContrato;
begin
  Result := TContrato(inherited Add);
end;

procedure TContratos.Assinar;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].Assinar;
end;

procedure TCOntratos.GerarCIOT;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].GerarXML;
end;

function TContratos.GetItem(Index: integer): TContrato;
begin
  Result := TContrato(inherited Items[Index]);
end;

function TContratos.GetNamePath: String;
begin
  Result := 'Contrato';
end;

function TContratos.Insert(Index: integer): TContrato;
begin
  Result := TContrato(inherited Insert(Index));
end;

procedure TContratos.SetItem(Index: integer; const Value: TContrato);
begin
  Items[Index].Assign(Value);
end;

function TContratos.LoadFromFile(const CaminhoArquivo: String;
  AGerarCIOT: Boolean = True): Boolean;
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
  Result := LoadFromString(String(XMLUTF8), AGerarCIOT);

  if Result then
  begin
    // Atribui Nome do arquivo a novas notas inseridas //
    for i := l to Self.Count - 1 do
      Self.Items[i].NomeArq := CaminhoArquivo;
  end;
end;

function TContratos.LoadFromStream(AStream: TStringStream;
  AGerarCIOT: Boolean = True): Boolean;
var
  AXML: AnsiString;
begin
  AStream.Position := 0;
  AXML := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(AXML), AGerarCIOT);
end;

function TContratos.LoadFromString(AXMLString: String;
  AGerarCIOT: Boolean = True): Boolean;
var
  AXML: AnsiString;
  N: integer;

  function PosCIOT: integer;
  begin
    Result := pos('</CIOT>', AXMLString);
  end;

begin
  N := PosCIOT;

  while N > 0 do
  begin
    AXML := copy(AXMLString, 1, N + 6);
    AXMLString := Trim(copy(AXMLString, N + 7, length(AXMLString)));

    with Self.Add do
    begin
      LerXML(AXML);

      if AGerarCIOT then // Recalcula o XML
        GerarXML;
    end;

    N := PosCIOT;
  end;

  Result := Self.Count > 0;
end;

function TContratos.GravarXML(const PathNomeArquivo: String): Boolean;
var
  i: integer;
  NomeArq, PathArq: String;
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
