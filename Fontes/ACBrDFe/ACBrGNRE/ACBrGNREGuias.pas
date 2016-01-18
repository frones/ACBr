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

unit ACBrGNREGuias;

interface

uses
  Classes, SysUtils, Dialogs, StrUtils,
  ACBrDFeUtil, pcnConversao, pcnAuxiliar, pcnLeitor,
  ACBrGNReConfiguracoes,
  pgnreGNRE, pgnreGNRER, pgnreGNREW;

type

  { Guia }

  Guia = class(TCollectionItem)
  private
    FGNRe: TGNRe;
    FGNReW: TGNReW;
    FGNReR: TGNReR;

    FXMLAssinado: String;
    FXMLOriginal: String;
    FAlertas: String;
    FErroValidacao: String;
    FErroValidacaoCompleto: String;
    FErroRegrasdeNegocios: String;
    FNomeArq: String;

    function GetConfirmada: Boolean;
    function GetProcessada: Boolean;


    function GetMsg: String;
    function GetNumID: String;
    function GetXMLAssinado: String;
    procedure SetXML(AValue: String);
    procedure SetXMLOriginal(AValue: String);
    function ValidarConcatChave: Boolean;
    function CalcularNomeArquivo: String;
    function CalcularPathArquivo: String;
    function CalcularNomeArquivoCompleto(NomeArquivo: String = '';
      PathArquivo: String = ''): String;
  public
    constructor Create(Collection2: TCollection); override;
    destructor Destroy; override;
    procedure Imprimir;
    procedure ImprimirPDF;

    procedure Assinar;
    procedure Validar;
    function VerificarAssinatura: Boolean;
    function ValidarRegrasdeNegocios: Boolean;

    function LerXML(AXML: AnsiString): Boolean;

    function GerarXML: String;
    function GravarXML(NomeArquivo: String = ''; PathArquivo: String = ''): Boolean;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(sPara, sAssunto: String; sMensagem: TStrings = nil;
      EnviaPDF: Boolean = True; sCC: TStrings = nil; Anexos: TStrings = nil);

    property NomeArq: String read FNomeArq write FNomeArq;

    property GNRe: TGNRe read FGNRe;

    // Atribuir a "XML", faz o componente transferir os dados lido para as propriedades internas e "XMLAssinado"
    property XML: String         read FXMLOriginal   write SetXML;
    // Atribuir a "XMLOriginal", reflete em XMLAssinado, se existir a tag de assinatura
    property XMLOriginal: String read FXMLOriginal   write SetXMLOriginal;
    property XMLAssinado: String read GetXMLAssinado write FXMLAssinado;
    property Confirmada: Boolean read GetConfirmada;
    property Processada: Boolean read GetProcessada;
    property Msg: String read GetMsg;
    property NumID: String read GetNumID;

    property Alertas: String read FAlertas;
    property ErroValidacao: String read FErroValidacao;
    property ErroValidacaoCompleto: String read FErroValidacaoCompleto;
    property ErroRegrasdeNegocios: String read FErroRegrasdeNegocios;
  end;

  { TGuias }

  TGuias = class(TOwnedCollection)
  private
    FACBrGNRe: TComponent;
    FConfiguracoes: TConfiguracoesGNRe;

    function GetItem(Index: integer): Guia;
    procedure SetItem(Index: integer; const Value: Guia);

    procedure VerificarGNREGuias;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    procedure GerarGNRe;
    procedure Assinar;
    procedure Validar;
    function VerificarAssinatura(out Erros: String): Boolean;
    function ValidarRegrasdeNegocios(out Erros: String): Boolean;
    procedure Imprimir;
    procedure ImprimirPDF;
    function Add: Guia;
    function Insert(Index: integer): Guia;

    property Items[Index: integer]: Guia read GetItem write SetItem; default;

    function GetNamePath: String; override;
    // Incluido o Parametro AGerarGNRe que determina se após carregar os dados da GNRe
    // para o componente, será gerado ou não novamente o XML da GNRe.
    function LoadFromFile(CaminhoArquivo: String; AGerarGNRe: Boolean = True): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarGNRe: Boolean = True): Boolean;
    function LoadFromString(AXMLString: String; AGerarGNRe: Boolean = True): Boolean;
    function GravarXML(PathNomeArquivo: String = ''): Boolean;

    property ACBrGNRe: TComponent read FACBrGNRe;
  end;

implementation

uses
  ACBrGNRe, ACBrUtil, pgnreConversao, synautil;

{ Guia }

constructor Guia.Create(Collection2: TCollection);
begin
  inherited Create(Collection2);
  FGNRe := TGNRe.Create;
  FGNReW := TGNReW.Create(FGNRe);
  FGNReR := TGNReR.Create(FGNRe);
  (*
  with TACBrGNRe(TGuias(Collection).ACBrGNRe) do
  begin
    FGNRe.Ide.modelo := StrToInt(ModeloDFToStr(Configuracoes.Geral.ModeloDF));
    FGNRe.infGNRe.Versao := VersaoDFToDbl(Configuracoes.Geral.VersaoDF);

    FGNRe.Ide.tpNF := tnSaida;
    FGNRe.Ide.indPag := ipVista;
    FGNRe.Ide.verProc := 'ACBrGNRe';
    FGNRe.Ide.tpAmb := Configuracoes.WebServices.Ambiente;
    FGNRe.Ide.tpEmis := Configuracoes.Geral.FormaEmissao;

    if Assigned(FGNREGuia) then
      FGNRe.Ide.tpImp := FGNREGuia.TipoDANFE;

    FGNRe.Emit.EnderEmit.xPais := 'BRASIL';
    FGNRe.Emit.EnderEmit.cPais := 1058;
    FGNRe.Emit.EnderEmit.nro := 'SEM NUMERO';

    FGNRe.Dest.EnderDest.xPais := 'BRASIL';
    FGNRe.Dest.EnderDest.cPais := 1058;
  end;
  *)
end;

destructor Guia.Destroy;
begin
  FGNReW.Free;
  FGNReR.Free;
  FGNRe.Free;
  inherited Destroy;
end;

procedure Guia.Imprimir;
begin
  with TACBrGNRe(TNotasFiscais(Collection).ACBrGNRe) do
  begin
    if not Assigned(FGNREGuia) then
      raise EACBrGNReException.Create('Componente FGNREGuia não associado.')
    else
      FGNREGuia.ImprimirGuia(GNRe);
  end;
end;

procedure Guia.ImprimirPDF;
begin
  with TACBrGNRe(TGuias(Collection).ACBrGNRe) do
  begin
    if not Assigned(FGNREGuia) then
      raise EACBrGNReException.Create('Componente FGNREGuia não associado.')
    else
      FGNREGuia.ImprimirGuiaPDF(GNRe);
  end;
end;

procedure Guia.Assinar;
var
  XMLStr: String;
  XMLUTF8: AnsiString;
  Leitor: TLeitor;
  CNPJEmitente, CNPJCertificado: String;
begin
  // Verificando se pode assinar esse XML (O XML tem o mesmo CNPJ do Certificado ??)
  CNPJEmitente    := OnlyNumber(GNRe.Emit.CNPJCPF);
  CNPJCertificado := OnlyNumber(TACBrGNRe(TGuias(Collection).ACBrGNRe).SSL.CertCNPJ);

  // verificar somente os 8 primeiros digitos, para evitar problemas quando
  // a filial estiver utilizando o certificado da matriz
  if (CNPJCertificado <> '') and (Copy(CNPJEmitente, 1, 8) <> Copy(CNPJCertificado, 1, 8)) then
    raise EACBrGNReException.Create('Erro ao Assinar. O XML informado possui CNPJ diferente do Certificado Digital' );

  // Gera novamente, para processar propriedades que podem ter sido modificadas
  XMLStr := GerarXML;

  // XML já deve estar em UTF8, para poder ser assinado //
  XMLUTF8 := ConverteXMLtoUTF8(XMLStr);

  with TACBrGNRe(TGuias(Collection).ACBrGNRe) do
  begin
    FXMLAssinado := SSL.Assinar(String(XMLUTF8), 'GNRe', 'infGNRe');
    FXMLOriginal := FXMLAssinado;

    Leitor := TLeitor.Create;
    try
      leitor.Grupo := FXMLAssinado;
      GNRe.signature.URI := Leitor.rAtributo('Reference URI=');
      GNRe.signature.DigestValue := Leitor.rCampo(tcStr, 'DigestValue');
      GNRe.signature.SignatureValue := Leitor.rCampo(tcStr, 'SignatureValue');
      GNRe.signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');
    finally
      Leitor.Free;
    end;

    if Configuracoes.Arquivos.Salvar and
       (not Configuracoes.Arquivos.SalvarApenasGNReProcessadas) then
    begin
      if NaoEstaVazio(NomeArq) then
        Gravar(NomeArq, FXMLAssinado)
      else
        Gravar(CalcularNomeArquivoCompleto(), FXMLAssinado);
    end;
  end;
end;

procedure Guia.Validar;
var
  Erro, AXML: String;
  NotaEhValida, ok: Boolean;
  ALayout: TLayOut;
  VerServ: Real;
  Modelo: TpcnModeloDF;
  cUF: Integer;
begin
  AXML := XMLAssinado;

  with TACBrGNRe(TGuias(Collection).ACBrGNRe) do
  begin
    VerServ := FGNRe.infGNRe.Versao;
    Modelo  := StrToModeloDF(ok, IntToStr(FGNRe.Ide.modelo));
    cUF     := FGNRe.Ide.cUF;

    if EhAutorizacao( DblToVersaoDF(ok, VerServ), Modelo, cUF) then
      ALayout := LayGNReAutorizacao
    else
      ALayout := LayGNReRecepcao;

    // Extraindo apenas os dados da GNRe (sem GNReProc)
    AXML := '<GNRe xmlns' + RetornarConteudoEntre(AXML, '<GNRe xmlns', '</GNRe>') + '</GNRe>';

    NotaEhValida := SSL.Validar(AXML, GerarNomeArqSchema(ALayout, VerServ), Erro);

    if not NotaEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados da nota: ') +
        IntToStr(GNRe.Ide.nNF) + sLineBreak + FAlertas ;
      FErroValidacaoCompleto := FErroValidacao + sLineBreak + Erro;

      raise EACBrGNReException.CreateDef(
        IfThen(Configuracoes.Geral.ExibirErroSchema, ErroValidacaoCompleto,
        ErroValidacao));
    end;
  end;
end;

function Guia.VerificarAssinatura: Boolean;
var
  Erro, AXML: String;
  AssEhValida: Boolean;
begin
  AXML := XMLAssinado;

  with TACBrGNRe(TGuias(Collection).ACBrGNRe) do
  begin
    AssEhValida := SSL.VerificarAssinatura(AXML, Erro, 'infGNRe');

    if not AssEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação da assinatura da nota: ') +
        IntToStr(GNRe.Ide.nNF) + sLineBreak + Erro;
    end;
  end;

  Result := AssEhValida;
end;

function Guia.ValidarRegrasdeNegocios: Boolean;
begin
  Result := True; // Não Implementado
end;

function Guia.LerXML(AXML: AnsiString): Boolean;
begin
  Result := False;
  FGNReR.Leitor.Arquivo := AXML;
  FGNReR.LerXml;

  XMLOriginal := string(AXML);

  Result := True;
end;

function Guia.GravarXML(NomeArquivo, PathArquivo: String): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  FNomeArq := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);

  Result := TACBrGNRe(TGuias(Collection).ACBrGNRe).Gravar(FNomeArq, FXMLOriginal);
end;

function Guia.GravarStream(AStream: TStream): Boolean;
begin
  Result := False;

  if EstaVazio(FXMLOriginal) then
    GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FXMLOriginal));
  Result := True;
end;

procedure Guia.EnviarEmail(sPara, sAssunto: String; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC, Anexos: TStrings);
var
  NomeArq : String;
  AnexosEmail:TStrings;
  StreamGNRe : TMemoryStream;
begin
  if not Assigned(TACBrGNRe(TGuias(Collection).ACBrGNRe).MAIL) then
    raise EACBrGNReException.Create('Componente ACBrMail não associado');

  AnexosEmail := TStringList.Create;
  StreamGNRe := TMemoryStream.Create;
  try
    AnexosEmail.Clear;
    if Assigned(Anexos) then
      AnexosEmail.Assign(Anexos);

    with TACBrGNRe(TGuias(Collection).ACBrGNRe) do
    begin
      GravarStream(StreamGNRe);

      if (EnviaPDF) then
      begin
        if Assigned(FGNREGuia) then
        begin
          FGNREGuia.ImprimirGuiaPDF(FGNRe);
          NomeArq := PathWithDelim(FGNREGuia.PathPDF) + NumID + '-gnre.pdf';
          AnexosEmail.Add(NomeArq);
        end;
      end;

      EnviarEmail( sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamGNRe,
                   NumID + '-gnre.xml');
    end;
  finally
    AnexosEmail.Free;
    StreamGNRe.Free;
  end;
end;

function Guia.GerarXML: String;
var
  IdAnterior : String;
begin
  with TACBrGNRe(TGuias(Collection).ACBrGNRe) do
  begin
    IdAnterior := GNRe.infGNRe.ID;
    FGNReW.Gerador.Opcoes.FormatoAlerta := Configuracoes.Geral.FormatoAlerta;
    FGNReW.Gerador.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
  end;

  FGNReW.Opcoes.GerarTXTSimultaneamente := False;

  FGNReW.GerarXml;
  //DEBUG
  //WriteToTXT('c:\temp\Guia.xml', FGNReW.Gerador.ArquivoFormatoXML, False, False);
  XMLOriginal := FGNReW.Gerador.ArquivoFormatoXML;

  // XML gerado pode ter nova Chave e ID, então devemos calcular novamente
  // o nome do arquivo, mantendo o PATH do arquivo carregado
  if (NaoEstaVazio(FNomeArq) and (IdAnterior <> FGNRe.infGNRe.ID)) then
    FNomeArq := CalcularNomeArquivoCompleto('', ExtractFilePath(FNomeArq));

  FAlertas := ACBrStr( FGNReW.Gerador.ListaDeAlertas.Text );
  Result := FXMLOriginal;
end;

function Guia.CalcularNomeArquivo: String;
var
  xID: String;
  NomeXML: String;
begin
  xID := Self.NumID;

  if EstaVazio(xID) then
    raise EACBrGNReException.Create('ID Inválido. Impossível Salvar XML');

  NomeXML := '-gnre.xml';

  Result := xID + NomeXML;
end;

function Guia.CalcularPathArquivo: String;
var
  Data: TDateTime;
begin
  with TACBrGNRe(TGuias(Collection).ACBrGNRe) do
  begin
    if Configuracoes.Arquivos.EmissaoPathGNRe then
      Data := FGNRe.Ide.dEmi
    else
      Data := Now;

    Result := PathWithDelim(Configuracoes.Arquivos.GetPathGNRe(Data, FGNRe.Emit.CNPJCPF, FGNRe.Ide.modelo));
  end;
end;

function Guia.CalcularNomeArquivoCompleto(NomeArquivo,
  PathArquivo: String): String;
begin
  if EstaVazio(NomeArquivo) then
    NomeArquivo := CalcularNomeArquivo;

  if EstaVazio(PathArquivo) then
    PathArquivo := CalcularPathArquivo
  else
    PathArquivo := PathWithDelim(PathArquivo);

  Result := PathArquivo + NomeArquivo;
end;

function Guia.ValidarConcatChave: Boolean;
var
  wAno, wMes, wDia: word;
  chaveGNRe : String;
begin
  DecodeDate(GNRe.ide.dEmi, wAno, wMes, wDia);

  chaveGNRe := 'GNRe' + OnlyNumber(GNRe.infGNRe.ID);
  {(*}
  Result := not
    ((Copy(chaveGNRe, 4, 2) <> IntToStrZero(GNRe.Ide.cUF, 2)) or
    (Copy(chaveGNRe, 6, 2)  <> Copy(FormatFloat('0000', wAno), 3, 2)) or
    (Copy(chaveGNRe, 8, 2)  <> FormatFloat('00', wMes)) or
    (Copy(chaveGNRe, 10, 14)<> PadLeft(OnlyNumber(GNRe.Emit.CNPJCPF), 14, '0')) or
    (Copy(chaveGNRe, 24, 2) <> IntToStrZero(GNRe.Ide.modelo, 2)) or
    (Copy(chaveGNRe, 26, 3) <> IntToStrZero(GNRe.Ide.serie, 3)) or
    (Copy(chaveGNRe, 29, 9) <> IntToStrZero(GNRe.Ide.nNF, 9)) or
    (Copy(chaveGNRe, 38, 1) <> TpEmisToStr(GNRe.Ide.tpEmis)) or
    (Copy(chaveGNRe, 39, 8) <> IntToStrZero(GNRe.Ide.cNF, 8)));
  {*)}
end;

function Guia.GetConfirmada: Boolean;
begin
  Result := TACBrGNRe(TGuias(Collection).ACBrGNRe).CstatConfirmada(
    FGNRe.procGNRe.cStat);
end;

function Guia.GetProcessada: Boolean;
begin
  Result := TACBrGNRe(TGuias(Collection).ACBrGNRe).CstatProcessado(
    FGNRe.procGNRe.cStat);
end;

function Guia.GetMsg: String;
begin
  Result := FGNRe.procGNRe.xMotivo;
end;

function Guia.GetNumID: String;
begin
  Result := OnlyNumber(GNRe.infGNRe.ID);
end;

function Guia.GetXMLAssinado: String;
begin
  if EstaVazio(FXMLAssinado) then
    Assinar;

  Result := FXMLAssinado;
end;

procedure Guia.SetXML(AValue: String);
begin
  LerXML(AValue);
end;

procedure Guia.SetXMLOriginal(AValue: String);
begin
  FXMLOriginal := AValue;

  if XmlEstaAssinado(FXMLOriginal) then
    FXMLAssinado := FXMLOriginal
  else
    FXMLAssinado := '';
end;

{ TGuias }

constructor TGuias.Create(AOwner: TPersistent;
  ItemClass: TCollectionItemClass);
begin
  if not (AOwner is TACBrGNRe) then
    raise EACBrGNReException.Create('AOwner deve ser do tipo TACBrGNRe');

  inherited;

  FACBrGNRe := TACBrGNRe(AOwner);
  FConfiguracoes := TACBrGNRe(FACBrGNRe).Configuracoes;
end;

function TGuias.Add: Guia;
begin
  Result := Guia(inherited Add);
end;

procedure TGuias.Assinar;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].Assinar;
end;

procedure TGuias.GerarGNRe;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].GerarXML;
end;

function TGuias.GetItem(Index: integer): Guia;
begin
  Result := Guia(inherited Items[Index]);
end;

function TGuias.GetNamePath: String;
begin
  Result := 'Guia';
end;

procedure TGuias.VerificarGNREGuias;
begin
begin
  if not Assigned(TACBrGNRe(FACBrGNRe).GNREGuia) then
    raise EACBrGNReException.Create('Componente FGNREGuia não associado.');
end;

procedure TGuias.Imprimir;
begin
  VerificarGNREGuias;
  TACBrGNRe(FACBrGNRe).DANFE.ImprimirGuia(nil);
end;

procedure TGuias.ImprimirPDF;
begin
  VerificarGNREGuias;
  TACBrGNRe(FACBrGNRe).DANFE.ImprimirGuiaPDF(nil);
end;

function TGuias.Insert(Index: integer): Guia;
begin
  Result := Guia(inherited Insert(Index));
end;

procedure TGuias.SetItem(Index: integer; const Value: Guia);
begin
  Items[Index].Assign(Value);
end;

procedure TGuias.Validar;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].Validar;   // Dispara exception em caso de erro
end;

function TGuias.VerificarAssinatura(out Erros: String): Boolean;
var
  i: integer;
begin
  Result := True;
  Erros := '';

  for i := 0 to Self.Count - 1 do
  begin
    if not Self.Items[i].VerificarAssinatura then
    begin
      Result := False;
      Erros := Erros + Self.Items[i].ErroValidacao + sLineBreak;
    end;
  end;
end;

function TGuias.ValidarRegrasdeNegocios(out Erros: String): Boolean;
var
  i: integer;
begin
  Result := True;
  Erros := '';

  for i := 0 to Self.Count - 1 do
  begin
    if not Self.Items[i].ValidarRegrasdeNegocios then
    begin
      Result := False;
      Erros := Erros + Self.Items[i].ErroRegrasdeNegocios + sLineBreak;
    end;
  end;
end;

function TGuias.LoadFromFile(CaminhoArquivo: String;
  AGerarGNRe: Boolean): Boolean;
  XMLStr: String;
  XMLUTF8: AnsiString;
  i, l: integer;
  MS: TMemoryStream;
begin
  Result := False;

  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile(CaminhoArquivo);
    XMLUTF8 := ReadStrFromStream(MS, MS.Size);
  finally
    MS.Free;
  end;

  l := Self.Count; // Indice da última guia já existente

  // Converte de UTF8 para a String nativa da IDE //
  XMLStr := DecodeToString(XMLUTF8, True);
  LoadFromString(XMLStr, AGerarGNRe);

  // Atribui Nome do arquivo a novas guias inseridas //
  for i := l to Self.Count - 1 do
    Self.Items[i].NomeArq := CaminhoArquivo;

  Result := True;
end;

function TGuias.LoadFromStream(AStream: TStringStream;
  AGerarGNRe: Boolean): Boolean;
var
  AXML: AnsiString;
begin
  Result := False;
  AStream.Position := 0;
  AXML := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(AXML), AGerarGNRe);
end;

function TGuias.LoadFromString(AXMLString: String;
  AGerarGNRe: Boolean): Boolean;
var
  AXML: AnsiString;
  P, N: integer;

  function PosGNRe: integer;
  begin
    Result := pos('</guias>', AXMLString);
  end;

begin
  N := PosGNRe;
  while N > 0 do
  begin
    P := pos('</GNReProc>', AXMLString);
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
      LerXML(AXML);

      if AGerarGNRe then // Recalcula o XML
        GerarXML;
    end;

    N := PosGNRe;
  end;

  Result := Self.Count > 0;
end;

function TGuias.GravarXML(PathNomeArquivo: String): Boolean;
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
