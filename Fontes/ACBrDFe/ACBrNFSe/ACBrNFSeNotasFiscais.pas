{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{  de Serviço eletrônica - NFSe                                                }

{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }

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

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrNFSeNotasFiscais;

interface

uses
  Classes, SysUtils, Dialogs, Forms, StrUtils,
  ACBrNFSeConfiguracoes, ACBrDFeUtil,
  pnfsNFSe, pnfsNFSeR, pnfsNFSeW, pcnConversao, pcnAuxiliar, pcnLeitor;

type

  { NotaFiscal }

  NotaFiscal = class(TCollectionItem)
  private
    FNFSe: TNFSe;
    FNFSeW: TNFSeW;
    FNFSeR: TNFSeR;

    FXML: String;
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
    function ValidarConcatChave: Boolean;
    function CalcularNomeArquivo: String;
    function CalcularPathArquivo: String;
    function CalcularNomeArquivoCompleto(NomeArquivo: String = '';
      PathArquivo: String = ''): String;

    procedure Assinar(Assina: Boolean);
    procedure Validar;
  public
    constructor Create(Collection2: TCollection); override;
    destructor Destroy; override;
    procedure Imprimir;
    procedure ImprimirPDF;

    function VerificarAssinatura: Boolean;
    function ValidarRegrasdeNegocios: Boolean;

    function LerXML(AXML: AnsiString): Boolean;

    function GerarXML: String;
    function GravarXML(NomeArquivo: String = ''; PathArquivo: String = ''): Boolean;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(sPara, sAssunto: String; sMensagem: TStrings = nil;
      EnviaPDF: Boolean = True; sCC: TStrings = nil; Anexos: TStrings = nil);

    property NomeArq: String read FNomeArq write FNomeArq;

    property NFSe: TNFSe read FNFSe;

    property XML: String read FXML;
    property XMLOriginal: String read FXMLOriginal write FXMLOriginal;
    property XMLAssinado: String read GetXMLAssinado;
    property Confirmada: Boolean read GetConfirmada;
    property Processada: Boolean read GetProcessada;
    property Msg: String read GetMsg;
    property NumID: String read GetNumID;

    property Alertas: String read FAlertas;
    property ErroValidacao: String read FErroValidacao;
    property ErroValidacaoCompleto: String read FErroValidacaoCompleto;
    property ErroRegrasdeNegocios: String read FErroRegrasdeNegocios;

  end;

  { TNotasFiscais }

  TNotasFiscais = class(TOwnedCollection)
  private
    FTransacao: Boolean;
    FNumeroLote: String;
    FACBrNFSe: TComponent;
    FConfiguracoes: TConfiguracoesNFSe;

    function GetItem(Index: integer): NotaFiscal;
    procedure SetItem(Index: integer; const Value: NotaFiscal);

    procedure VerificarDANFSE;
    procedure Validar;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    procedure GerarNFSe;
    function VerificarAssinatura(out Erros: String): Boolean;
    function ValidarRegrasdeNegocios(out Erros: String): Boolean;

    procedure Assinar(Assina: Boolean);
    procedure Imprimir;
    procedure ImprimirPDF;

    function Add: NotaFiscal;
    function Insert(Index: integer): NotaFiscal;

    property Items[Index: integer]: NotaFiscal read GetItem write SetItem; default;

    function GetNamePath: String; override;
    // Incluido o Parametro AGerarNFSe que determina se após carregar os dados da NFSe
    // para o componente, será gerado ou não novamente o XML da NFSe.
    function LoadFromFile(CaminhoArquivo: String; AGerarNFSe: Boolean = True): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarNFSe: Boolean = True): Boolean;
    function LoadFromString(AXMLString: String; AGerarNFSe: Boolean = True): Boolean;
    function GravarXML(PathNomeArquivo: String = ''): Boolean;

    property NumeroLote: String read FNumeroLote write FNumeroLote;
    property Transacao: Boolean read FTransacao write FTransacao;
    property ACBrNFSe: TComponent read FACBrNFSe;
  end;

implementation

uses
  ACBrNFSe, ACBrUtil, pnfsConversao, synautil;

{ NotaFiscal }

constructor NotaFiscal.Create(Collection2: TCollection);
begin
  inherited Create(Collection2);
  FNFSe := TNFSe.Create;
  FNFSeW := TNFSeW.Create(FNFSe);
  FNFSeR := TNFSeR.Create(FNFSe);

  with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
  begin
  (*
    FNFSe.Ide.tpNF := tnSaida;
    FNFSe.Ide.indPag := ipVista;
    FNFSe.Ide.verProc := 'ACBrNFSe';
    FNFSe.Ide.tpAmb := Configuracoes.WebServices.Ambiente;
    FNFSe.Ide.tpEmis := Configuracoes.Geral.FormaEmissao;

    if Assigned(DANFSE) then
      FNFSe.Ide.tpImp := DANFSE.TipoDANFSE;

    FNFSe.Emit.EnderEmit.xPais := 'BRASIL';
    FNFSe.Emit.EnderEmit.cPais := 1058;
    FNFSe.Emit.EnderEmit.nro := 'SEM NUMERO';
   *)
  end;
end;

destructor NotaFiscal.Destroy;
begin
  FNFSeW.Free;
  FNFSeR.Free;
  FNFSe.Free;
  inherited Destroy;
end;

procedure NotaFiscal.Imprimir;
begin
  with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
  begin
    if not Assigned(DANFSE) then
      raise EACBrNFSeException.Create('Componente DANFSE não associado.')
    else
      DANFSE.ImprimirDANFSE(NFSe);
  end;
end;

procedure NotaFiscal.ImprimirPDF;
begin
  with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
  begin
    if not Assigned(DANFSE) then
      raise EACBrNFSeException.Create('Componente DANFSE não associado.')
    else
      DANFSE.ImprimirDANFSEPDF(NFSe);
  end;
end;

procedure NotaFiscal.Assinar(Assina: Boolean);
var
  XMLAss: String;
  ArqXML: String;
  Leitor: TLeitor;
begin
  ArqXML := GerarXML;

  // XML já deve estar em UTF8, para poder ser assinado //
  ArqXML := ConverteXMLtoUTF8(ArqXML);
  FXMLOriginal := ArqXML;

  with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
  begin
    if Assina then
    begin
      XMLAss := SSL.Assinar(ArqXML, 'NFSe', 'infNFSe');
      FXMLAssinado := XMLAss;

      // Remove header, pois podem existir várias Notas no XML //
      //TODO: Verificar se precisa
      //XMLAss := StringReplace(XMLAss, '<' + ENCODING_UTF8_STD + '>', '', [rfReplaceAll]);
      //XMLAss := StringReplace(XMLAss, '<' + XML_V01 + '>', '', [rfReplaceAll]);

      Leitor := TLeitor.Create;
      try
        leitor.Grupo := XMLAss;
        NFSe.signature.URI := Leitor.rAtributo('Reference URI=');
        NFSe.signature.DigestValue := Leitor.rCampo(tcStr, 'DigestValue');
        NFSe.signature.SignatureValue := Leitor.rCampo(tcStr, 'SignatureValue');
        NFSe.signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');
      finally
        Leitor.Free;
      end;

      if Configuracoes.Geral.Salvar then
        Gravar(CalcularNomeArquivoCompleto(), XMLAss);

      if NaoEstaVazio(NomeArq) then
        Gravar(NomeArq, XMLAss);
    end;
  end;
end;

procedure NotaFiscal.Validar;
var
  Erro, AXML: String;
  NotaEhValida: Boolean;
  ALayout: TLayOutNFSe;
begin
  AXML := FXMLAssinado;

  if EstaVazio(AXML) then
  begin
//    Assinar;
    AXML := FXMLAssinado;
  end;

  with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
  begin
  (*
    if EhAutorizacao then
      ALayout := LayNFSeRetAutorizacao
    else
      ALayout := LayNFSeRetRecepcao;
  *)
    NotaEhValida := SSL.Validar(AXML, GerarNomeArqSchema(ALayout, 1.00 {FNFSe.infNFSe.Versao}), Erro);

    if not NotaEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados da nota: ') +
        NFSe.IdentificacaoRps.Numero + sLineBreak + FAlertas ;
      FErroValidacaoCompleto := FErroValidacao + sLineBreak + Erro;

      raise EACBrNFSeException.CreateDef(
        IfThen(Configuracoes.Geral.ExibirErroSchema, ErroValidacaoCompleto,
        ErroValidacao));
    end;
  end;
end;

function NotaFiscal.VerificarAssinatura: Boolean;
var
  Erro, AXML: String;
  AssEhValida: Boolean;
begin
  AXML := FXMLOriginal;

  if EstaVazio(AXML) then
  begin
//    if EstaVazio(FXMLAssinado) then
//      Assinar;

    AXML := FXMLAssinado;
  end;

  with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
  begin
    AssEhValida := SSL.VerificarAssinatura(AXML, Erro);

    if not AssEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação da assinatura da nota: ') +
        NFSe.IdentificacaoRps.Numero + sLineBreak + Erro;
    end;
  end;

  Result := AssEhValida;
end;

function NotaFiscal.ValidarRegrasdeNegocios: Boolean;
var
  Erros: String;

  procedure AdicionaErro(const Erro: String);
  begin
    Erros := Erros + Erro + sLineBreak;
  end;

begin
  with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
  begin
    Erros := '';
    (*
    if not ValidarConcatChave then  //A03-10
      AdicionaErro(
        '502-Rejeição: Erro na Chave de Acesso - Campo Id não corresponde à concatenação dos campos correspondentes');

      if (NFSe.Ide.indPres = pcEntregaDomicilio) then //B25b-10
        AdicionaErro('794-Rejeição: NF-e com indicativo de NFC-e com entrega a domicílio');
    end;
    *)
  end;

  Result := EstaVazio(Erros);

  if not Result then
  begin
    Erros := ACBrStr('Erro(s) nas Regras de negócios da nota: '+
                     NFSe.IdentificacaoRps.Numero + sLineBreak +
                     Erros);
  end;

  FErroRegrasdeNegocios := Erros;
end;

function NotaFiscal.LerXML(AXML: AnsiString): Boolean;
//var
//  Ok: Boolean;
begin
  Result := False;
  FNFSeR.Leitor.Arquivo := AXML;
  FNFSeR.LerXml;

  FXML := string(AXML);
  FXMLOriginal := FXML;
  Result := True;
end;

function NotaFiscal.GravarXML(NomeArquivo: String; PathArquivo: String): Boolean;
begin
  FNomeArq := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);
  GerarXML;
  Result := TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe).Gravar(FNomeArq, FXML);
end;

function NotaFiscal.GravarStream(AStream: TStream): Boolean;
begin
  Result := False;
  GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FXML) );
  Result := True;
end;

procedure NotaFiscal.EnviarEmail(sPara, sAssunto: String; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings);
var
  NomeArq : String;
  AnexosEmail:TStrings;
  StreamNFSe : TMemoryStream;
begin
  if not Assigned(TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe).MAIL) then
    raise EACBrNFSeException.Create('Componente ACBrMail não associado');

  AnexosEmail := TStringList.Create;
  StreamNFSe := TMemoryStream.Create;
  try
    AnexosEmail.Clear;
    if Assigned(Anexos) then
      AnexosEmail.Assign(Anexos);

    with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
    begin
      GravarStream(StreamNFSe);

      if (EnviaPDF) then
      begin
        if Assigned(DANFSE) then
        begin
          DANFSE.ImprimirDANFSEPDF(FNFSe);
          NomeArq := PathWithDelim(DANFSE.PathPDF) + NumID + '-NFSe.pdf';
          AnexosEmail.Add(NomeArq);
        end;
      end;

      EnviarEmail( sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamNFSe,
                   NumID +'-NFSe.xml');
    end;
  finally
    AnexosEmail.Free;
    StreamNFSe.Free;
  end;
end;

function NotaFiscal.GerarXML: String;
begin
  with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
  begin
    FNFSeW.Gerador.Opcoes.FormatoAlerta := Configuracoes.Geral.FormatoAlerta;
    FNFSeW.Gerador.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
  end;

  FNFSeW.GerarXml;
  FXML := FNFSeW.Gerador.ArquivoFormatoXML;
  FXMLAssinado := '';
  FAlertas := FNFSeW.Gerador.ListaDeAlertas.Text;
  Result := FXML;
end;

function NotaFiscal.CalcularNomeArquivo: String;
var
  xID: String;
begin
  xID := Self.NumID;

  if EstaVazio(xID) then
    raise EACBrNFSeException.Create('ID Inválido. Impossível Salvar XML');

  Result := xID + '-NFSe.xml';
end;

function NotaFiscal.CalcularPathArquivo: String;
var
  Data: TDateTime;
begin
  with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
  begin
    if Configuracoes.Arquivos.EmissaoPathNFSe then
      Data := FNFSe.DataEmissaoRps
    else
      Data := Now;

    Result := PathWithDelim(Configuracoes.Arquivos.GetPathNFSe(Data, FNFSe.Prestador.Cnpj));
  end;
end;

function NotaFiscal.CalcularNomeArquivoCompleto(NomeArquivo: String;
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

function NotaFiscal.ValidarConcatChave: Boolean;
var
  wAno, wMes, wDia: word;
begin
  DecodeDate(NFSe.DataEmissaoRps, wAno, wMes, wDia);
(*
  Result := not
    ((Copy(NFSe.infNFSe.ID, 4, 2) <> IntToStrZero(NFSe.Ide.cUF, 2)) or
    (Copy(NFSe.infNFSe.ID, 6, 2)  <> Copy(FormatFloat('0000', wAno), 3, 2)) or
    (Copy(NFSe.infNFSe.ID, 8, 2)  <> FormatFloat('00', wMes)) or
    (Copy(NFSe.infNFSe.ID, 10, 14)<> PadLeft(OnlyNumber(NFSe.Emit.CNPJCPF), 14, '0')) or
    (Copy(NFSe.infNFSe.ID, 24, 2) <> IntToStrZero(NFSe.Ide.modelo, 2)) or
    (Copy(NFSe.infNFSe.ID, 26, 3) <> IntToStrZero(NFSe.Ide.serie, 3)) or
    (Copy(NFSe.infNFSe.ID, 29, 9) <> IntToStrZero(NFSe.Ide.nNF, 9)) or
    (Copy(NFSe.infNFSe.ID, 38, 1) <> TpEmisToStr(NFSe.Ide.tpEmis)) or
    (Copy(NFSe.infNFSe.ID, 39, 8) <> IntToStrZero(NFSe.Ide.cNF, 8)));
*)
 Result := True;
end;

function NotaFiscal.GetConfirmada: Boolean;
begin
//  Result := TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe).CstatConfirmada(
//    FNFSe.procNFSe.cStat);
end;

function NotaFiscal.GetProcessada: Boolean;
begin
//  Result := TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe).CstatProcessado(
//    FNFSe.procNFSe.cStat);
end;

function NotaFiscal.GetMsg: String;
begin
//  Result := FNFSe.procNFSe.xMotivo;
end;

function NotaFiscal.GetNumID: String;
begin
  Result := Trim(OnlyNumber(NFSe.InfID.ID));
end;

function NotaFiscal.GetXMLAssinado: String;
begin
//  if EstaVazio(FXMLAssinado) then
//    Assinar;

  Result := FXMLAssinado;
end;

{ TNotasFiscais }

constructor TNotasFiscais.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  if not (AOwner is TACBrNFSe) then
    raise EACBrNFSeException.Create('AOwner deve ser do tipo TACBrNFSe');

  inherited;

  FACBrNFSe := TACBrNFSe(AOwner);
  FConfiguracoes := TACBrNFSe(FACBrNFSe).Configuracoes;
end;


function TNotasFiscais.Add: NotaFiscal;
begin
  Result := NotaFiscal(inherited Add);
end;

procedure TNotasFiscais.Assinar(Assina: Boolean);
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].Assinar(Assina);
end;

procedure TNotasFiscais.GerarNFSe;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].GerarXML;
end;

function TNotasFiscais.GetItem(Index: integer): NotaFiscal;
begin
  Result := NotaFiscal(inherited Items[Index]);
end;

function TNotasFiscais.GetNamePath: String;
begin
  Result := 'NotaFiscal';
end;

procedure TNotasFiscais.VerificarDANFSE;
begin
  if not Assigned(TACBrNFSe(FACBrNFSe).DANFSE) then
    raise EACBrNFSeException.Create('Componente DANFSE não associado.');
end;

procedure TNotasFiscais.Imprimir;
begin
  VerificarDANFSE;
  TACBrNFSe(FACBrNFSe).DANFSE.ImprimirDANFSE(nil);
end;

procedure TNotasFiscais.ImprimirPDF;
begin
  VerificarDANFSE;
  TACBrNFSe(FACBrNFSe).DANFSE.ImprimirDANFSEPDF(nil);
end;

function TNotasFiscais.Insert(Index: integer): NotaFiscal;
begin
  Result := NotaFiscal(inherited Insert(Index));
end;

procedure TNotasFiscais.SetItem(Index: integer; const Value: NotaFiscal);
begin
  Items[Index].Assign(Value);
end;

procedure TNotasFiscais.Validar;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].Validar;   // Dispara exception em caso de erro
end;

function TNotasFiscais.VerificarAssinatura(out Erros: String): Boolean;
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

function TNotasFiscais.ValidarRegrasdeNegocios(out Erros: String): Boolean;
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

function TNotasFiscais.LoadFromFile(CaminhoArquivo: String;
  AGerarNFSe: Boolean = True): Boolean;
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
    LoadFromString(XML, AGerarNFSe);

    for i := 0 to Self.Count - 1 do
      Self.Items[i].NomeArq := CaminhoArquivo;

    Result := True;
  finally
    ArquivoXML.Free;
  end;
end;

function TNotasFiscais.LoadFromStream(AStream: TStringStream;
  AGerarNFSe: Boolean = True): Boolean;
var
  XMLOriginal: AnsiString;
begin
  Result := False;
  AStream.Position := 0;
  XMLOriginal := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(XMLOriginal), AGerarNFSe);
end;

function TNotasFiscais.LoadFromString(AXMLString: String;
  AGerarNFSe: Boolean = True): Boolean;
var
  AXML: AnsiString;
  P, N: integer;

  function PosNFSe: integer;
  begin
    Result := pos('</NFSe>', AXMLString);
  end;

begin
  Result := False;
  N := PosNFSe;
  while N > 0 do
  begin
    P := pos('</NFSeProc>', AXMLString);
    if P > 0 then
    begin
      AXML := copy(AXMLString, 1, P + 5);
      AXMLString := Trim(copy(AXMLString, P + 10, length(AXMLString)));
    end
    else
    begin
      AXML := copy(AXMLString, 1, N + 5);
      AXMLString := Trim(copy(AXMLString, N + 6, length(AXMLString)));
    end;

    with Self.Add do
    begin
      LerXML(AXML);

      if AGerarNFSe then // Recalcula o XML
        GerarXML;
    end;

    N := PosNFSe;
  end;
end;

function TNotasFiscais.GravarXML(PathNomeArquivo: String): Boolean;
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
