{******************************************************************************}
{ Projeto: Componente ACBrMDFe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
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

{*******************************************************************************
|* Historico
|*
|* 01/08/2012: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit ACBrMDFeManifestos;

interface

uses
  Classes, SysUtils, Dialogs, Forms, StrUtils,
  ACBrMDFeConfiguracoes, ACBrDFeUtil,
  pmdfeMDFe, pmdfeMDFeR, pmdfeMDFeW, pcnConversao, pcnAuxiliar, pcnLeitor;

type

  { Manifesto }

  Manifesto = class(TCollectionItem)
  private
    FMDFe: TMDFe;
    FMDFeW: TMDFeW;
    FMDFeR: TMDFeR;

    FXML: String;
    FXMLAssinado: String;
    FXMLOriginal: String;
    FAlertas: String;
    FErroValidacao: String;
    FErroValidacaoCompleto: String;
    FErroRegrasdeNegocios: String;
    FNomeArq: String;

    function GetConfirmado: Boolean;
    function GetProcessado: Boolean;

    function GetMsg: String;
    function GetNumID: String;
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

    property MDFe: TMDFe read FMDFe;

    property XML: String read FXML;
    property XMLOriginal: String read FXMLOriginal write FXMLOriginal;
    property XMLAssinado: String read FXMLAssinado;
    property Confirmado: Boolean read GetConfirmado;
    property Processado: Boolean read GetProcessado;
    property Msg: String read GetMsg;
    property NumID: String read GetNumID;

    property Alertas: String read FAlertas;
    property ErroValidacao: String read FErroValidacao;
    property ErroValidacaoCompleto: String read FErroValidacaoCompleto;
    property ErroRegrasdeNegocios: String read FErroRegrasdeNegocios;

  end;

  { TManifestos }

  TManifestos = class(TOwnedCollection)
  private
    FACBrMDFe: TComponent;
    FConfiguracoes: TConfiguracoesMDFe;

    function GetItem(Index: integer): Manifesto;
    procedure SetItem(Index: integer; const Value: Manifesto);

    procedure VerificarDAMDFE;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    procedure GerarMDFe;
    procedure Assinar;
    procedure Validar;
    function VerificarAssinatura(out Erros: String): Boolean;
    function ValidarRegrasdeNegocios(out Erros: String): Boolean;
    procedure Imprimir;
    procedure ImprimirPDF;

    function Add: Manifesto;
    function Insert(Index: integer): Manifesto;

    property Items[Index: integer]: Manifesto read GetItem write SetItem; default;

    function GetNamePath: String; override;
    // Incluido o Parametro AGerarMDFe que determina se após carregar os dados do MDFe
    // para o componente, será gerado ou não novamente o XML do MDFe.
    function LoadFromFile(CaminhoArquivo: String; AGerarMDFe: Boolean = True): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarMDFe: Boolean = True): Boolean;
    function LoadFromString(AXMLString: String; AGerarMDFe: Boolean = True): Boolean;
    function GravarXML(PathArquivo: String = ''): Boolean;

    property ACBrMDFe: TComponent read FACBrMDFe;
  end;

implementation

uses
  ACBrMDFe, ACBrUtil, pmdfeConversaoMDFe;

{ Manifesto }

constructor Manifesto.Create(Collection2: TCollection);
begin
  inherited Create(Collection2);
  FMDFe := TMDFe.Create;
  FMDFeW := TMDFeW.Create(FMDFe);
  FMDFeR := TMDFeR.Create(FMDFe);

  with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
  begin
    FMDFe.Ide.modelo := '58';
    FMDFe.infMDFe.Versao := VersaoMDFeToDbl(Configuracoes.Geral.VersaoDF);

    FMDFe.Ide.verProc := 'ACBrMDFe';
    FMDFe.Ide.tpAmb := Configuracoes.WebServices.Ambiente;
    FMDFe.Ide.tpEmis := Configuracoes.Geral.FormaEmissao;

//    if Assigned(DAMDFE) then
//      FMDFe.Ide.tpImp := DAMDFE.TipoDAMDFE;
  end;
end;

destructor Manifesto.Destroy;
begin
  FMDFeW.Free;
  FMDFeR.Free;
  FMDFe.Free;
  inherited Destroy;
end;

procedure Manifesto.Imprimir;
begin
  with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
  begin
    if not Assigned(DAMDFE) then
      raise EACBrMDFeException.Create('Componente DAMDFE não associado.')
    else
      DAMDFE.ImprimirDAMDFE(MDFe);
  end;
end;

procedure Manifesto.ImprimirPDF;
begin
  with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
  begin
    if not Assigned(DAMDFE) then
      raise EACBrMDFeException.Create('Componente DAMDFE não associado.')
    else
      DAMDFE.ImprimirDAMDFEPDF(MDFe);
  end;
end;

procedure Manifesto.Assinar;
var
  XMLAss: String;
  ArqXML: String;
  Leitor: TLeitor;
begin
  ArqXML := GerarXML;

  // XML já deve estar em UTF8, para poder ser assinado //
  ArqXML := ConverteXMLtoUTF8(ArqXML);
  FXMLOriginal := ArqXML;

  with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
  begin
    XMLAss := SSL.Assinar(ArqXML, 'MDFe', 'infMDFe');
    FXMLAssinado := XMLAss;

    // Remove header, pois podem existir várias Manifestos no XML //
    //TODO: Verificar se precisa
    //XMLAss := StringReplace(XMLAss, '<' + ENCODING_UTF8_STD + '>', '', [rfReplaceAll]);
    //XMLAss := StringReplace(XMLAss, '<' + XML_V01 + '>', '', [rfReplaceAll]);

    Leitor := TLeitor.Create;
    try
      leitor.Grupo := XMLAss;
      MDFe.signature.URI := Leitor.rAtributo('Reference URI=');
      MDFe.signature.DigestValue := Leitor.rCampo(tcStr, 'DigestValue');
      MDFe.signature.SignatureValue := Leitor.rCampo(tcStr, 'SignatureValue');
      MDFe.signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');
    finally
      Leitor.Free;
    end;

    if Configuracoes.Geral.Salvar then
      Gravar(CalcularNomeArquivoCompleto(), XMLAss);

    if NaoEstaVazio(NomeArq) then
      Gravar(NomeArq, XMLAss);
  end;
end;

procedure Manifesto.Validar;
var
  Erro, AXML: String;
  MDFeEhValida: Boolean;
  ALayout: TLayOutMDFe;
  VersaoStr: String;
begin
  AXML := FXMLAssinado;

  if EstaVazio(AXML) then
  begin
    Assinar;
    AXML := FXMLAssinado;
  end;

  with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
  begin
    ALayout := LayMDFeRetRecepcao;

    VersaoStr := FloatToString( FMDFe.infMDFe.Versao, '.', '0.00');
    MDFeEhValida := SSL.Validar(AXML, GerarNomeArqSchema(ALayout, VersaoStr), Erro);

    if not MDFeEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados do Manifesto: ') +
        IntToStr(MDFe.Ide.nMDF) + sLineBreak + FAlertas ;
      FErroValidacaoCompleto := FErroValidacao + sLineBreak + Erro;

      raise EACBrMDFeException.CreateDef(
        IfThen(Configuracoes.Geral.ExibirErroSchema, ErroValidacaoCompleto,
        ErroValidacao));
    end;
  end;
end;

function Manifesto.VerificarAssinatura: Boolean;
var
  Erro, AXML: String;
  AssEhValida: Boolean;
begin
  AXML := FXMLOriginal;

  if EstaVazio(AXML) then
  begin
    if EstaVazio(FXMLAssinado) then
      Assinar;

    AXML := FXMLAssinado;
  end;

  with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
  begin
    AssEhValida := SSL.VerificarAssinatura(AXML, Erro);

    if not AssEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação da assinatura do Manifesto: ') +
        IntToStr(MDFe.Ide.nMDF) + sLineBreak + Erro;
    end;
  end;

  Result := AssEhValida;
end;

function Manifesto.ValidarRegrasdeNegocios: Boolean;
var
  Erros: String;

  procedure AdicionaErro(const Erro: String);
  begin
    Erros := Erros + Erro + sLineBreak;
  end;

begin
  with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
  begin
    Erros := '';

    if not ValidarConcatChave then  //A03-10
      AdicionaErro(
        '502-Rejeição: Erro na Chave de Acesso - Campo Id não corresponde à concatenação dos campos correspondentes');

  end;

  Result := EstaVazio(Erros);

  if not Result then
  begin
    Erros := ACBrStr('Erro(s) nas Regras de negócios do Manifesto: '+
                     IntToStr(MDFe.Ide.nMDF) + sLineBreak +
                     Erros);
  end;

  FErroRegrasdeNegocios := Erros;
end;

function Manifesto.LerXML(AXML: AnsiString): Boolean;
var
  Ok: Boolean;
begin
  Result := False;
  FMDFeR.Leitor.Arquivo := AXML;
  FMDFeR.LerXml;

  with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
  begin
    Configuracoes.Geral.VersaoDF := DblToVersaoMDFe(OK, FMDFeR.MDFe.infMDFe.Versao);
  end;

  FXML := string(AXML);
  FXMLOriginal := FXML;
  Result := True;
end;

function Manifesto.GravarXML(NomeArquivo: String; PathArquivo: String): Boolean;
begin
  FNomeArq := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);
  GerarXML;
  Result := TACBrMDFe(TManifestos(Collection).ACBrMDFe).Gravar(FNomeArq, FXML);
end;

function Manifesto.GravarStream(AStream: TStream): Boolean;
begin
  Result := False;
  GerarXML;

  AStream.Size := 0;
  AStream.WriteBuffer(FXML[1], Length(FXML));  // Gravando no Buffer da Stream //
  Result := True;
end;

procedure Manifesto.EnviarEmail(sPara, sAssunto: String; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings);
var
  NomeArq : String;
  AnexosEmail:TStrings;
  StreamMDFe : TMemoryStream;
begin
  if not Assigned(TACBrMDFe(TManifestos(Collection).ACBrMDFe).MAIL) then
    raise EACBrMDFeException.Create('Componente ACBrMail não associado');

  AnexosEmail := TStringList.Create;
  StreamMDFe := TMemoryStream.Create;
  try
    AnexosEmail.Clear;
    if Assigned(Anexos) then
      AnexosEmail.Assign(Anexos);

    with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
    begin
      GravarStream(StreamMDFe);

      if (EnviaPDF) then
      begin
        if Assigned(DAMDFE) then
        begin
          DAMDFE.ImprimirDAMDFEPDF(FMDFe);
          NomeArq := PathWithDelim(DAMDFE.PathPDF) + NumID + '-mdfe.pdf';
          AnexosEmail.Add(NomeArq);
        end;
      end;

      EnviarEmail( sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamMDFe,
                   NumID + '-mdfe.xml');
    end;
  finally
    AnexosEmail.Free;
    StreamMDFe.Free;
  end;
end;

function Manifesto.GerarXML: String;
begin
  with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
  begin
    FMDFeW.Gerador.Opcoes.FormatoAlerta := Configuracoes.Geral.FormatoAlerta;
    FMDFeW.Gerador.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
//    FMDFeW.Opcoes.GerarTXTSimultaneamente := False;
  end;

  FMDFeW.GerarXml;
  FXML := FMDFeW.Gerador.ArquivoFormatoXML;
  FXMLAssinado := '';
  FAlertas := FMDFeW.Gerador.ListaDeAlertas.Text;
  Result := FXML;
end;

function Manifesto.CalcularNomeArquivo: String;
var
  xID: String;
begin
  xID := Self.NumID;

  if EstaVazio(xID) then
    raise EACBrMDFeException.Create('ID Inválido. Impossível Salvar XML');

  Result := xID + '-mdfe.xml';
end;

function Manifesto.CalcularPathArquivo: String;
var
  Data: TDateTime;
begin
  with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
  begin
    if Configuracoes.Arquivos.EmissaoPathMDFe then
      Data := FMDFe.Ide.dhEmi
    else
      Data := Now;

    Result := PathWithDelim(Configuracoes.Arquivos.GetPathMDFe(Data, FMDFe.Emit.CNPJ));
  end;
end;

function Manifesto.CalcularNomeArquivoCompleto(NomeArquivo: String;
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

function Manifesto.ValidarConcatChave: Boolean;
var
  wAno, wMes, wDia: word;
begin
  DecodeDate(MDFe.ide.dhEmi, wAno, wMes, wDia);

  Result := not
    ((Copy(MDFe.infMDFe.ID, 4, 2) <> IntToStrZero(MDFe.Ide.cUF, 2)) or
    (Copy(MDFe.infMDFe.ID, 6, 2)  <> Copy(FormatFloat('0000', wAno), 3, 2)) or
    (Copy(MDFe.infMDFe.ID, 8, 2)  <> FormatFloat('00', wMes)) or
    (Copy(MDFe.infMDFe.ID, 10, 14)<> PadLeft(OnlyNumber(MDFe.Emit.CNPJ), 14, '0')) or
    (Copy(MDFe.infMDFe.ID, 24, 2) <> MDFe.Ide.modelo) or
    (Copy(MDFe.infMDFe.ID, 26, 3) <> IntToStrZero(MDFe.Ide.serie, 3)) or
    (Copy(MDFe.infMDFe.ID, 29, 9) <> IntToStrZero(MDFe.Ide.nMDF, 9)) or
    (Copy(MDFe.infMDFe.ID, 38, 1) <> TpEmisToStr(MDFe.Ide.tpEmis)) or
    (Copy(MDFe.infMDFe.ID, 39, 8) <> IntToStrZero(MDFe.Ide.cMDF, 8)));
end;

function Manifesto.GetConfirmado: Boolean;
begin
  Result := TACBrMDFe(TManifestos(Collection).ACBrMDFe).cStatConfirmado(
    FMDFe.procMDFe.cStat);
end;

function Manifesto.GetProcessado: Boolean;
begin
  Result := TACBrMDFe(TManifestos(Collection).ACBrMDFe).cStatProcessado(
    FMDFe.procMDFe.cStat);
end;

function Manifesto.GetMsg: String;
begin
  Result := FMDFe.procMDFe.xMotivo;
end;

function Manifesto.GetNumID: String;
begin
  Result := Trim(OnlyNumber(MDFe.infMDFe.ID));
end;

{ TManifestos }

constructor TManifestos.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  if not (AOwner is TACBrMDFe) then
    raise EACBrMDFeException.Create('AOwner deve ser do tipo TACBrMDFe');

  inherited;

  FACBrMDFe := TACBrMDFe(AOwner);
  FConfiguracoes := TACBrMDFe(FACBrMDFe).Configuracoes;
end;

function TManifestos.Add: Manifesto;
begin
  Result := Manifesto(inherited Add);
end;

procedure TManifestos.Assinar;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].Assinar;
end;

procedure TManifestos.GerarMDFe;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].GerarXML;
end;

function TManifestos.GetItem(Index: integer): Manifesto;
begin
  Result := Manifesto(inherited Items[Index]);
end;

function TManifestos.GetNamePath: String;
begin
  Result := 'Manifesto';
end;

procedure TManifestos.VerificarDAMDFE;
begin
  if not Assigned(TACBrMDFe(FACBrMDFe).DAMDFE) then
    raise EACBrMDFeException.Create('Componente DAMDFE não associado.');
end;

procedure TManifestos.Imprimir;
begin
  VerificarDAMDFE;
  TACBrMDFe(FACBrMDFE).DAMDFE.ImprimirDAMDFE(nil);
end;

procedure TManifestos.ImprimirPDF;
begin
  VerificarDAMDFE;
  TACBrMDFe(FACBrMDFE).DAMDFE.ImprimirDAMDFEPDF(nil);
end;

function TManifestos.Insert(Index: integer): Manifesto;
begin
  Result := Manifesto(inherited Insert(Index));
end;

procedure TManifestos.SetItem(Index: integer; const Value: Manifesto);
begin
  Items[Index].Assign(Value);
end;

procedure TManifestos.Validar;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].Validar;   // Dispara exception em caso de erro
end;

function TManifestos.VerificarAssinatura(out Erros: String): Boolean;
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

function TManifestos.ValidarRegrasdeNegocios(out Erros: String): Boolean;
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

function TManifestos.LoadFromFile(CaminhoArquivo: String;
  AGerarMDFe: Boolean = True): Boolean;
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
    LoadFromString(XML, AGerarMDFe);

    for i := 0 to Self.Count - 1 do
      Self.Items[i].NomeArq := CaminhoArquivo;

    Result := True;
  finally
    ArquivoXML.Free;
  end;
end;

function TManifestos.LoadFromStream(AStream: TStringStream;
  AGerarMDFe: Boolean = True): Boolean;
var
  XMLOriginal: String;
begin
  Result := False;
  XMLOriginal := '';
  AStream.Position := 0;
  SetLength(XMLOriginal, AStream.Size);
  AStream.ReadBuffer(XMLOriginal[1], AStream.Size);

  Result := Self.LoadFromString(XMLOriginal, AGerarMDFe);
end;

function TManifestos.LoadFromString(AXMLString: String;
  AGerarMDFe: Boolean = True): Boolean;
var
  AXML: AnsiString;
  P, N: integer;

  function PosMDFe: integer;
  begin
    Result := pos('</MDFe>', AXMLString);
  end;

begin
  Result := False;
  N := PosMDFe;
  while N > 0 do
  begin
    P := pos('</mdfeProc>', AXMLString);
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

      if AGerarMDFe then // Recalcula o XML
        GerarXML;
    end;

    N := PosMDFe;
  end;
end;

function TManifestos.GravarXML(PathArquivo: String): Boolean;
var
  i: integer;
begin
  Result := True;
  i := 0;
  while Result and (i < Self.Count) do
  begin
    Result := Self.Items[i].GravarXML('', PathArquivo);
    Inc(i);
  end;
end;

end.
