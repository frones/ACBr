{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

unit ACBrNF3eNotasFiscais;

interface

uses
  Classes, SysUtils, StrUtils,
  ACBrXmlBase,
  ACBrNF3eConfiguracoes, ACBrNF3eClass,
  ACBrNF3eIniReader, ACBrNF3eIniWriter,
  ACBrNF3eXmlReader, ACBrNF3eXmlWriter;

type

  { TNotaFiscal }

  TNotaFiscal = class(TCollectionItem)
  private
    FNF3e: TNF3e;
    // Xml
    FNF3eW: TNF3eXmlWriter;
    FNF3eR: TNF3eXmlReader;
    // Ini
    FNF3eIniR: TNF3eIniReader;
    FNF3eIniW: TNF3eIniWriter;
    FConfiguracoes: TConfiguracoesNF3e;
    FXMLAssinado: String;
    FXMLOriginal: String;
    FAlertas: String;
    FErroValidacao: String;
    FErroValidacaoCompleto: String;
    FErroRegrasdeNegocios: String;
    FNomeArq: String;

    function GetConfirmada: Boolean;
    function GetcStat: Integer;
    function GetProcessada: Boolean;
    function GetCancelada: Boolean;

    function GetMsg: String;
    function GetNumID: String;
    function GetXMLAssinado: String;
    procedure SetXML(const AValue: String);
    procedure SetXMLOriginal(const AValue: String);
    function ValidarConcatChave: Boolean;
    function CalcularNomeArquivo: String;
    function CalcularPathArquivo: String;
  public
    constructor Create(Collection2: TCollection); override;
    destructor Destroy; override;
    procedure Imprimir;
    procedure ImprimirPDF;

    procedure Assinar;
    procedure Validar;
    function VerificarAssinatura: Boolean;
    function ValidarRegrasdeNegocios: Boolean;

    function LerXML(const AXML: String): Boolean;
    function LerArqIni(const AIniString: String): Boolean;
    function GerarNF3eIni: String;

    function GerarXML: String;
    function GravarXML(const NomeArquivo: String = ''; const PathArquivo: String = ''): Boolean;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings = nil;
      EnviaPDF: Boolean = True; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil);

    property NomeArq: String read FNomeArq write FNomeArq;
    function CalcularNomeArquivoCompleto(NomeArquivo: String = '';
      PathArquivo: String = ''): String;

    property NF3e: TNF3e read FNF3e;

    // Atribuir a "XML", faz o componente transferir os dados lido para as propriedades internas e "XMLAssinado"
    property XML: String         read FXMLOriginal   write SetXML;
    // Atribuir a "XMLOriginal", reflete em XMLAssinado, se existir a tag de assinatura
    property XMLOriginal: String read FXMLOriginal   write SetXMLOriginal;    // Sempre deve estar em UTF8
    property XMLAssinado: String read GetXMLAssinado write FXMLAssinado;      // Sempre deve estar em UTF8
    property Confirmada: Boolean read GetConfirmada;
    property Processada: Boolean read GetProcessada;
    property Cancelada: Boolean read GetCancelada;
    property cStat: Integer read GetcStat;
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
    FACBrNF3e: TComponent;
    FConfiguracoes: TConfiguracoesNF3e;

    function GetItem(Index: integer): TNotaFiscal;
    procedure SetItem(Index: integer; const Value: TNotaFiscal);

    procedure VerificarDANF3e;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    procedure GerarNF3e;
    procedure Assinar;
    procedure Validar;
    function VerificarAssinatura(out Erros: String): Boolean;
    function ValidarRegrasdeNegocios(out Erros: String): Boolean;
    procedure Imprimir;
    procedure ImprimirCancelado;
    procedure ImprimirResumido;
    procedure ImprimirPDF;
    procedure ImprimirResumidoPDF;
    function Add: TNotaFiscal;
    function Insert(Index: integer): TNotaFiscal;

    property Items[Index: integer]: TNotaFiscal read GetItem write SetItem; default;

    function GetNamePath: String; override;
    // Incluido o Parametro AGerarNF3e que determina se após carregar os dados da NF3e
    // para o componente, será gerado ou não novamente o XML da NF3e.
    function LoadFromFile(const CaminhoArquivo: String; AGerarNF3e: Boolean = False): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarNF3e: Boolean = False): Boolean;
    function LoadFromString(const AXMLString: String; AGerarNF3e: Boolean = False): Boolean;
    function LoadFromIni(const AIniString: String): Boolean;

    function GerarIni: String;
    function GravarXML(const APathNomeArquivo: String = ''): Boolean;

    property ACBrNF3e: TComponent read FACBrNF3e;
  end;

implementation

uses
  dateutils, IniFiles,
  synautil,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.XMLHTML, ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBrDFeUtil,
  ACBrNF3e, ACBrNF3eConversao,
  ACBrXmlDocument;
{ NotaFiscal }

constructor TNotaFiscal.Create(Collection2: TCollection);
begin
  inherited Create(Collection2);

  FNF3e := TNF3e.Create;
  // Xml
  FNF3eW := TNF3eXmlWriter.Create(FNF3e);
  FNF3eR := TNF3eXmlReader.Create(FNF3e);
  // Ini
  FNF3eIniR := TNF3eIniReader.Create(FNF3e);
  FNF3eIniW := TNF3eIniWriter.Create(FNF3e);

  FConfiguracoes := TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e).Configuracoes;

  FNF3e.Ide.verProc := 'ACBrNF3e';

  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    FNF3e.Ide.modelo := 66;
    FNF3e.infNF3e.Versao := VersaoNF3eToDbl(Configuracoes.Geral.VersaoDF);
    FNF3e.Ide.tpAmb := TACBrTipoAmbiente(Configuracoes.WebServices.Ambiente);
    FNF3e.Ide.tpEmis := TACBrTipoEmissao(Configuracoes.Geral.FormaEmissao);
  end;
end;

destructor TNotaFiscal.Destroy;
begin
  // Xml
  FNF3eW.Free;
  FNF3eR.Free;
  // Ini
  FNF3eIniR.Free;
  FNF3eIniW.Free;

  FNF3e.Free;

  inherited Destroy;
end;

procedure TNotaFiscal.Imprimir;
begin
  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    if not Assigned(DANF3e) then
      raise EACBrNF3eException.Create('Componente DANF3e não associado.')
    else
      DANF3e.ImprimirDANF3e(NF3e);
  end;
end;

procedure TNotaFiscal.ImprimirPDF;
begin
  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    if not Assigned(DANF3e) then
      raise EACBrNF3eException.Create('Componente DANF3e não associado.')
    else
      DANF3e.ImprimirDANF3ePDF(NF3e);
  end;
end;

procedure TNotaFiscal.Assinar;
var
  XMLStr: String;
  XMLUTF8: AnsiString;
  Document: TACBrXmlDocument;
  ANode, SignatureNode, ReferenceNode, X509DataNode: TACBrXmlNode;
begin
  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    if not Assigned(SSL.AntesDeAssinar) then
      SSL.ValidarCNPJCertificado( NF3e.Emit.CNPJ );
  end;

  // Gera novamente, para processar propriedades que podem ter sido modificadas
  XMLStr := GerarXML;

  // XML já deve estar em UTF8, para poder ser assinado //
  XMLUTF8 := ConverteXMLtoUTF8(XMLStr);

  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    FXMLAssinado := SSL.Assinar(String(XMLUTF8), 'NF3e', 'infNF3e');
    // SSL.Assinar() sempre responde em UTF8...
    FXMLOriginal := FXMLAssinado;

    Document := TACBrXmlDocument.Create;
    try
      Document.LoadFromXml(FXMLOriginal);
      ANode := Document.Root;

      if ANode <> nil then
      begin
        SignatureNode := ANode.Childrens.FindAnyNs('Signature');
        ReferenceNode := SignatureNode.Childrens.FindAnyNs('SignedInfo')
                                      .Childrens.FindAnyNs('Reference');
        X509DataNode :=  SignatureNode.Childrens.FindAnyNs('KeyInfo')
                                      .Childrens.FindAnyNs('X509Data');

        NF3e.signature.URI := ObterConteudoTag(ReferenceNode.Attributes.Items['URI']);
        NF3e.signature.DigestValue := ObterConteudoTag(ReferenceNode.Childrens.FindAnyNs('DigestValue'), tcStr);
        NF3e.signature.SignatureValue := ObterConteudoTag(SignatureNode.Childrens.FindAnyNs('SignatureValue'), tcStr);
        NF3e.signature.X509Certificate := ObterConteudoTag(X509DataNode.Childrens.FindAnyNs('X509Certificate'), tcStr);
      end;
    finally
      FreeAndNil(Document);
    end;

//    with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
//    begin
      NF3e.infNF3eSupl.qrCodNF3e := GetURLQRCode(NF3e);

      GerarXML;
//    end;

    if Configuracoes.Arquivos.Salvar then
    begin
      if NaoEstaVazio(NomeArq) then
        Gravar(NomeArq, FXMLAssinado)
      else
      begin
        NomeArq := CalcularNomeArquivoCompleto();
        Gravar(NomeArq, FXMLAssinado);
      end;
    end;
  end;
end;

procedure TNotaFiscal.Validar;
var
  Erro, AXML: String;
  NotaEhValida{, ok}: Boolean;
  ALayout: TLayOut;
  VerServ: Real;
//  cUF: Integer;
begin
  AXML := FXMLAssinado;
  if AXML = '' then
    AXML := XMLOriginal;

  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    VerServ := FNF3e.infNF3e.Versao;
//    cUF     := FNF3e.Ide.cUF;

//    if EhAutorizacao( DblToVersaoNF3e(ok, VerServ), Modelo, cUF) then
//      ALayout := LayNF3eAutorizacao
//    else
      ALayout := LayNF3eRecepcao;

    // Extraindo apenas os dados da NF3e (sem nf3eProc)
    AXML := ObterDFeXML(AXML, 'NF3e', ACBRNF3e_NAMESPACE);

    if EstaVazio(AXML) then
    begin
      Erro := ACBrStr('NF3e não encontrada no XML');
      NotaEhValida := False;
    end
    else
      NotaEhValida := SSL.Validar(AXML, GerarNomeArqSchema(ALayout, VerServ), Erro);

    if not NotaEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados da nota: ') +
        IntToStr(NF3e.Ide.nNF) + sLineBreak + FAlertas ;
      FErroValidacaoCompleto := FErroValidacao + sLineBreak + Erro;

      raise EACBrNF3eException.CreateDef(
        IfThen(Configuracoes.Geral.ExibirErroSchema, ErroValidacaoCompleto,
        ErroValidacao));
    end;
  end;
end;

function TNotaFiscal.VerificarAssinatura: Boolean;
var
  Erro, AXML: String;
  AssEhValida: Boolean;
begin
  AXML := FXMLAssinado;
  if AXML = '' then
    AXML := XMLOriginal;

  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    // Extraindo apenas os dados da NF3e (sem nf3eProc)
    AXML := ObterDFeXML(AXML, 'NF3e', ACBRNF3e_NAMESPACE);

    if EstaVazio(AXML) then
    begin
      Erro := ACBrStr('NF3e não encontrada no XML');
      AssEhValida := False;
    end
    else
      AssEhValida := SSL.VerificarAssinatura(AXML, Erro, 'infNF3e');

    if not AssEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação da assinatura da nota: ') +
        IntToStr(NF3e.Ide.nNF) + sLineBreak + Erro;
    end;
  end;

  Result := AssEhValida;
end;

function TNotaFiscal.ValidarRegrasdeNegocios: Boolean;
const
  SEM_GTIN = 'SEM GTIN';
var
  Erros: String;
  Inicio, Agora: TDateTime;

  procedure GravaLog(AString: String);
  begin
    //DEBUG
    //Log := Log + FormatDateTime('hh:nn:ss:zzz',Now) + ' - ' + AString + sLineBreak;
  end;

  procedure AdicionaErro(const Erro: String);
  begin
    Erros := Erros + Erro + sLineBreak;
  end;

begin
  Inicio := Now;
  Agora := IncMinute(Now, 5);  //Aceita uma tolerância de até 5 minutos, devido ao sincronismo de horário do servidor da Empresa e o servidor da SEFAZ.
  GravaLog('Inicio da Validação');

  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    Erros := '';
    {
      Incluir as regras aqui
    }
  end;

  Result := EstaVazio(Erros);

  if not Result then
  begin
    Erros := ACBrStr('Erro(s) nas Regras de negócios da nota: '+
                     IntToStr(NF3e.Ide.nNF) + sLineBreak +
                     Erros);
  end;

  GravaLog('Fim da Validação. Tempo: '+FormatDateTime('hh:nn:ss:zzz', Now - Inicio)+sLineBreak+
           'Erros:' + Erros);

  //DEBUG
  //WriteToTXT('c:\temp\Notafiscal.txt', Log);

  FErroRegrasdeNegocios := Erros;
end;

function TNotaFiscal.LerXML(const AXML: String): Boolean;
begin
  XMLOriginal := AXML;

  FNF3eR.Arquivo := XMLOriginal;
  FNF3eR.LerXml;
  Result := True;
end;

function TNotaFiscal.LerArqIni(const AIniString: String): Boolean;
begin
  FNF3eIniR.VersaoDF := FConfiguracoes.Geral.VersaoDF;
  FNF3eIniR.Ambiente := Integer(FConfiguracoes.WebServices.Ambiente);
  FNF3eIniR.tpEmis := FConfiguracoes.Geral.FormaEmissaoCodigo;

  FNF3eIniR.LerIni(AIniString);

  GerarXML;

  Result := True;
end;

function TNotaFiscal.GerarNF3eIni: String;
begin
  Result := FNF3eIniW.GravarIni;
end;

function TNotaFiscal.GravarXML(const NomeArquivo: String; const PathArquivo: String): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  FNomeArq := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);

  Result := TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e).Gravar(FNomeArq, FXMLOriginal);
end;

function TNotaFiscal.GravarStream(AStream: TStream): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FXMLOriginal));

  Result := True;
end;

procedure TNotaFiscal.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings; sReplyTo: TStrings);
var
  NomeArq_temp : String;
  AnexosEmail:TStrings;
  StreamNF3e : TMemoryStream;
begin
  if not Assigned(TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e).MAIL) then
    raise EACBrNF3eException.Create('Componente ACBrMail não associado');

  AnexosEmail := TStringList.Create;
  StreamNF3e := TMemoryStream.Create;
  try
    AnexosEmail.Clear;

    if Assigned(Anexos) then
      AnexosEmail.Assign(Anexos);

    with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
    begin
      Self.GravarStream(StreamNF3e);

      if (EnviaPDF) then
      begin
        if Assigned(DANF3e) then
        begin
          DANF3e.ImprimirDANF3ePDF(FNF3e);
          NomeArq_temp := PathWithDelim(DANF3e.PathPDF) + NumID + '-NF3e.pdf';
          AnexosEmail.Add(NomeArq_temp);
        end;
      end;

      EnviarEmail( sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamNF3e,
                   NumID +'-NF3e.xml', sReplyTo);
    end;
  finally
    AnexosEmail.Free;
    StreamNF3e.Free;
  end;
end;

function TNotaFiscal.GerarXML: String;
var
  IdAnterior : String;
begin
  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    IdAnterior := NF3e.infNF3e.ID;

    FNF3eW.Opcoes.FormatoAlerta  := Configuracoes.Geral.FormatoAlerta;
    FNF3eW.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
    FNF3eW.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
    FNF3eW.Opcoes.IdentarXML := Configuracoes.Geral.IdentarXML;
    FNF3eW.Opcoes.NormatizarMunicipios  := Configuracoes.Arquivos.NormatizarMunicipios;
    FNF3eW.Opcoes.PathArquivoMunicipios := Configuracoes.Arquivos.PathArquivoMunicipios;
    FNF3eW.Opcoes.QuebraLinha := Configuracoes.WebServices.QuebradeLinha;

    TimeZoneConf.Assign( Configuracoes.WebServices.TimeZoneConf );

    {
      Ao gerar o XML as tags e atributos tem que ser exatamente os da configuração
    }
    {
    FNF3eW.VersaoDF := Configuracoes.Geral.VersaoDF;
    FNF3eW.ModeloDF := 62;
    FNF3eW.tpAmb := TACBrTipoAmbiente(Configuracoes.WebServices.Ambiente);
    FNF3eW.tpEmis := TACBrTipoEmissao(Configuracoes.Geral.FormaEmissao);
    }
    FNF3eW.idCSRT := Configuracoes.RespTec.IdCSRT;
    FNF3eW.CSRT   := Configuracoes.RespTec.CSRT;
  end;

  FNF3eW.GerarXml;
  //DEBUG
  //WriteToTXT('c:\temp\Notafiscal.xml', FNF3eW.Document.Xml, False, False);

  XMLOriginal := FNF3eW.Document.Xml;

  { XML gerado pode ter nova Chave e ID, então devemos calcular novamente o
    nome do arquivo, mantendo o PATH do arquivo carregado }
  if (NaoEstaVazio(FNomeArq) and (IdAnterior <> FNF3e.infNF3e.ID)) then
    FNomeArq := CalcularNomeArquivoCompleto('', ExtractFilePath(FNomeArq));

  FAlertas := ACBrStr( FNF3eW.ListaDeAlertas.Text );

  Result := FXMLOriginal;
end;

function TNotaFiscal.CalcularNomeArquivo: String;
var
  xID: String;
  NomeXML: String;
begin
  xID := Self.NumID;

  if EstaVazio(xID) then
    raise EACBrNF3eException.Create('ID Inválido. Impossível Salvar XML');

  NomeXML := '-NF3e.xml';

  Result := xID + NomeXML;
end;

function TNotaFiscal.CalcularPathArquivo: String;
var
  Data: TDateTime;
begin
  with TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e) do
  begin
    if Configuracoes.Arquivos.EmissaoPathNF3e then
      Data := FNF3e.Ide.dhEmi
    else
      Data := Now;

    Result := PathWithDelim(Configuracoes.Arquivos.GetPathNF3e(Data, FNF3e.Emit.CNPJ));
  end;
end;

function TNotaFiscal.CalcularNomeArquivoCompleto(NomeArquivo: String;
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

function TNotaFiscal.ValidarConcatChave: Boolean;
var
  wAno, wMes, wDia: word;
  chaveNF3e : String;
begin
  DecodeDate(NF3e.ide.dhEmi, wAno, wMes, wDia);

  chaveNF3e := OnlyNumber(NF3e.infNF3e.ID);
  {(*}
  Result := not
    ((Copy(chaveNF3e, 1, 2) <> IntToStrZero(NF3e.Ide.cUF, 2)) or
    (Copy(chaveNF3e, 3, 2)  <> Copy(FormatFloat('0000', wAno), 3, 2)) or
    (Copy(chaveNF3e, 5, 2)  <> FormatFloat('00', wMes)) or
    (Copy(chaveNF3e, 7, 14)<> PadLeft(OnlyNumber(NF3e.Emit.CNPJ), 14, '0')) or
    (Copy(chaveNF3e, 21, 2) <> IntToStrZero(NF3e.Ide.modelo, 2)) or
    (Copy(chaveNF3e, 23, 3) <> IntToStrZero(NF3e.Ide.serie, 3)) or
    (Copy(chaveNF3e, 26, 9) <> IntToStrZero(NF3e.Ide.nNF, 9)) or
    (Copy(chaveNF3e, 35, 1) <> TipoEmissaoToStr(NF3e.Ide.tpEmis)) or
    (Copy(chaveNF3e, 36, 1) <> SiteAutorizadorToStr(NF3e.Ide.nSiteAutoriz)) or
    (Copy(chaveNF3e, 37, 7) <> IntToStrZero(NF3e.Ide.cNF, 7)));
  {*)}
end;

function TNotaFiscal.GetConfirmada: Boolean;
begin
  Result := TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e).CstatConfirmada(
    FNF3e.procNF3e.cStat);
end;

function TNotaFiscal.GetcStat: Integer;
begin
  Result := FNF3e.procNF3e.cStat;
end;

function TNotaFiscal.GetProcessada: Boolean;
begin
  Result := TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e).CstatProcessado(
    FNF3e.procNF3e.cStat);
end;

function TNotaFiscal.GetCancelada: Boolean;
begin
  Result := TACBrNF3e(TNotasFiscais(Collection).ACBrNF3e).CstatCancelada(
    FNF3e.procNF3e.cStat);
end;

function TNotaFiscal.GetMsg: String;
begin
  Result := FNF3e.procNF3e.xMotivo;
end;

function TNotaFiscal.GetNumID: String;
begin
  Result := OnlyNumber(NF3e.infNF3e.ID);
end;

function TNotaFiscal.GetXMLAssinado: String;
begin
  if EstaVazio(FXMLAssinado) then
    Assinar;

  Result := FXMLAssinado;
end;

procedure TNotaFiscal.SetXML(const AValue: String);
begin
  LerXML(AValue);
end;

procedure TNotaFiscal.SetXMLOriginal(const AValue: String);
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

{ TNotasFiscais }

constructor TNotasFiscais.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  if not (AOwner is TACBrNF3e) then
    raise EACBrNF3eException.Create('AOwner deve ser do tipo TACBrNF3e');

  inherited Create(AOwner, ItemClass);

  FACBrNF3e := TACBrNF3e(AOwner);
  FConfiguracoes := TACBrNF3e(FACBrNF3e).Configuracoes;
end;

function TNotasFiscais.Add: TNotaFiscal;
begin
  Result := TNotaFiscal(inherited Add);
end;

procedure TNotasFiscais.Assinar;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].Assinar;
end;

procedure TNotasFiscais.GerarNF3e;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].GerarXML;
end;

function TNotasFiscais.GetItem(Index: integer): TNotaFiscal;
begin
  Result := TNotaFiscal(inherited Items[Index]);
end;

function TNotasFiscais.GetNamePath: String;
begin
  Result := 'NotaFiscal';
end;

procedure TNotasFiscais.VerificarDANF3e;
begin
  if not Assigned(TACBrNF3e(FACBrNF3e).DANF3e) then
    raise EACBrNF3eException.Create('Componente DANF3e não associado.');
end;

procedure TNotasFiscais.Imprimir;
begin
  VerificarDANF3e;
  TACBrNF3e(FACBrNF3e).DANF3e.ImprimirDANF3e(nil);
end;

procedure TNotasFiscais.ImprimirCancelado;
begin
  VerificarDANF3e;
  TACBrNF3e(FACBrNF3e).DANF3e.ImprimirDANF3eCancelado(nil);
end;

procedure TNotasFiscais.ImprimirResumido;
begin
  VerificarDANF3e;
  TACBrNF3e(FACBrNF3e).DANF3e.ImprimirDANF3eResumido(nil);
end;

procedure TNotasFiscais.ImprimirPDF;
begin
  VerificarDANF3e;
  TACBrNF3e(FACBrNF3e).DANF3e.ImprimirDANF3ePDF(nil);
end;

procedure TNotasFiscais.ImprimirResumidoPDF;
begin
  VerificarDANF3e;
  TACBrNF3e(FACBrNF3e).DANF3e.ImprimirDANF3eResumidoPDF(nil);
end;

function TNotasFiscais.Insert(Index: integer): TNotaFiscal;
begin
  Result := TNotaFiscal(inherited Insert(Index));
end;

procedure TNotasFiscais.SetItem(Index: integer; const Value: TNotaFiscal);
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

  if Self.Count < 1 then
  begin
    Erros := 'Nenhuma NF3e carregada';
    Result := False;
    Exit;
  end;

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

function TNotasFiscais.LoadFromFile(const CaminhoArquivo: String;
  AGerarNF3e: Boolean): Boolean;
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
  Result := LoadFromString(String(XMLUTF8), AGerarNF3e);

  if Result then
  begin
    // Atribui Nome do arquivo a novas notas inseridas //
    for i := l to Self.Count - 1 do
      Self.Items[i].NomeArq := CaminhoArquivo;
  end;
end;

function TNotasFiscais.LoadFromStream(AStream: TStringStream;
  AGerarNF3e: Boolean): Boolean;
var
  AXML: AnsiString;
begin
  AStream.Position := 0;
  AXML := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(AXML), AGerarNF3e);
end;

function TNotasFiscais.LoadFromString(const AXMLString: String;
  AGerarNF3e: Boolean): Boolean;
var
  ANF3eXML, XMLStr: AnsiString;
  P, N: integer;

  function PosNF3e: integer;
  begin
    Result := pos('</NF3e>', XMLStr);
  end;

begin
  // Verifica se precisa Converter de UTF8 para a String nativa da IDE //
  XMLStr := ConverteXMLtoNativeString(AXMLString);

  N := PosNF3e;
  while N > 0 do
  begin
    P := pos('</nf3eProc>', XMLStr);

    if P <= 0 then
      P := pos('</procNF3e>', XMLStr);  // NF3e obtida pelo Portal da Receita

    if P > 0 then
    begin
      ANF3eXML := copy(XMLStr, 1, P + 10);
      XMLStr := Trim(copy(XMLStr, P + 10, length(XMLStr)));
    end
    else
    begin
      ANF3eXML := copy(XMLStr, 1, N + 6);
      XMLStr := Trim(copy(XMLStr, N + 6, length(XMLStr)));
    end;

    with Self.Add do
    begin
      LerXML(ANF3eXML);

      if AGerarNF3e then // Recalcula o XML
        GerarXML;
    end;

    N := PosNF3e;
  end;

  Result := Self.Count > 0;
end;

function TNotasFiscais.LoadFromIni(const AIniString: String): Boolean;
begin
  with Self.Add do
    LerArqIni(AIniString);

  Result := Self.Count > 0;
end;

function TNotasFiscais.GerarIni: String;
begin
  Result := '';

  if (Self.Count > 0) then
    Result := Self.Items[0].GerarNF3eIni;
end;

function TNotasFiscais.GravarXML(const APathNomeArquivo: String): Boolean;
var
  i: integer;
  NomeArq, PathArq : String;
begin
  Result := True;
  i := 0;

  while Result and (i < Self.Count) do
  begin
    PathArq := ExtractFilePath(APathNomeArquivo);
    NomeArq := ExtractFileName(APathNomeArquivo);
    Result := Self.Items[i].GravarXML(NomeArq, PathArq);
    Inc(i);
  end;
end;

end.
