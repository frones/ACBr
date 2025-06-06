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

unit ACBrBPeBilhetes;

interface

uses
  Classes, SysUtils, StrUtils,
  ACBrBPeConfiguracoes,
  ACBrBPeClass,
  ACBrBPeXmlReader, ACBrBPeXmlWriter,
  ACBrBPeIniReader, ACBrBPeIniWriter,
//  pcnConversao,
  ACBrBPeConversao;

type

  { TBilhete }

  TBilhete = class(TCollectionItem)
  private
    FBPe: TBPe;
    // Xml
    FBPeW: TBPeXmlWriter;
    FBPeR: TBPeXmlReader;
    // Ini
    FBPeIniR: TBPeIniReader;
    FBPeIniW: TBPeIniWriter;

    FConfiguracoes: TConfiguracoesBPe;
    FXMLAssinado: String;
    FXMLOriginal: String;
    FAlertas: String;
    FErroValidacao: String;
    FErroValidacaoCompleto: String;
    FErroRegrasdeNegocios: String;
    FNomeArq: String;

    function GetConfirmada: Boolean;
    function GetProcessada: Boolean;
    function GetCancelada: Boolean;

    function GetMsg: String;
    function GetNumID: String;
    function GetXMLAssinado: String;
    procedure SetXML(const AValue: String);
    procedure SetXMLOriginal(const AValue: String);
//    function ValidarConcatChave: Boolean;
    function CalcularNomeArquivo: String;
    function CalcularPathArquivo: String;
    function GetcStat: Integer;
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
    function GerarBPeIni: String;

    function GerarXML: String;
    function GravarXML(const NomeArquivo: String = ''; const PathArquivo: String = ''): Boolean;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings = nil;
      EnviaPDF: Boolean = True; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil; ManterPDFSalvo: Boolean = True;
      sBCC: Tstrings = nil);

    property NomeArq: String read FNomeArq write FNomeArq;
    function CalcularNomeArquivoCompleto(NomeArquivo: String = '';
      PathArquivo: String = ''): String;

    property BPe: TBPe read FBPe;

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

  { TBilhetes }

  TBilhetes = class(TOwnedCollection)
  private
    FACBrBPe: TComponent;
    FConfiguracoes: TConfiguracoesBPe;

    function GetItem(Index: integer): TBilhete;
    procedure SetItem(Index: integer; const Value: TBilhete);

    procedure VerificarDABPE;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    procedure GerarBPe;
    procedure Assinar;
    procedure Validar;
    function VerificarAssinatura(out Erros: String): Boolean;
    function ValidarRegrasdeNegocios(out Erros: String): Boolean;
    procedure Imprimir;
    procedure ImprimirCancelado;
    procedure ImprimirResumido;
    procedure ImprimirPDF;
    procedure ImprimirResumidoPDF;
    procedure ImprimirOffline;

    function Add: TBilhete;
    function Insert(Index: integer): TBilhete;

    property Items[Index: integer]: TBilhete read GetItem write SetItem; default;

    function GetNamePath: String; override;
    // Incluido o Parametro AGerarBPe que determina se após carregar os dados da BPe
    // para o componente, será gerado ou não novamente o XML da BPe.
    function LoadFromFile(const CaminhoArquivo: String; AGerarBPe: Boolean = False): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarBPe: Boolean = False): Boolean;
    function LoadFromString(const AXMLString: String; AGerarBPe: Boolean = False): Boolean;
    function LoadFromIni(const AIniString: String): Boolean;

    function GerarIni: String;
    function GravarXML(const PathNomeArquivo: String = ''): Boolean;

    property ACBrBPe: TComponent read FACBrBPe;
  end;

implementation

uses
  dateutils, IniFiles,
  synautil,
  ACBrXmlBase,
  ACBrBPe,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.XMLHTML,
  ACBrUtil.DateTime,
  ACBrDFeUtil,
  ACBrXmlDocument;
{ Bilhete }

constructor TBilhete.Create(Collection2: TCollection);
begin
  inherited Create(Collection2);

  FBPe := TBPe.Create;
  // Xml
  FBPeW := TBPeXmlWriter.Create(FBPe);
  FBPeR := TBPeXmlReader.Create(FBPe);
  // Ini
  FBPeIniR := TBPeIniReader.Create(FBPe);
  FBPeIniW := TBPeIniWriter.Create(FBPe);

  FConfiguracoes := TACBrBPe(TBilhetes(Collection).ACBrBPe).Configuracoes;

  FBPe.Ide.tpBPe := tbNormal;
  FBPe.Ide.verProc := 'ACBrBPe';

  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    FBPe.Ide.modelo := StrToInt(ModeloBPeToStr(Configuracoes.Geral.ModeloDF));
    FBPe.infBPe.Versao := VersaoBPeToDbl(Configuracoes.Geral.VersaoDF);
    FBPe.Ide.tpAmb := TACBrTipoAmbiente(Configuracoes.WebServices.Ambiente);
    FBPe.Ide.tpEmis := TACBrTipoEmissao(Configuracoes.Geral.FormaEmissao);
  end;
end;

destructor TBilhete.Destroy;
begin
  // Xml
  FBPeW.Free;
  FBPeR.Free;
  // Ini
  FBPeIniR.Free;
  FBPeIniW.Free;

  FBPe.Free;

  inherited Destroy;
end;

procedure TBilhete.Imprimir;
begin
  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    if not Assigned(DABPE) then
      raise EACBrBPeException.Create('Componente DABPE não associado.')
    else
      DABPE.ImprimirDABPE(BPe);
  end;
end;

procedure TBilhete.ImprimirPDF;
begin
  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    if not Assigned(DABPE) then
      raise EACBrBPeException.Create('Componente DABPE não associado.')
    else
      DABPE.ImprimirDABPEPDF(BPe);
  end;
end;

procedure TBilhete.Assinar;
var
  XMLStr: String;
  XMLUTF8: AnsiString;
  Document: TACBrXmlDocument;
  ANode, SignatureNode, ReferenceNode, X509DataNode: TACBrXmlNode;
begin
  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    if not Assigned(SSL.AntesDeAssinar) then
      SSL.ValidarCNPJCertificado( BPe.Emit.CNPJ );
  end;

  // Gera novamente, para processar propriedades que podem ter sido modificadas
  XMLStr := GerarXML;

  // XML já deve estar em UTF8, para poder ser assinado //
  XMLUTF8 := ConverteXMLtoUTF8(XMLStr);

  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    if Configuracoes.Geral.ModeloDF = moBPe then
      FXMLAssinado := SSL.Assinar(String(XMLUTF8), 'BPe', 'infBPe')
    else
      FXMLAssinado := SSL.Assinar(String(XMLUTF8), 'BPeTM', 'infBPe');

    // SSL.Assinar() sempre responde em UTF8...
    FXMLOriginal := FXMLAssinado;

    Document := TACBrXmlDocument.Create;
    try
//      try
        Document.LoadFromXml(FXMLOriginal);
        ANode := Document.Root;

        if ANode <> nil then
        begin
          SignatureNode := ANode.Childrens.FindAnyNs('Signature');
          ReferenceNode := SignatureNode.Childrens.FindAnyNs('SignedInfo')
                                        .Childrens.FindAnyNs('Reference');
          X509DataNode :=  SignatureNode.Childrens.FindAnyNs('KeyInfo')
                                        .Childrens.FindAnyNs('X509Data');

          BPe.signature.URI := ObterConteudoTag(ReferenceNode.Attributes.Items['URI']);
          BPe.signature.DigestValue := ObterConteudoTag(ReferenceNode.Childrens.FindAnyNs('DigestValue'), tcStr);
          BPe.signature.SignatureValue := ObterConteudoTag(SignatureNode.Childrens.FindAnyNs('SignatureValue'), tcStr);
          BPe.signature.X509Certificate := ObterConteudoTag(X509DataNode.Childrens.FindAnyNs('X509Certificate'), tcStr);
        end;
//      except
        //Result := False;
//      end;
    finally
      FreeAndNil(Document);
    end;

    if Configuracoes.Geral.ModeloDF = moBPe then
    begin
      BPe.infBPeSupl.qrCodBPe := GetURLQRCode(BPe);

      BPe.infBPeSupl.boardPassBPe := GetURLConsultaBPe(BPe.Ide.cUF, BPe.Ide.tpAmb);

      GerarXML;
    end;

    if Configuracoes.Arquivos.Salvar and
       (not Configuracoes.Arquivos.SalvarApenasBPeProcessadas) then
    begin
      if NaoEstaVazio(NomeArq) then
        Gravar(NomeArq, FXMLAssinado)
      else
        Gravar(CalcularNomeArquivoCompleto(), FXMLAssinado);
    end;
  end;
end;

procedure TBilhete.Validar;
var
  Erro, AXML, Grupo: String;
  BilheteEhValida: Boolean;
  ALayout: TLayOutBPe;
  VerServ: Real;
begin
  AXML := FXMLAssinado;
  if AXML = '' then
    AXML := XMLOriginal;

  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    VerServ := FBPe.infBPe.Versao;

    if Pos('BPeTM', AXML) > 0 then
    begin
      ALayout := LayBPeRecepcaoTM;
      Grupo := 'BPeTM';
    end
    else
    begin
      ALayout := LayBPeRecepcao;
      Grupo := 'BPe';
    end;

    // Extraindo apenas os dados da BPe (sem bpeProc)
    AXML := ObterDFeXML(AXML, Grupo, ACBRBPE_NAMESPACE);

    if EstaVazio(AXML) then
    begin
      Erro := ACBrStr(Grupo + ' não encontrada no XML');
      BilheteEhValida := False;
    end
    else
      BilheteEhValida := SSL.Validar(AXML, GerarNomeArqSchema(ALayout, VerServ), Erro);

    if not BilheteEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados do Bilhete: ') +
        IntToStr(BPe.Ide.nBP) + sLineBreak + FAlertas ;
      FErroValidacaoCompleto := FErroValidacao + sLineBreak + Erro;

      raise EACBrBPeException.CreateDef(
        IfThen(Configuracoes.Geral.ExibirErroSchema, ErroValidacaoCompleto,
        ErroValidacao));
    end;
  end;
end;

function TBilhete.VerificarAssinatura: Boolean;
var
  Erro, AXML, Grupo: String;
  AssEhValida: Boolean;
begin
  AXML := FXMLAssinado;
  if AXML = '' then
    AXML := XMLOriginal;

  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    if Pos('BPeTM', AXML) > 0 then
      Grupo := 'BPeTM'
    else
      Grupo := 'BPe';

    // Extraindo apenas os dados da BPe (sem bpeProc)
    AXML := ObterDFeXML(AXML, Grupo, ACBRBPE_NAMESPACE);

    if EstaVazio(AXML) then
    begin
      Erro := ACBrStr(Grupo + ' não encontrada no XML');
      AssEhValida := False;
    end
    else
      AssEhValida := SSL.VerificarAssinatura(AXML, Erro, 'infBPe');

    if not AssEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação da assinatura do Bilhete: ') +
        IntToStr(BPe.Ide.nBP) + sLineBreak + Erro;
    end;
  end;

  Result := AssEhValida;
end;

function TBilhete.ValidarRegrasdeNegocios: Boolean;
var
  Erros: String;
  I: Integer;
  Agora: TDateTime;

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
  // Converte o DateTime do Sistema para o TimeZone configurado, para evitar
  // divergência de Fuso Horário.
  Agora := DataHoraTimeZoneModoDeteccao(TACBrBPe(TBilhetes(Collection).ACBrBPe));
  GravaLog('Inicio da Validação');

  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    Erros := '';

    GravaLog('Validar: 701-versão');
    if BPe.infBPe.Versao < 1.00 then
      AdicionaErro('701-Rejeição: Versão inválida');

    GravaLog('Validar: 897-Código do documento: ' + IntToStr(BPe.Ide.nBP));
    if not ValidarCodigoDFe(BPe.Ide.cBP, BPe.Ide.nBP) then
      AdicionaErro('897-Rejeição: Código numérico em formato inválido ');

    for I:=0 to BPe.autXML.Count-1 do
    begin
      GravaLog('Validar: 325-' + IntToStr(I) + '-CPF download');
      if (Length(Trim(OnlyNumber(BPe.autXML[I].CNPJCPF))) <= 11) and
        not ValidarCPF(BPe.autXML[I].CNPJCPF) then
        AdicionaErro('325-Rejeição: CPF autorizado para download inválido');

      GravaLog('Validar: 323-' + IntToStr(I) + '-CNPJ download');
      if (Length(Trim(OnlyNumber(BPe.autXML[I].CNPJCPF))) > 11) and
        not ValidarCNPJ(BPe.autXML[I].CNPJCPF) then
        AdicionaErro('323-Rejeição: CNPJ autorizado para download inválido');
    end;

  end;

  Result := EstaVazio(Erros);

  if not Result then
  begin
    Erros := ACBrStr('Erro(s) nas Regras de negócios do Bilhete: ' +
                     IntToStr(BPe.Ide.nBP) + sLineBreak + Erros);
  end;

  GravaLog('Fim da Validação. Tempo: ' +
           FormatDateTime('hh:nn:ss:zzz', Now - Agora) + sLineBreak +
           'Erros:' + Erros);

  //DEBUG
  //WriteToTXT('c:\temp\Bilhete.txt', Log);

  FErroRegrasdeNegocios := Erros;
end;

function TBilhete.LerXML(const AXML: String): Boolean;
begin
  XMLOriginal := AXML;

  FBPeR.Arquivo := XMLOriginal;
  FBPeR.LerXml;

  Result := True;
end;

function TBilhete.GravarXML(const NomeArquivo: String; const PathArquivo: String): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  FNomeArq := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);

  Result := TACBrBPe(TBilhetes(Collection).ACBrBPe).Gravar(FNomeArq, FXMLOriginal);
end;

function TBilhete.GravarStream(AStream: TStream): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FXMLOriginal));
  Result := True;
end;

procedure TBilhete.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings; sReplyTo: TStrings;
  ManterPDFSalvo: Boolean; sBCC: Tstrings);
var
  NomeArqTemp: string;
  AnexosEmail: TStrings;
  StreamBPe: TMemoryStream;
begin
  if not Assigned(TACBrBPe(TBilhetes(Collection).ACBrBPe).MAIL) then
    raise EACBrBPeException.Create('Componente ACBrMail não associado');

  AnexosEmail := TStringList.Create;
  StreamBPe := TMemoryStream.Create;
  try
    AnexosEmail.Clear;
    if Assigned(Anexos) then
      AnexosEmail.Assign(Anexos);

    with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
    begin
      Self.GravarStream(StreamBPe);

      if (EnviaPDF) then
      begin
        if Assigned(DABPE) then
        begin
          DABPE.ImprimirDABPEPDF(FBPe);
          NomeArqTemp := PathWithDelim(DABPE.PathPDF) + NumID + '-bpe.pdf';
          AnexosEmail.Add(NomeArqTemp);
        end;
      end;

      EnviarEmail( sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamBPe,
                   NumID + '-bpe.xml', sReplyTo, sBCC);
    end;
  finally
    if not ManterPDFSalvo then
      DeleteFile(NomeArqTemp);

    AnexosEmail.Free;
    StreamBPe.Free;
  end;
end;

function TBilhete.GerarXML: String;
var
  IdAnterior : String;
begin
  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    IdAnterior := BPe.infBPe.ID;
    FBPeW.Opcoes.FormatoAlerta  := Configuracoes.Geral.FormatoAlerta;
    FBPeW.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
    FBPeW.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
    FBPeW.Opcoes.IdentarXML     := Configuracoes.Geral.IdentarXML;
    FBPeW.Opcoes.NormatizarMunicipios   := Configuracoes.Arquivos.NormatizarMunicipios;
    FBPeW.Opcoes.PathArquivoMunicipios  := Configuracoes.Arquivos.PathArquivoMunicipios;
    FBPeW.Opcoes.QuebraLinha := Configuracoes.WebServices.QuebradeLinha;

    TimeZoneConf.Assign( Configuracoes.WebServices.TimeZoneConf );

    {
      Ao gerar o XML as tags e atributos tem que ser exatamente os da configuração
    }
    {
    FBPeW.VersaoDF := Configuracoes.Geral.VersaoDF;
    FBPeW.ModeloDF := Configuracoes.Geral.ModeloDF;
    FBPeW.tpAmb := TACBrTipoAmbiente(Configuracoes.WebServices.Ambiente);
    FBPeW.tpEmis := TACBrTipoEmissao(Configuracoes.Geral.FormaEmissao);
    }
    FBPeW.idCSRT := Configuracoes.RespTec.IdCSRT;
    FBPeW.CSRT   := Configuracoes.RespTec.CSRT;
  end;

  FBPeW.GerarXml;
  //DEBUG
  //WriteToTXT('c:\temp\Bilhete.xml', FBPeW.Gerador.ArquivoFormatoXML, False, False);

  XMLOriginal := FBPeW.Document.Xml;

  { XML gerado pode ter nova Chave e ID, então devemos calcular novamente o
    nome do arquivo, mantendo o PATH do arquivo carregado }
  if (NaoEstaVazio(FNomeArq) and (IdAnterior <> FBPe.infBPe.ID)) then
    FNomeArq := CalcularNomeArquivoCompleto('', ExtractFilePath(FNomeArq));

  FAlertas := ACBrStr( FBPeW.ListaDeAlertas.Text );
  Result := FXMLOriginal;
end;

function TBilhete.GerarBPeIni: String;
begin
  Result := FBPeIniW.GravarIni;
end;

function TBilhete.CalcularNomeArquivo: String;
var
  xID: String;
  NomeXML: String;
begin
  xID := Self.NumID;

  if EstaVazio(xID) then
    raise EACBrBPeException.Create('ID Inválido. Impossível Salvar XML');

  NomeXML := '-bpe.xml';

  Result := xID + NomeXML;
end;

function TBilhete.CalcularPathArquivo: String;
var
  Data: TDateTime;
begin
  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    if Configuracoes.Arquivos.EmissaoPathBPe then
      Data := FBPe.Ide.dhEmi
    else
      Data := Now;

    Result := PathWithDelim(Configuracoes.Arquivos.GetPathBPe(Data, FBPe.Emit.CNPJ, FBPe.Emit.IE, Configuracoes.Geral.ModeloDF));
  end;
end;

function TBilhete.CalcularNomeArquivoCompleto(NomeArquivo: String;
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

//function TBilhete.ValidarConcatChave: Boolean;
//var
//  wAno, wMes, wDia: word;
//  chaveBPe : String;
//begin
//  DecodeDate(BPe.ide.dhEmi, wAno, wMes, wDia);
//
//  chaveBPe := 'BPe' + OnlyNumber(BPe.infBPe.ID);
//  {(*}
//  Result := not
//    ((Copy(chaveBPe, 4, 2) <> IntToStrZero(BPe.Ide.cUF, 2)) or
//    (Copy(chaveBPe, 6, 2)  <> Copy(FormatFloat('0000', wAno), 3, 2)) or
//    (Copy(chaveBPe, 8, 2)  <> FormatFloat('00', wMes)) or
//    (Copy(chaveBPe, 10, 14)<> PadLeft(OnlyNumber(BPe.Emit.CNPJ), 14, '0')) or
//    (Copy(chaveBPe, 24, 2) <> IntToStrZero(BPe.Ide.modelo, 2)) or
//    (Copy(chaveBPe, 26, 3) <> IntToStrZero(BPe.Ide.serie, 3)) or
//    (Copy(chaveBPe, 29, 9) <> IntToStrZero(BPe.Ide.nBP, 9)) or
//    (Copy(chaveBPe, 38, 1) <> TpEmisToStr(BPe.Ide.tpEmis)) or
//    (Copy(chaveBPe, 39, 8) <> IntToStrZero(BPe.Ide.cBP, 8)));
//  {*)}
//end;

function TBilhete.GetConfirmada: Boolean;
begin
  Result := TACBrBPe(TBilhetes(Collection).ACBrBPe).CstatConfirmada(
    FBPe.procBPe.cStat);
end;

function TBilhete.GetcStat: Integer;
begin
  Result := FBPe.procBPe.cStat;
end;

function TBilhete.GetProcessada: Boolean;
begin
  Result := TACBrBPe(TBilhetes(Collection).ACBrBPe).CstatProcessado(
    FBPe.procBPe.cStat);
end;

function TBilhete.GetCancelada: Boolean;
begin
  Result := TACBrBPe(TBilhetes(Collection).ACBrBPe).CstatCancelada(
    FBPe.procBPe.cStat);
end;

function TBilhete.GetMsg: String;
begin
  Result := FBPe.procBPe.xMotivo;
end;

function TBilhete.GetNumID: String;
begin
  Result := OnlyNumber(BPe.infBPe.ID);
end;

function TBilhete.GetXMLAssinado: String;
begin
  if EstaVazio(FXMLAssinado) then
    Assinar;

  Result := FXMLAssinado;
end;

procedure TBilhete.SetXML(const AValue: String);
begin
  LerXML(AValue);
end;

procedure TBilhete.SetXMLOriginal(const AValue: String);
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

function TBilhete.LerArqIni(const AIniString: String): Boolean;
begin
  FBPeIniR.VersaoDF := FConfiguracoes.Geral.VersaoDF;
  FBPeIniR.Ambiente := Integer(FConfiguracoes.WebServices.Ambiente);
  FBPeIniR.tpEmis := FConfiguracoes.Geral.FormaEmissaoCodigo;

  FBPeIniR.LerIni(AIniString);

  if FBPe.ide.tpBPe = tbNormal then
    FConfiguracoes.Geral.ModeloDF := moBPe
  else
    FConfiguracoes.Geral.ModeloDF := moBPeTM;

  GerarXML;

  Result := True;
end;

{ TBilhetes }

constructor TBilhetes.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  if not (AOwner is TACBrBPe) then
    raise EACBrBPeException.Create('AOwner deve ser do tipo TACBrBPe');

  inherited Create(AOwner, ItemClass);

  FACBrBPe := TACBrBPe(AOwner);
  FConfiguracoes := TACBrBPe(FACBrBPe).Configuracoes;
end;


function TBilhetes.Add: TBilhete;
begin
  Result := TBilhete(inherited Add);
end;

procedure TBilhetes.Assinar;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].Assinar;
end;

procedure TBilhetes.GerarBPe;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].GerarXML;
end;

function TBilhetes.GerarIni: String;
begin
  Result := '';
  if (Self.Count > 0) then
    Result := Self.Items[0].GerarBPeIni;
end;

function TBilhetes.GetItem(Index: integer): TBilhete;
begin
  Result := TBilhete(inherited Items[Index]);
end;

function TBilhetes.GetNamePath: String;
begin
  Result := 'Bilhete';
end;

procedure TBilhetes.VerificarDABPE;
begin
  if not Assigned(TACBrBPe(FACBrBPe).DABPE) then
    raise EACBrBPeException.Create('Componente DABPE não associado.');
end;

procedure TBilhetes.Imprimir;
begin
  VerificarDABPE;
  TACBrBPe(FACBrBPe).DABPE.ImprimirDABPE(nil);
end;

procedure TBilhetes.ImprimirCancelado;
begin
  VerificarDABPE;
  TACBrBPe(FACBrBPe).DABPE.ImprimirDABPECancelado(nil);
end;

procedure TBilhetes.ImprimirOffline;
var
  ViaConsumidorOld: Boolean;
begin
  VerificarDABPE;

  ViaConsumidorOld := TACBrBPe(FACBrBPe).DABPE.ViaConsumidor;

  try
    TACBrBPe(FACBrBPe).DABPE.ViaConsumidor := True;
    TACBrBPe(FACBrBPe).DABPE.ImprimirDABPE(nil);

    TACBrBPe(FACBrBPe).DABPE.ViaConsumidor := False;
    TACBrBPe(FACBrBPe).DABPE.ImprimirDABPE(nil);
  finally
    TACBrBPe(FACBrBPe).DABPE.ViaConsumidor := ViaConsumidorOld;
  end;
end;

procedure TBilhetes.ImprimirResumido;
begin
  VerificarDABPE;
  TACBrBPe(FACBrBPe).DABPE.ImprimirDABPEResumido(nil);
end;

procedure TBilhetes.ImprimirPDF;
begin
  VerificarDABPE;
  TACBrBPe(FACBrBPe).DABPE.ImprimirDABPEPDF(nil);
end;

procedure TBilhetes.ImprimirResumidoPDF;
begin
  VerificarDABPE;
  TACBrBPe(FACBrBPe).DABPE.ImprimirDABPEResumidoPDF(nil);
end;

function TBilhetes.Insert(Index: integer): TBilhete;
begin
  Result := TBilhete(inherited Insert(Index));
end;

procedure TBilhetes.SetItem(Index: integer; const Value: TBilhete);
begin
  Items[Index].Assign(Value);
end;

procedure TBilhetes.Validar;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].Validar;   // Dispara exception em caso de erro
end;

function TBilhetes.VerificarAssinatura(out Erros: String): Boolean;
var
  i: integer;
begin
  Result := True;
  Erros := '';

  if Self.Count < 1 then
  begin
    Erros := 'Nenhum BPe carregado';
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

function TBilhetes.ValidarRegrasdeNegocios(out Erros: String): Boolean;
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

function TBilhetes.LoadFromFile(const CaminhoArquivo: String;
  AGerarBPe: Boolean): Boolean;
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

  l := Self.Count; // Indice da última Bilhete já existente
  Result := LoadFromString(String(XMLUTF8), AGerarBPe);

  if Result then
  begin
    // Atribui Nome do arquivo a novas Bilhetes inseridas
    for i := l to Self.Count - 1 do
      Self.Items[i].NomeArq := CaminhoArquivo;
  end;
end;

function TBilhetes.LoadFromStream(AStream: TStringStream;
  AGerarBPe: Boolean): Boolean;
var
  AXML: AnsiString;
begin
  AStream.Position := 0;
  AXML := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(AXML), AGerarBPe);
end;

function TBilhetes.LoadFromString(const AXMLString: String;
  AGerarBPe: Boolean): Boolean;
var
  ABPeXML, XMLStr: AnsiString;
  P, N: integer;
  Modelo: TModeloBPe;

  function PosBPe: integer;
  begin
    if Modelo = moBPeTM then
      Result := Pos('</BPeTM>', XMLStr)
    else
      Result := pos('</BPe>', XMLStr);
  end;

begin
  // Verifica se precisa Converter de UTF8 para a String nativa da IDE
  XMLStr := ConverteXMLtoNativeString(AXMLString);

  if Pos('</BPeTM>', XMLStr) > 0 then
    Modelo := moBPeTM
  else
    Modelo := moBPe;

  N := PosBPe;
  while N > 0 do
  begin
    P := pos('</BPeProc>', XMLStr);

    if P <= 0 then
      P := pos('</bpeProc>', XMLStr);

    if P <= 0 then
      P := pos('</procBPe>', XMLStr);  // BPe obtida pelo Portal da Receita

    if P > 0 then
    begin
      ABPeXML := copy(XMLStr, 1, P + 10);
      XMLStr := Trim(copy(XMLStr, P + 10, length(XMLStr)));
    end
    else
    begin
      ABPeXML := copy(XMLStr, 1, N + 7);
      XMLStr := Trim(copy(XMLStr, N + 7, length(XMLStr)));
    end;

    with Self.Add do
    begin
      LerXML(ABPeXML);

      if AGerarBPe then // Recalcula o XML
        GerarXML;
    end;

    N := PosBPe;
  end;

  Result := Self.Count > 0;
end;

function TBilhetes.LoadFromIni(const AIniString: String): Boolean;
begin
  with Self.Add do
    LerArqIni(AIniString);

  Result := Self.Count > 0;
end;

function TBilhetes.GravarXML(const PathNomeArquivo: String): Boolean;
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
