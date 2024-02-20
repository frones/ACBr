{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrBPeBilhetes;

interface

uses
  Classes, SysUtils, StrUtils,
  ACBrBPeConfiguracoes,
  ACBrBPeClass, ACBrBPeXmlReader, ACBrBPeXmlWriter,
//  pcnConversao,
  ACBrBPeConversao;

type

  { Bilhete }

  Bilhete = class(TCollectionItem)
  private
    FBPe: TBPe;
    FBPeW: TBPeXmlWriter;
    FBPeR: TBPeXmlReader;

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

    function GerarTXT: String;
    function GravarTXT(const NomeArquivo: String = ''; const PathArquivo: String = ''): Boolean;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings = nil;
      EnviaPDF: Boolean = True; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil);

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

    function GetItem(Index: integer): Bilhete;
    procedure SetItem(Index: integer; const Value: Bilhete);

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

    function Add: Bilhete;
    function Insert(Index: integer): Bilhete;

    property Items[Index: integer]: Bilhete read GetItem write SetItem; default;

    function GetNamePath: String; override;
    // Incluido o Parametro AGerarBPe que determina se após carregar os dados da BPe
    // para o componente, será gerado ou não novamente o XML da BPe.
    function LoadFromFile(const CaminhoArquivo: String; AGerarBPe: Boolean = False): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarBPe: Boolean = False): Boolean;
    function LoadFromString(const AXMLString: String; AGerarBPe: Boolean = False): Boolean;
    function LoadFromIni(const AIniString: String): Boolean;

    function GerarIni: String;
    function GravarXML(const PathNomeArquivo: String = ''): Boolean;
    function GravarTXT(PathNomeArquivo: String = ''): Boolean;

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

constructor Bilhete.Create(Collection2: TCollection);
begin
  inherited Create(Collection2);

  FBPe := TBPe.Create;
  FBPeW := TBPeXmlWriter.Create(FBPe);
  FBPeR := TBPeXmlReader.Create(FBPe);
  FConfiguracoes := TACBrBPe(TBilhetes(Collection).ACBrBPe).Configuracoes;

  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
//    FBPe.Ide.modelo := 63;
    FBPe.Ide.modelo := StrToInt(ModeloBPeToStr(Configuracoes.Geral.ModeloDF));
    FBPe.infBPe.Versao := VersaoBPeToDbl(Configuracoes.Geral.VersaoDF);
    FBPe.Ide.tpBPe := tbNormal;
    FBPe.Ide.verProc := 'ACBrBPe';
    FBPe.Ide.tpAmb := TACBrTipoAmbiente(Configuracoes.WebServices.Ambiente);
    FBPe.Ide.tpEmis  := TACBrTipoEmissao(Configuracoes.Geral.FormaEmissao);
  end;
end;

destructor Bilhete.Destroy;
begin
  FBPeW.Free;
  FBPeR.Free;
  FBPe.Free;

  inherited Destroy;
end;

procedure Bilhete.Imprimir;
begin
  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    if not Assigned(DABPE) then
      raise EACBrBPeException.Create('Componente DABPE não associado.')
    else
      DABPE.ImprimirDABPE(BPe);
  end;
end;

procedure Bilhete.ImprimirPDF;
begin
  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    if not Assigned(DABPE) then
      raise EACBrBPeException.Create('Componente DABPE não associado.')
    else
      DABPE.ImprimirDABPEPDF(BPe);
  end;
end;

procedure Bilhete.Assinar;
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

          BPe.signature.URI := ObterConteudoTag(ReferenceNode.Attributes.Items['URI']);
          BPe.signature.DigestValue := ObterConteudoTag(ReferenceNode.Childrens.FindAnyNs('DigestValue'), tcStr);
          BPe.signature.SignatureValue := ObterConteudoTag(SignatureNode.Childrens.FindAnyNs('SignatureValue'), tcStr);
          BPe.signature.X509Certificate := ObterConteudoTag(X509DataNode.Childrens.FindAnyNs('X509Certificate'), tcStr);
        end;
      except
        //Result := False;
      end;
    finally
      FreeAndNil(Document);
    end;

    if Configuracoes.Geral.ModeloDF = moBPe then
    begin
      with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
      begin
        BPe.infBPeSupl.qrCodBPe := GetURLQRCode(BPe.Ide.cUF,
                                                BPe.Ide.tpAmb,
                                                BPe.infBPe.ID);

  //      BPe.infBPeSupl.boardPassBPe := GetURLConsultaNFCe(BPe.Ide.cUF, BPe.Ide.tpAmb);

        GerarXML;
      end;
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

procedure Bilhete.Validar;
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

function Bilhete.VerificarAssinatura: Boolean;
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

function Bilhete.ValidarRegrasdeNegocios: Boolean;
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
  Agora := DataHoraTimeZoneModoDeteccao( TACBrBPe(TBilhetes(Collection).ACBrBPe) );   //Converte o DateTime do Sistema para o TimeZone configurado, para evitar divergência de Fuso Horário.
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

function Bilhete.LerXML(const AXML: String): Boolean;
begin
  XMLOriginal := AXML;

  FBPeR.Arquivo := XMLOriginal;
  FBPeR.LerXml;

  Result := True;
end;

function Bilhete.GravarXML(const NomeArquivo: String; const PathArquivo: String): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  FNomeArq := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);

  Result := TACBrBPe(TBilhetes(Collection).ACBrBPe).Gravar(FNomeArq, FXMLOriginal);
end;

function Bilhete.GravarTXT(const NomeArquivo: String; const PathArquivo: String): Boolean;
var
  ATXT: String;
begin
  FNomeArq := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);
  ATXT := GerarTXT;
  Result := TACBrBPe(TBilhetes(Collection).ACBrBPe).Gravar(
    ChangeFileExt(FNomeArq, '.txt'), ATXT);
end;

function Bilhete.GravarStream(AStream: TStream): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FXMLOriginal));
  Result := True;
end;

procedure Bilhete.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings; sReplyTo: TStrings);
var
  NomeArq_Temp : String;
  AnexosEmail:TStrings;
  StreamBPe : TMemoryStream;
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
          NomeArq_Temp := PathWithDelim(DABPE.PathPDF) + NumID + '-bpe.pdf';
          AnexosEmail.Add(NomeArq_Temp);
        end;
      end;

      EnviarEmail( sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamBPe,
                   NumID + '-bpe.xml', sReplyTo);
    end;
  finally
    AnexosEmail.Free;
    StreamBPe.Free;
  end;
end;

function Bilhete.GerarXML: String;
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

    TimeZoneConf.Assign( Configuracoes.WebServices.TimeZoneConf );

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

function Bilhete.GerarBPeIni: String;
var
  I: integer;
  sSecao: string;
  INIRec: TMemIniFile;
  IniBPe: TStringList;
begin
  Result := '';

  if not ValidarChave(BPe.infBPe.ID) then
    raise EACBrBPeException.Create('BPe Inconsistente para gerar INI. Chave Inválida.');

  INIRec := TMemIniFile.Create('');
  try
    with FBPe do
    begin
      INIRec.WriteInteger('ide', 'cUF', ide.cUF);
      INIRec.WriteInteger('ide', 'mod', Ide.modelo);
      INIRec.WriteInteger('ide', 'serie', Ide.serie);
      INIRec.WriteInteger('ide', 'nBP', Ide.nBP);
      INIRec.WriteInteger('ide', 'cBP', Ide.cBP);
      INIRec.WriteString('ide', 'modal', ModalBPeToStr(Ide.modal));
      INIRec.WriteString('ide', 'dhEmi', DateTimeToStr(Ide.dhEmi));
      INIRec.WriteString('ide', 'tpEmis', TipoEmissaoToStr(Ide.tpEmis));
      INIRec.WriteString('ide', 'verProc', Ide.verProc);
      INIRec.WriteString('ide', 'tpBPe', tpBPeToStr(Ide.tpBPe));
      INIRec.WriteString('ide','indPres', PresencaCompradorToStr(Ide.indPres));
      INIRec.WriteString('ide', 'UFIni', Ide.UFIni);
      INIRec.WriteInteger('ide', 'cMunIni', Ide.cMunIni);
      INIRec.WriteString('ide', 'UFFim', Ide.UFFim);
      INIRec.WriteInteger('ide', 'cMunFim', Ide.cMunFim);
      INIRec.WriteString('ide', 'dhCont', DateTimeToStr(Ide.dhCont));
      INIRec.WriteString('ide', 'xJust', Ide.xJust);

      // Emitente

      INIRec.WriteString('emit', 'CNPJ', Emit.CNPJ);
      INIRec.WriteString('emit', 'IE', Emit.IE);
      INIRec.WriteString('emit', 'IEST', Emit.IEST);
      INIRec.WriteString('emit', 'xNome', Emit.xNome);
      INIRec.WriteString('emit', 'xFant', Emit.xFant);
      INIRec.WriteString('emit', 'TAR', Emit.TAR);
      INIRec.WriteString('emit', 'CRT', CRTToStr(Emit.CRT));

      INIRec.WriteString('emit', 'xLgr', Emit.enderEmit.xLgr);
      INIRec.WriteString('emit', 'nro', Emit.enderEmit.nro);
      INIRec.WriteString('emit', 'xCpl', Emit.enderEmit.xCpl);
      INIRec.WriteString('emit', 'xBairro', Emit.enderEmit.xBairro);
      INIRec.WriteInteger('emit', 'cMun', Emit.enderEmit.cMun);
      INIRec.WriteString('emit', 'xMun', Emit.enderEmit.xMun);
      INIRec.WriteInteger('emit', 'CEP', Emit.enderEmit.CEP);
      INIRec.WriteString('emit', 'UF', Emit.enderEmit.UF);
      INIRec.WriteString('emit', 'fone', Emit.enderEmit.fone);
      INIRec.WriteString('emit', 'email', Emit.enderEmit.Email);

      // Comprador

      INIRec.WriteString('comp', 'xNome', comp.xNome);
      INIRec.WriteString('comp', 'CNPJCPF', comp.CNPJCPF);
      INIRec.WriteString('comp', 'idEstrangeiro', comp.idEstrangeiro);
      INIRec.WriteString('comp', 'IE', comp.IE);

      INIRec.WriteString('comp', 'xLgr', comp.EnderComp.xLgr);
      INIRec.WriteString('comp', 'nro', comp.EnderComp.nro);
      INIRec.WriteString('comp', 'xCpl', comp.EnderComp.xCpl);
      INIRec.WriteString('comp', 'xBairro', comp.EnderComp.xBairro);
      INIRec.WriteInteger('comp', 'cMun', comp.EnderComp.cMun);
      INIRec.WriteString('comp', 'xMun', comp.EnderComp.xMun);
      INIRec.WriteInteger('comp', 'CEP', comp.EnderComp.CEP);
      INIRec.WriteString('comp', 'UF', comp.EnderComp.UF);
      INIRec.WriteInteger('comp', 'cPais', comp.EnderComp.cPais);
      INIRec.WriteString('comp', 'xPais', comp.EnderComp.xPais);
      INIRec.WriteString('comp', 'fone', comp.EnderComp.fone);
      INIRec.WriteString('comp', 'email', Comp.EnderComp.email);

      // Agencia

      INIRec.WriteString('agencia', 'xNome', agencia.xNome);
      INIRec.WriteString('agencia', 'CNPJ', agencia.CNPJ);

      INIRec.WriteString('agencia', 'xLgr', agencia.EnderAgencia.xLgr);
      INIRec.WriteString('agencia', 'nro', agencia.EnderAgencia.nro);
      INIRec.WriteString('agencia', 'xCpl', agencia.EnderAgencia.xCpl);
      INIRec.WriteString('agencia', 'xBairro', agencia.EnderAgencia.xBairro);
      INIRec.WriteInteger('agencia', 'cMun', agencia.EnderAgencia.cMun);
      INIRec.WriteString('agencia', 'xMun', agencia.EnderAgencia.xMun);
      INIRec.WriteInteger('agencia', 'CEP', agencia.EnderAgencia.CEP);
      INIRec.WriteString('agencia', 'UF', agencia.EnderAgencia.UF);
      INIRec.WriteInteger('agencia', 'cPais', agencia.EnderAgencia.cPais);
      INIRec.WriteString('agencia', 'xPais', agencia.EnderAgencia.xPais);
      INIRec.WriteString('agencia', 'fone', agencia.EnderAgencia.fone);
      INIRec.WriteString('agencia', 'email', agencia.EnderAgencia.email);

      // Informações dos BP-e de Substituição

      INIRec.WriteString('infBPeSub', 'chBPe', infBPeSub.chBPe);
      INIRec.WriteString('infBPeSub', 'tpSub', tpSubstituicaoToStr(infBpeSub.tpSub));

      // Informações da Passagem

      INIRec.WriteString('infPassagem', 'cLocOrig', infPassagem.cLocOrig);
      INIRec.WriteString('infPassagem', 'xLocOrig', infPassagem.xLocOrig);
      INIRec.WriteString('infPassagem', 'cLocDest', infPassagem.cLocDest);
      INIRec.WriteString('infPassagem', 'xLocDest', infPassagem.xLocDest);
      INIRec.WriteString('infPassagem', 'dhEmb', DateTimeToStr(infPassagem.dhEmb));
      INIRec.WriteString('infPassagem', 'dhValidade', DateTimeToStr(infPassagem.dhValidade));

      // Informações do Passageiro

      if infPassagem.infPassageiro.xNome <> '' then
      begin
        INIRec.WriteString('infPassageiro', 'xNome', infPassagem.infPassageiro.xNome);
        INIRec.WriteString('infPassageiro', 'CPF', infPassagem.infPassageiro.CPF);
        INIRec.WriteString('infPassageiro', 'tpDoc', tpDocumentoToStr(infPassagem.infPassageiro.tpDoc));
        INIRec.WriteString('infPassageiro', 'nDoc', infPassagem.infPassageiro.nDoc);
        INIRec.WriteString('infPassageiro', 'xDoc', infPassagem.infPassageiro.xDoc);
        INIRec.WriteString('infPassageiro', 'dNasc', DateToStr(infPassagem.infPassageiro.dNasc));
        INIRec.WriteString('infPassageiro', 'fone', infPassagem.infPassageiro.Fone);
        INIRec.WriteString('infPassageiro', 'email', infPassagem.infPassageiro.Email);
      end;

      // Informações da Viagem

      for i := 0 to infViagem.Count - 1 do
      begin
        sSecao := 'infViagem' + IntToStrZero(i + 1, 3);

        with infViagem[i] do
        begin
          INIRec.WriteString(sSecao, 'cPercurso', infViagem[i].cPercurso);
          INIRec.WriteString(sSecao, 'xPercurso', infViagem[i].xPercurso);
          INIRec.WriteString(sSecao, 'tpViagem', tpViagemToStr(infViagem[i].tpViagem));
          INIRec.WriteString(sSecao, 'tpServ', tpServicoToStr(infViagem[i].tpServ));
          INIRec.WriteString(sSecao, 'tpAcomodacao', tpAcomodacaoToStr(infViagem[i].tpAcomodacao));
          INIRec.WriteString(sSecao, 'tpTrecho', tpTrechoToStr(infViagem[i].tpTrecho));
          INIRec.WriteString(sSecao, 'dhViagem', DateTimeToStr(infViagem[i].dhViagem));
          INIRec.WriteString(sSecao, 'dhConexao', DateTimeToStr(infViagem[i].dhConexao));
          INIRec.WriteString(sSecao, 'prefixo', infViagem[i].Prefixo);
          INIRec.WriteInteger(sSecao, 'poltrona', infViagem[i].Poltrona);
          INIRec.WriteString(sSecao, 'plataforma', infViagem[i].Plataforma);

          // Informações da Travessia

          if infViagem[i].infTravessia.tpVeiculo <> tvNenhum then
          begin
            INIRec.WriteString(sSecao, 'tpVeiculo', tpVeiculoToStr(infViagem[i].infTravessia.tpVeiculo));
            INIRec.WriteString(sSecao, 'sitVeiculo', SitVeiculoToStr(infViagem[i].infTravessia.sitVeiculo));
          end;
        end;
      end;

      // Informações sobre os Valores do BPe

      INIRec.WriteFloat('infValorBPe', 'vBP', infValorBPe.vBP);
      INIRec.WriteFloat('infValorBPe', 'vDesconto', infValorBPe.vDesconto);
      INIRec.WriteFloat('infValorBPe', 'vPgto', infValorBPe.vPgto);
      INIRec.WriteFloat('infValorBPe', 'vTroco', infValorBPe.vTroco);
      INIRec.WriteString('infValorBPe', 'tpDesconto', tpDescontoToStr(infValorBPe.tpDesconto));
      INIRec.WriteString('infValorBPe', 'xDesconto', infValorBPe.xDesconto);
      INIRec.WriteString('infValorBPe', 'cDesconto', infValorBPe.cDesconto);

      // Informações da Viagem

      for i := 0 to infValorBPe.Comp.Count - 1 do
      begin
        sSecao := 'Comp' + IntToStrZero(i + 1, 3);

        with infValorBPe.Comp[i] do
        begin
          INIRec.WriteString(sSecao, 'tpComp', tpComponenteToStr(infValorBPe.Comp[i].tpComp));
          INIRec.WriteFloat(sSecao, 'vComp', infValorBPe.Comp[i].vComp);
        end;
      end;

      // ICMS

      INIRec.WriteString('ICMS', 'CST', CSTICMSTOStr(Imp.ICMS.CST));
      INIRec.WriteFloat('ICMS', 'vBC', Imp.ICMS.vBC);
      INIRec.WriteFloat('ICMS', 'pICMS', Imp.ICMS.pICMS);
      INIRec.WriteFloat('ICMS', 'vICMS', Imp.ICMS.vICMS);
      INIRec.WriteFloat('ICMS', 'pRedBC', Imp.ICMS.pRedBC);
      INIRec.WriteFloat('ICMS', 'vCred', Imp.ICMS.vCred);
      INIRec.WriteFloat('ICMS', 'vICMSDeson', Imp.ICMS.vICMSDeson);
      INIRec.WriteString('ICMS', 'cBenef', Imp.ICMS.cBenef);
      INIRec.WriteFloat('ICMS', 'vTotTrib', Imp.vTotTrib);
      INIRec.WriteString('ICMS', 'infAdFisco', Imp.infAdFisco);

      // ICMSUFFim

      INIRec.WriteFloat('ICMSUFFim', 'vBCUFFim', Imp.ICMSUFFim.vBCUFFim);
      INIRec.WriteFloat('ICMSUFFim', 'pFCPUFFim', Imp.ICMSUFFim.pFCPUFFim);
      INIRec.WriteFloat('ICMSUFFim', 'pICMSUFFim', Imp.ICMSUFFim.pICMSUFFim);
      INIRec.WriteFloat('ICMSUFFim', 'pICMSInter', Imp.ICMSUFFim.pICMSInter);
      INIRec.WriteFloat('ICMSUFFim', 'pICMSInterPart', Imp.ICMSUFFim.pICMSInterPart);
      INIRec.WriteFloat('ICMSUFFim', 'vFCPUFFim', Imp.ICMSUFFim.vFCPUFFim);
      INIRec.WriteFloat('ICMSUFFim', 'vICMSUFFim', Imp.ICMSUFFim.vICMSUFFim);
      INIRec.WriteFloat('ICMSUFFim', 'vICMSUFIni', Imp.ICMSUFFim.vICMSUFIni);

      // Pagamento

      for i := 0 to pag.Count - 1 do
      begin
        sSecao := 'pag' + IntToStrZero(i + 1, 2);

        with pag[I] do
        begin
          INIRec.WriteString(sSecao, 'tPag', FormaPagamentoBPeToStr(tPag));
          INIRec.WriteString(sSecao, 'xPag', xPag);
          INIRec.WriteString(sSecao, 'nDocPag', nDocPag);
          INIRec.WriteFloat(sSecao, 'vPag', vPag);
          INIRec.WriteString(sSecao, 'tpIntegra', tpIntegraToStr(tpIntegra));
          INIRec.WriteString(sSecao, 'CNPJ', CNPJ);
          INIRec.WriteString(sSecao, 'tBand', BandeiraCardToStr(tBand));
          INIRec.WriteString(sSecao, 'xBand', xBand);
          INIRec.WriteString(sSecao, 'cAut', cAut);
          INIRec.WriteString(sSecao, 'nsuTrans', nsuTrans);
          INIRec.WriteString(sSecao, 'nsuHost', nsuHost);
          INIRec.WriteInteger(sSecao, 'nParcelas', nParcelas);
          INIRec.WriteString(sSecao, 'infAdCard', infAdCard);
        end;
      end;

      // Autorizados a baixar o XML

      for i := 0 to autXML.Count - 1 do
      begin
        sSecao := 'autXML' + IntToStrZero(i + 1, 2);

        with autXML.Items[i] do
        begin
          INIRec.WriteString(sSecao, 'CNPJCPF', CNPJCPF);
        end;
      end;

      // Informações Adicionais

      INIRec.WriteString('infAdic', 'infAdFisco', InfAdic.infAdFisco);
      INIRec.WriteString('infAdic', 'infCpl', InfAdic.infCpl);

      // Informações do Responsável Técnico

      INIRec.WriteString('infRespTec', 'CNPJ', infRespTec.CNPJ);
      INIRec.WriteString('infRespTec', 'xContato', infRespTec.xContato);
      INIRec.WriteString('infRespTec', 'email', infRespTec.email);
      INIRec.WriteString('infRespTec', 'fone', infRespTec.fone);
    end;

    IniBPe := TStringList.Create;
    try
      INIRec.GetStrings(IniBPe);
      Result := StringReplace(IniBPe.Text, sLineBreak + sLineBreak, sLineBreak, [rfReplaceAll]);
    finally
      IniBPe.Free;
    end;
  finally
    INIRec.Free;
  end;
end;

function Bilhete.GerarTXT: String;
var
  IdAnterior : String;
begin
{
  with TACBrBPe(TBilhetes(Collection).ACBrBPe) do
  begin
    IdAnterior := BPe.infBPe.ID;
    FBPeW.Gerador.Opcoes.FormatoAlerta  := Configuracoes.Geral.FormatoAlerta;
    FBPeW.Gerador.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
    FBPeW.Gerador.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
    FBPeW.Gerador.Opcoes.IdentarXML     := Configuracoes.Geral.IdentarXML;
    FBPeW.Opcoes.NormatizarMunicipios   := Configuracoes.Arquivos.NormatizarMunicipios;
    FBPeW.Opcoes.PathArquivoMunicipios  := Configuracoes.Arquivos.PathArquivoMunicipios;
  end;

  FBPeW.Opcoes.GerarTXTSimultaneamente := True;

  FBPeW.GerarXml;
  XMLOriginal := FBPeW.Gerador.ArquivoFormatoXML;

  // XML gerado pode ter nova Chave e ID, então devemos calcular novamente o
  // nome do arquivo, mantendo o PATH do arquivo carregado
  if (NaoEstaVazio(FNomeArq) and (IdAnterior <> FBPe.infBPe.ID)) then
    FNomeArq := CalcularNomeArquivoCompleto('', ExtractFilePath(FNomeArq));

  FAlertas := FBPeW.Gerador.ListaDeAlertas.Text;
  Result := FBPeW.Gerador.ArquivoFormatoTXT;
  }
  Result := '';
end;

function Bilhete.CalcularNomeArquivo: String;
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

function Bilhete.CalcularPathArquivo: String;
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

function Bilhete.CalcularNomeArquivoCompleto(NomeArquivo: String;
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

//function Bilhete.ValidarConcatChave: Boolean;
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

function Bilhete.GetConfirmada: Boolean;
begin
  Result := TACBrBPe(TBilhetes(Collection).ACBrBPe).CstatConfirmada(
    FBPe.procBPe.cStat);
end;

function Bilhete.GetcStat: Integer;
begin
  Result := FBPe.procBPe.cStat;
end;

function Bilhete.GetProcessada: Boolean;
begin
  Result := TACBrBPe(TBilhetes(Collection).ACBrBPe).CstatProcessado(
    FBPe.procBPe.cStat);
end;

function Bilhete.GetCancelada: Boolean;
begin
  Result := TACBrBPe(TBilhetes(Collection).ACBrBPe).CstatCancelada(
    FBPe.procBPe.cStat);
end;

function Bilhete.GetMsg: String;
begin
  Result := FBPe.procBPe.xMotivo;
end;

function Bilhete.GetNumID: String;
begin
  Result := OnlyNumber(BPe.infBPe.ID);
end;

function Bilhete.GetXMLAssinado: String;
begin
  if EstaVazio(FXMLAssinado) then
    Assinar;

  Result := FXMLAssinado;
end;

procedure Bilhete.SetXML(const AValue: String);
begin
  LerXML(AValue);
end;

procedure Bilhete.SetXMLOriginal(const AValue: String);
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

function Bilhete.LerArqIni(const AIniString: String): Boolean;
var
  INIRec: TMemIniFile;
  sSecao, versao, sFim: String;
  OK: Boolean;
  I: Integer;
begin
  Result := True;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with FBPe do
    begin
      infBPe.versao := StringToFloatDef(INIRec.ReadString('infBPe', 'versao', VersaoBPeToStr(FConfiguracoes.Geral.VersaoDF)),0);

      versao := FloatToString(infBPe.versao, '.', '#0.00');
      FConfiguracoes.Geral.VersaoDF := StrToVersaoBPe(versao);

      Ide.tpAmb   := StrToTipoAmbiente(OK, INIRec.ReadString(sSecao, 'tpAmb', IntToStr(Integer(FConfiguracoes.WebServices.Ambiente))));
      Ide.modelo  := INIRec.ReadInteger('ide', 'mod', 63);
      Ide.serie   := INIRec.ReadInteger('ide', 'serie', 1);
      Ide.nBP     := INIRec.ReadInteger('ide', 'nBP', 0);
      Ide.cBP     := INIRec.ReadInteger('ide', 'cBP', 0);
      Ide.modal   := StrToModalBPe(INIRec.ReadString('ide', 'modal', '1'));
      Ide.dhEmi   := StringToDateTime(INIRec.ReadString('ide', 'dhEmi', '0'));
      Ide.tpEmis  := StrToTipoEmissao(OK, INIRec.ReadString(sSecao, 'tpEmis', IntToStr(FConfiguracoes.Geral.FormaEmissaoCodigo)));
      Ide.verProc := INIRec.ReadString('ide', 'verProc', 'ACBrBPe');
      Ide.tpBPe   := StrTotpBPe(INIRec.ReadString('ide', 'tpBPe', '0'));
      Ide.indPres := StrToPresencaComprador(INIRec.ReadString('ide', 'indPres', '1'));
      Ide.UFIni   := INIRec.ReadString('ide', 'UFIni', '');
      Ide.cMunIni := INIRec.ReadInteger('ide', 'cMunIni', 0);
      Ide.UFFim   := INIRec.ReadString('ide', 'UFFim', '');
      Ide.cMunFim := INIRec.ReadInteger('ide', 'cMunFim', 0);
      Ide.dhCont  := StringToDateTime(INIRec.ReadString('ide', 'dhCont', '0'));
      Ide.xJust   := INIRec.ReadString('ide', 'xJust', '');

      //
      // Seção [emit] Emitente do BP-e
      //
      Emit.CNPJ  := INIRec.ReadString('emit', 'CNPJ', '');
      Emit.IE    := INIRec.ReadString('emit', 'IE', '');
      Emit.IEST  := INIRec.ReadString('emit', 'IEST', '');
      Emit.xNome := INIRec.ReadString('emit', 'xNome', '');
      Emit.xFant := INIRec.ReadString('emit', 'xFant', '');
      Emit.IM    := INIRec.ReadString('emit', 'IM', '');
      Emit.CNAE  := INIRec.ReadString('emit', 'CNAE', '');
      Emit.CRT   := StrToCRT(INIRec.ReadString('emit', 'CRT', '3'));
      Emit.TAR   := INIRec.ReadString('emit', 'TAR', '');

      Emit.enderEmit.xLgr    := INIRec.ReadString('emit', 'xLgr', '');
      Emit.enderEmit.nro     := INIRec.ReadString('emit', 'nro', '');
      Emit.enderEmit.xCpl    := INIRec.ReadString('emit', 'xCpl', '');
      Emit.enderEmit.xBairro := INIRec.ReadString('emit', 'xBairro', '');
      Emit.enderEmit.cMun    := INIRec.ReadInteger('emit', 'cMun', 0);
      Emit.enderEmit.xMun    := INIRec.ReadString('emit', 'xMun', '');
      Emit.enderEmit.CEP     := INIRec.ReadInteger('emit', 'CEP', 0);
      Emit.enderEmit.UF      := INIRec.ReadString('emit', 'UF', '');
      Emit.enderEmit.fone    := INIRec.ReadString('emit', 'fone', '');
      Emit.enderEmit.Email   := INIRec.ReadString('emit', 'Email', '');

      ide.cUF := INIRec.ReadInteger('ide', 'cUF', UFparaCodigoUF(Emit.enderEmit.UF));

      //
      // Seção [comp] Comprador
      //
      Comp.xNome         := INIRec.ReadString('comp', 'xNome', '');
      Comp.CNPJCPF       := INIRec.ReadString('comp', 'CNPJCPF', '');
      Comp.idEstrangeiro := INIRec.ReadString('comp', 'idEstrangeiro', '');
      Comp.IE            := INIRec.ReadString('comp', 'IE', '');

      Comp.enderComp.xLgr    := INIRec.ReadString('comp', 'xLgr', '');
      Comp.enderComp.nro     := INIRec.ReadString('comp', 'nro', '');
      Comp.enderComp.xCpl    := INIRec.ReadString('comp', 'xCpl', '');
      Comp.enderComp.xBairro := INIRec.ReadString('comp', 'xBairro', '');
      Comp.enderComp.cMun    := INIRec.ReadInteger('comp', 'cMun', 0);
      Comp.enderComp.xMun    := INIRec.ReadString('comp', 'xMun', '');
      Comp.enderComp.CEP     := INIRec.ReadInteger('comp', 'CEP', 0);
      Comp.enderComp.UF      := INIRec.ReadString('comp', 'UF', '');
      Comp.EnderComp.cPais   := INIRec.ReadInteger('comp', 'cPais', 0);
      Comp.EnderComp.xPais   := INIRec.ReadString('comp', 'xPais', '');
      Comp.enderComp.fone    := INIRec.ReadString('comp', 'fone', '');
      Comp.enderComp.Email   := INIRec.ReadString('comp', 'Email', '');

      //
      // Seção [Agencia] Agência que comercializou o BP-e
      //
      Agencia.xNome := INIRec.ReadString('Agencia', 'xNome', '');
      Agencia.CNPJ  := INIRec.ReadString('Agencia', 'CNPJ', '');

      Agencia.enderAgencia.xLgr    := INIRec.ReadString('Agencia', 'xLgr', '');
      Agencia.enderAgencia.nro     := INIRec.ReadString('Agencia', 'nro', '');
      Agencia.enderAgencia.xCpl    := INIRec.ReadString('Agencia', 'xCpl', '');
      Agencia.enderAgencia.xBairro := INIRec.ReadString('Agencia', 'xBairro', '');
      Agencia.enderAgencia.cMun    := INIRec.ReadInteger('Agencia', 'cMun', 0);
      Agencia.enderAgencia.xMun    := INIRec.ReadString('Agencia', 'xMun', '');
      Agencia.enderAgencia.CEP     := INIRec.ReadInteger('Agencia', 'CEP', 0);
      Agencia.enderAgencia.UF      := INIRec.ReadString('Agencia', 'UF', '');
      Agencia.enderAgencia.cPais   := INIRec.ReadInteger('Agencia', 'cPais', 0);
      Agencia.enderAgencia.xPais   := INIRec.ReadString('Agencia', 'xPais', '');
      Agencia.enderAgencia.fone    := INIRec.ReadString('Agencia', 'fone', '');
      Agencia.enderAgencia.Email   := INIRec.ReadString('Agencia', 'Email', '');

      //
      // Seção [infBPeSub] Informações do BP-e Substituido
      //
      if INIRec.ReadString('infBPeSub', 'chBPe', '') <> '' then
      begin
        with infBPeSub do
        begin
          chBPe := INIRec.ReadString('infBPeSub', 'chBPe', '');
          tpSub := StrTotpSubstituicao(INIRec.ReadString('infBPeSub', 'tpSub', '1'));
        end;
      end;

      //
      // Seção [infPassagem] Informações da Passagem
      //
      infPassagem.cLocOrig   := INIRec.ReadString('infPassagem', 'cLocOrig', '');
      infPassagem.xLocOrig   := INIRec.ReadString('infPassagem', 'xLocOrig', '');
      infPassagem.cLocDest   := INIRec.ReadString('infPassagem', 'cLocDest', '');
      infPassagem.xLocDest   := INIRec.ReadString('infPassagem', 'xLocDest', '');
      infPassagem.dhEmb      := StringToDateTime(INIRec.ReadString('infPassagem', 'dhEmb', '0'));
      infPassagem.dhValidade := StringToDateTime(INIRec.ReadString('infPassagem', 'dhValidade', '0'));

      //
      // Seção [infPassageiro] Informações do Passageiro
      //
      infPassagem.infPassageiro.xNome := INIRec.ReadString('infPassageiro', 'xNome', '');
      infPassagem.infPassageiro.CPF   := INIRec.ReadString('infPassageiro', 'CPF', '');
      infPassagem.infPassageiro.tpDoc := StrTotpDocumento(INIRec.ReadString('infPassageiro', 'tpDoc', '1'));
      infPassagem.infPassageiro.nDoc  := INIRec.ReadString('infPassageiro', 'nDoc', '');
      infPassagem.infPassageiro.xDoc  := INIRec.ReadString('infPassageiro', 'xDoc', '');
      infPassagem.infPassageiro.dNasc := StringToDateTime(INIRec.ReadString('infPassageiro', 'dNasc', '0'));
      infPassagem.infPassageiro.fone  := INIRec.ReadString('infPassageiro', 'fone', '');
      infPassagem.infPassageiro.Email := INIRec.ReadString('infPassageiro', 'Email', '');

      //
      // Seção [infViagemxxx] Informações da Viagem
      //
      I := 1;
      while true do
      begin
        sSecao := 'infViagem' + IntToStrZero(I, 3);
        sFim   := INIRec.ReadString(sSecao, 'cPercurso', 'FIM');
        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with infViagem.New do
        begin
          cPercurso    := sFim;
          xPercurso    := INIRec.ReadString(sSecao, 'xPercurso', '');
          tpViagem     := StrTotpViagem(INIRec.ReadString(sSecao, 'tpViagem', '00'));
          tpServ       := StrTotpServico(INIRec.ReadString(sSecao, 'tpServ', '1'));
          tpAcomodacao := StrTotpAcomodacao(INIRec.ReadString(sSecao, 'tpAcomodacao', '1'));
          tpTrecho     := StrTotpTrecho(INIRec.ReadString(sSecao, 'tpTrecho', '1'));
          dhViagem     := StringToDateTime(INIRec.ReadString(sSecao, 'dhViagem', '0'));
          dhConexao    := StringToDateTime(INIRec.ReadString(sSecao, 'dhConexao', '0'));
          Prefixo      := INIRec.ReadString(sSecao, 'Prefixo', '');
          Poltrona     := INIRec.ReadInteger(sSecao, 'Poltrona', 0);
          Plataforma   := INIRec.ReadString(sSecao, 'Plataforma', '');

          //
          // Informações da Travessia
          //
          if INIRec.ReadString(sSecao, 'tpVeiculo', '') <> '' then
          begin
            with infTravessia do
            begin
              tpVeiculo  := StrTotpVeiculo(INIRec.ReadString(sSecao, 'tpVeiculo', '01'));
              sitVeiculo := StrToSitVeiculo(INIRec.ReadString(sSecao, 'sitVeiculo', '01'));
            end;
          end;
        end;

        Inc(I);
      end;

      //
      // Seção [infValorBPe] Informações dos Valores do BP-e
      //
      infValorBPe.vBP        := StringToFloatDef(INIRec.ReadString('infValorBPe', 'vBP', ''), 0);
      infValorBPe.vDesconto  := StringToFloatDef(INIRec.ReadString('infValorBPe', 'vDesconto', ''), 0);
      infValorBPe.vPgto      := StringToFloatDef(INIRec.ReadString('infValorBPe', 'vPgto', ''), 0);
      infValorBPe.vTroco     := StringToFloatDef(INIRec.ReadString('infValorBPe', 'vTroco', ''), 0);
      infValorBPe.tpDesconto := StrTotpDesconto(INIRec.ReadString('infValorBPe', 'tpDesconto', '01'));
      infValorBPe.xDesconto  := INIRec.ReadString('infValorBPe', 'xDesconto', '');
      infValorBPe.cDesconto  := INIRec.ReadString('infValorBPe', 'cDesconto', '');

      //
      // Seção [Compxxx] Componentes do Valor do BPe
      //
      I := 1;
      while true do
      begin
        sSecao := 'Comp' + IntToStrZero(I, 3);
        sFim   := INIRec.ReadString(sSecao, 'tpComp', 'FIM');
        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        with infValorBPe.Comp.New do
        begin
          tpComp := StrTotpComponente(sFim);
          vComp  := StringToFloatDef(INIRec.ReadString(sSecao, 'vComp', ''), 0);
        end;

        Inc(I);
      end;

      //
      // Seção [ICMS] Informacoes relativas aos Impostos
      //
      with Imp do
      begin
        sSecao := 'ICMS';
        sFim   := INIRec.ReadString(sSecao, 'CST', 'FIM');

        if (sFim <> 'FIM') then
        begin
          with ICMS do
          begin
            CST    := StrToCSTICMS(sFim);
            pRedBC := StringToFloatDef(INIRec.ReadString(sSecao, 'pRedBC', ''), 0);
            vBC    := StringToFloatDef(INIRec.ReadString(sSecao, 'vBC', ''), 0);
            pICMS  := StringToFloatDef(INIRec.ReadString(sSecao, 'pICMS', ''), 0);
            vICMS  := StringToFloatDef(INIRec.ReadString(sSecao, 'vICMS', ''), 0);
            vCred  := StringToFloatDef(INIRec.ReadString(sSecao, 'vCred', ''), 0);

            pRedBCOutraUF := StringToFloatDef(INIRec.ReadString(sSecao, 'pRedBCOutraUF', ''), 0);
            vBCOutraUF    := StringToFloatDef(INIRec.ReadString(sSecao, 'vBCOutraUF', ''), 0);
            pICMSOutraUF  := StringToFloatDef(INIRec.ReadString(sSecao, 'pICMSOutraUF', ''), 0);
            vICMSOutraUF  := StringToFloatDef(INIRec.ReadString(sSecao, 'vICMSOutraUF', ''), 0);

            vICMSDeson := StringToFloatDef(INIRec.ReadString(sSecao, 'vICMSDeson', ''), 0);
            cBenef     := INIRec.ReadString(sSecao, 'cBenef', '');
          end;

          vTotTrib   := StringToFloatDef(INIRec.ReadString(sSecao, 'vTotTrib', ''), 0);
          infAdFisco := INIRec.ReadString(sSecao, 'infAdFisco', '');
        end;

        if StringToFloatDef(INIRec.ReadString('ICMSUFFim', 'pICMSInterPart', ''), 0) <> 0 then
        begin
          ICMSUFFim.vBCUFFim       := StringToFloatDef(INIRec.ReadString('ICMSUFFim', 'vBCUFFim', ''), 0);
          ICMSUFFim.pFCPUFFim      := StringToFloatDef(INIRec.ReadString('ICMSUFFim', 'pFCPUFFim', ''), 0);
          ICMSUFFim.pICMSUFFim     := StringToFloatDef(INIRec.ReadString('ICMSUFFim', 'pICMSUFFim', ''), 0);
          ICMSUFFim.pICMSInter     := StringToFloatDef(INIRec.ReadString('ICMSUFFim', 'pICMSInter', ''), 0);
          ICMSUFFim.pICMSInterPart := StringToFloatDef(INIRec.ReadString('ICMSUFFim', 'pICMSInterPart', ''), 0);
          ICMSUFFim.vFCPUFFim      := StringToFloatDef(INIRec.ReadString('ICMSUFFim', 'vFCPUFFim', ''), 0);
          ICMSUFFim.vICMSUFFim     := StringToFloatDef(INIRec.ReadString('ICMSUFFim', 'vICMSUFFim', ''), 0);
          ICMSUFFim.vICMSUFIni     := StringToFloatDef(INIRec.ReadString('ICMSUFFim', 'vICMSUFIni', ''), 0);
        end;
      end;

      //
      // Seção [Pagxx] Dados do Pagamento 01-10
      //
      I := 1 ;
      while true do
      begin
        sSecao := 'pag'+IntToStrZero(I,2) ;
        sFim   := INIRec.ReadString(sSecao, 'tpag', 'FIM');
        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break ;

        with pag.New do
        begin
          tPag    := StrToFormaPagamentoBPe(sFim);
          xPag    := INIRec.ReadString(sSecao, 'xPag', '');
          nDocPag := INIRec.ReadString(sSecao, 'nDocPag', '');
          vPag    := StringToFloatDef(INIRec.ReadString(sSecao, 'vPag', ''), 0);

          tpIntegra := StrTotpIntegra(INIRec.ReadString(sSecao, 'tpIntegra', ''));
          CNPJ      := INIRec.ReadString(sSecao, 'CNPJ', '');
          tBand     := StrToBandeiraCard(INIRec.ReadString(sSecao, 'tBand', '99'));
          xBand     := INIRec.ReadString(sSecao, 'xBand', '');
          cAut      := INIRec.ReadString(sSecao, 'cAut', '');
          nsuTrans  := INIRec.ReadString(sSecao, 'nsuTrans', '');
          nsuHost   := INIRec.ReadString(sSecao, 'nsuHost', '');
          nParcelas := INIRec.ReadInteger(sSecao, 'nParcelas', 1);
          infAdCard := INIRec.ReadString(sSecao, 'infAdCard', '');
        end;

        Inc(I);
      end;

      //
      // Seção [auxXMLxx] Autorizados para Download do XML do BPe 01-10
      //
      I := 1 ;
      while true do
      begin
        sSecao := 'autXML' + IntToStrZero(I,2) ;
        sFim   := OnlyNumber(INIRec.ReadString(sSecao, 'CNPJCPF', 'FIM'));
        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break ;

        with autXML.New do
          CNPJCPF := sFim;

        Inc(I);
      end;

      //
      // Seção [infAdic] Informações Adicionais
      //
      InfAdic.infAdFisco := INIRec.ReadString('infAdic','infAdFisco', '');
      InfAdic.infCpl     := INIRec.ReadString('infAdic','infCpl', '');

      sSecao := 'infRespTec';
      if INIRec.SectionExists(sSecao) then
      begin
        with infRespTec do
        begin
          CNPJ     := INIRec.ReadString(sSecao, 'CNPJ', '');
          xContato := INIRec.ReadString(sSecao, 'xContato', '');
          email    := INIRec.ReadString(sSecao, 'email', '');
          fone     := INIRec.ReadString(sSecao, 'fone', '');
        end;
      end;
    end;

    GerarXML;
  finally
     INIRec.Free;
  end;
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


function TBilhetes.Add: Bilhete;
begin
  Result := Bilhete(inherited Add);
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

function TBilhetes.GetItem(Index: integer): Bilhete;
begin
  Result := Bilhete(inherited Items[Index]);
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

function TBilhetes.Insert(Index: integer): Bilhete;
begin
  Result := Bilhete(inherited Insert(Index));
end;

procedure TBilhetes.SetItem(Index: integer; const Value: Bilhete);
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
    // Atribui Nome do arquivo a novas Bilhetes inseridas //
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
  // Verifica se precisa Converter de UTF8 para a String nativa da IDE //
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
      P := pos('</procBPe>', XMLStr);  // BPe obtida pelo Portal da Receita

    if P > 0 then
    begin
      ABPeXML := copy(XMLStr, 1, P + 10);
      XMLStr := Trim(copy(XMLStr, P + 10, length(XMLStr)));
    end
    else
    begin
      ABPeXML := copy(XMLStr, 1, N + 6);
      XMLStr := Trim(copy(XMLStr, N + 6, length(XMLStr)));
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

function TBilhetes.GravarTXT(PathNomeArquivo: String): Boolean;
var
  SL: TStringList;
  ArqTXT: String;
  I: integer;
begin
  Result := False;
  SL := TStringList.Create;
  try
    SL.Clear;
    for I := 0 to Self.Count - 1 do
    begin
      ArqTXT := Self.Items[I].GerarTXT;
      SL.Add(ArqTXT);
    end;

    if SL.Count > 0 then
    begin
      // Inserindo cabeçalho //
      SL.Insert(0, 'BILHETE|' + IntToStr(Self.Count));

      // Apagando as linhas em branco //
      i := 0;
      while (i <= SL.Count - 1) do
      begin
        if SL[I] = '' then
          SL.Delete(I)
        else
          Inc(i);
      end;

      if EstaVazio(PathNomeArquivo) then
        PathNomeArquivo := PathWithDelim(
          TACBrBPe(FACBrBPe).Configuracoes.Arquivos.PathSalvar) + 'BPe.TXT';

      SL.SaveToFile(PathNomeArquivo);
      Result := True;
    end;
  finally
    SL.Free;
  end;
end;

end.
