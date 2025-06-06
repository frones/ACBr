{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrMDFeManifestos;

interface

uses
  Classes, 
  SysUtils, 
  StrUtils,
  ACBrMDFeConfiguracoes,
  ACBrMDFe.Classes,
  {$IfDef USE_ACBr_XMLDOCUMENT}
  ACBrMDFe.XmlReader,
  ACBrMDFe.XmlWriter,
  {$Else}
  pmdfeMDFeR,
  pmdfeMDFeW,
  {$EndIf}
  ACBrMDFe.IniReader, ACBrMDFe.IniWriter,
  pcnConversao,
  pcnLeitor;

type

  { TManifesto }

  TManifesto = class(TCollectionItem)
  private
    FMDFe: TMDFe;
    // Xml
    {$IfDef USE_ACBr_XMLDOCUMENT}
    FMDFeR: TMDFeXmlReader;
    FMDFeW: TMDFeXmlWriter;
    {$Else}
    FMDFeR: TMDFeR;
    FMDFeW: TMDFeW;
    {$EndIf}
    // Ini
    FMDFeIniR: TMDFeIniReader;
    FMDFeIniW: TMDFeIniWriter;

    FConfiguracoes: TConfiguracoesMDFe;
    FXMLAssinado: String;
    FXMLOriginal: String;
    FAlertas: String;
    FErroValidacao: String;
    FErroValidacaoCompleto: String;
    FErroRegrasdeNegocios: String;
    FNomeArq: String;

    function GetConfirmado: Boolean;
    function GetProcessado: Boolean;
    function GetCancelado: Boolean;

    function GetMsg: String;
    function GetNumID: String;
    function GetXMLAssinado: String;
    procedure SetXML(const AValue: String);
    procedure SetXMLOriginal(const AValue: String);
    function ValidarConcatChave: Boolean;
    function CalcularNomeArquivo: String;
    function CalcularPathArquivo: String;
    function GetcStat: Integer;
  public
    constructor Create(Collection2: TCollection); override;
    destructor Destroy; override;
    procedure Imprimir;
    procedure ImprimirPDF; overload;
    function ImprimirPDF(AStream: TStream): Boolean; overload;

    procedure Assinar;
    procedure Validar;
    function VerificarAssinatura: Boolean;
    function ValidarRegrasdeNegocios: Boolean;

    function LerXML(const AXML: String): Boolean;
    function LerArqIni(const AIniString: String): Boolean;
    function GerarMDFeIni: String;

    function GerarXML: String;
    function GravarXML(const NomeArquivo: String = ''; const PathArquivo: String = ''): Boolean;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings = nil;
      EnviaPDF: Boolean = True; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil; ManterPDFSalvo: Boolean = True;
      sBCC: TStrings = nil);

    function CalcularNomeArquivoCompleto(NomeArquivo: String = '';
      PathArquivo: String = ''): String;

    property NomeArq: String read FNomeArq write FNomeArq;

    property MDFe: TMDFe read FMDFe;

    // Atribuir a "XML", faz o componente transferir os dados lido para as propriedades internas e "XMLAssinado"
    property XML: String         read FXMLOriginal   write SetXML;
    // Atribuir a "XMLOriginal", reflete em XMLAssinado, se existir a tag de assinatura
    property XMLOriginal: String read FXMLOriginal   write SetXMLOriginal;
    property XMLAssinado: String read GetXMLAssinado write FXMLAssinado;

    property Confirmado: Boolean read GetConfirmado;
    property Processado: Boolean read GetProcessado;
    property Cancelado: Boolean  read GetCancelado;
    property cStat: Integer read GetcStat;
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

    function GetItem(Index: integer): TManifesto;
    procedure SetItem(Index: integer; const Value: TManifesto);

    procedure VerificarDAMDFE;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    procedure GerarMDFe;
    procedure Assinar;
    procedure Validar;
    function VerificarAssinatura(out Erros: String): Boolean;
    function ValidarRegrasdeNegocios(out Erros: String): Boolean;
    procedure Imprimir;
    procedure ImprimirPDF; overload;
    procedure ImprimirPDF(AStream: TStream); overload;

    function Add: TManifesto;
    function Insert(Index: integer): TManifesto;

    property Items[Index: integer]: TManifesto read GetItem write SetItem; default;

    function GetNamePath: String; override;
    // Incluido o Parametro AGerarMDFe que determina se após carregar os dados do MDFe
    // para o componente, será gerado ou não novamente o XML do MDFe.
    function LoadFromFile(const CaminhoArquivo: String; AGerarMDFe: Boolean = False): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarMDFe: Boolean = False): Boolean;
    function LoadFromString(const AXMLString: String; AGerarMDFe: Boolean = False): Boolean;
    function LoadFromIni(const AIniString: String): Boolean;

    function GerarIni: String;
    function GravarXML(const PathNomeArquivo: String = ''): Boolean;

    property ACBrMDFe: TComponent read FACBrMDFe;
  end;

implementation

uses
  Dateutils, 
  IniFiles,
  synautil,
  ACBrMDFe,
  ACBrUtil.Base,
  ACBrUtil.XMLHTML,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBrDFeUtil, 
  pmdfeConversaoMDFe;

{ Manifesto }

constructor TManifesto.Create(Collection2: TCollection);
begin
  inherited Create(Collection2);

  FMDFe := TMDFe.Create;
  // Xml
  {$IfDef USE_ACBr_XMLDOCUMENT}
  FMDFeR := TMDFeXmlReader.Create(FMDFe);
  FMDFeW := TMDFeXmlWriter.Create(FMDFe);
  {$Else}
  FMDFeR := TMDFeR.Create(FMDFe);
  FMDFeW := TMDFeW.Create(FMDFe);
  {$EndIf}
  // Ini
  FMDFeIniR := TMDFeIniReader.Create(FMDFe);
  FMDFeIniW := TMDFeIniWriter.Create(FMDFe);

  FConfiguracoes := TACBrMDFe(TManifestos(Collection).ACBrMDFe).Configuracoes;

  FMDFe.Ide.verProc := 'ACBrMDFe';

  with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
  begin
    FMDFe.Ide.modelo := '58';
    FMDFe.infMDFe.Versao := VersaoMDFeToDbl(Configuracoes.Geral.VersaoDF);
    FMDFe.Ide.tpAmb := Configuracoes.WebServices.Ambiente;
    FMDFe.Ide.tpEmis := Configuracoes.Geral.FormaEmissao;
  end;
end;

destructor TManifesto.Destroy;
begin
  // Xml
  FMDFeW.Free;
  FMDFeR.Free;
  // Ini
  FMDFeIniR.Free;
  FMDFeIniW.Free;

  FMDFe.Free;

  inherited Destroy;
end;

procedure TManifesto.Imprimir;
begin
  with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
  begin
    if not Assigned(DAMDFE) then
      raise EACBrMDFeException.Create('Componente DAMDFE não associado.')
    else
      DAMDFE.ImprimirDAMDFE(MDFe);
  end;
end;

procedure TManifesto.ImprimirPDF;
begin
  with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
  begin
    if not Assigned(DAMDFE) then
      raise EACBrMDFeException.Create('Componente DAMDFE não associado.')
    else
      DAMDFE.ImprimirDAMDFEPDF(MDFe);
  end;
end;

function TManifesto.ImprimirPDF(AStream: TStream): Boolean;
begin
  with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
  begin
    if not Assigned(DAMDFE) then
      raise EACBrMDFeException.Create('Componente DAMDFE não associado.')
    else
    begin
      AStream.Size := 0;
      DAMDFE.ImprimirDAMDFEPDF(AStream, MDFe);
      Result := True;
    end;
  end;
end;

procedure TManifesto.Assinar;
var
  XMLStr: String;
  XMLUTF8: AnsiString;
  Leitor: TLeitor;
begin
  with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
  begin
    if not Assigned(SSL.AntesDeAssinar) then
      SSL.ValidarCNPJCertificado( MDFe.Emit.CNPJCPF );
  end;

  // Gera novamente, para processar propriedades que podem ter sido modificadas
  XMLStr := GerarXML;

  // XML já deve estar em UTF8, para poder ser assinado //
  XMLUTF8 := ConverteXMLtoUTF8(XMLStr);

  with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
  begin
    FXMLAssinado := SSL.Assinar(String(XMLUTF8), 'MDFe', 'infMDFe');
    // SSL.Assinar() sempre responde em UTF8...
    FXMLOriginal := FXMLAssinado;

    Leitor := TLeitor.Create;
    try
      leitor.Grupo := FXMLAssinado;
      MDFe.signature.URI := Leitor.rAtributo('Reference URI=');
      MDFe.signature.DigestValue := Leitor.rCampo(tcStr, 'DigestValue');
      MDFe.signature.SignatureValue := Leitor.rCampo(tcStr, 'SignatureValue');
      MDFe.signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');
    finally
      Leitor.Free;
    end;

    MDFe.infMDFeSupl.qrCodMDFe := GetURLQRCode(MDFe);
    GerarXML;

    if Configuracoes.Arquivos.Salvar and
      (not Configuracoes.Arquivos.SalvarApenasMDFeProcessados) then
    begin
      if NaoEstaVazio(NomeArq) then
        Gravar(NomeArq, FXMLAssinado)
      else
        Gravar(CalcularNomeArquivoCompleto(), FXMLAssinado);
    end;
  end;
end;

procedure TManifesto.Validar;
var
  Erro, AXML, AXMLModal, TagModal: String;
  MDFeEhValida, ModalEhValido: Boolean;
  ALayout: TLayOutMDFe;
begin
  AXML := FXMLAssinado;

  if AXML = '' then
    AXML := XMLOriginal;

  AXMLModal := Trim(RetornarConteudoEntre(AXML, '<infModal', '</infModal>'));

  case TACBrMDFe(TManifestos(Collection).ACBrMDFe).IdentificaSchemaModal(AXML) of
    schmdfeModalAereo:       TagModal := 'aereo';
    schmdfeModalAquaviario:  TagModal := 'aquav';
    schmdfeModalFerroviario: TagModal := 'ferrov';
    schmdfeModalRodoviario:  TagModal := 'rodo';
  end;

  AXMLModal := '<' + TagModal + ' xmlns="' + ACBRMDFE_NAMESPACE + '">' +
                  Trim(RetornarConteudoEntre(AXML, '<' + TagModal + '>', '</' + TagModal + '>')) +
               '</' + TagModal + '>';

  AXMLModal := '<?xml version="1.0" encoding="UTF-8" ?>' + AXMLModal;

  with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
  begin
    ALayout := LayMDFeRecepcao;

    // Extraindo apenas os dados da MDFe (sem mdfeProc)
    AXML := ObterDFeXML(AXML, 'MDFe', ACBRMDFE_NAMESPACE);

    if EstaVazio(AXML) then
    begin
      Erro := ACBrStr('MDFe não encontrada no XML');
      MDFeEhValida := False;
    end
    else
    begin
      ModalEhValido := SSL.Validar(AXMLModal, GerarNomeArqSchemaModal(AXML, FMDFe.infMDFe.Versao), Erro);

      if not ModalEhValido then
      begin
        FErroValidacao := ACBrStr('Falha na validação do Modal do Manifesto: ') +
          IntToStr(MDFe.Ide.nMDF) + sLineBreak + FAlertas ;
        FErroValidacaoCompleto := FErroValidacao + sLineBreak + Erro;

        raise EACBrMDFeException.CreateDef(
          IfThen(Configuracoes.Geral.ExibirErroSchema, ErroValidacaoCompleto,
          ErroValidacao));
      end;

      MDFeEhValida := SSL.Validar(AXML, GerarNomeArqSchema(ALayout, FMDFe.infMDFe.Versao), Erro);
    end;

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

function TManifesto.VerificarAssinatura: Boolean;
var
  Erro, AXML: String;
  AssEhValida: Boolean;
begin
  AXML := XMLAssinado;

  with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
  begin
    // Extraindo apenas os dados da MDFe (sem mdfeProc)
    AXML := ObterDFeXML(AXML, 'MDFe', ACBRMDFE_NAMESPACE);

    if EstaVazio(AXML) then
    begin
      Erro := ACBrStr('MDFe não encontrada no XML');
      AssEhValida := False;
    end
    else
      AssEhValida := SSL.VerificarAssinatura(AXML, Erro, 'infMDFe');

    if not AssEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação da assinatura do Manifesto: ') +
        IntToStr(MDFe.Ide.nMDF) + sLineBreak + Erro;
    end;
  end;

  Result := AssEhValida;
end;

function TManifesto.ValidarRegrasdeNegocios: Boolean;
var
  Erros{, Log}: String;
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
  Agora := DataHoraTimeZoneModoDeteccao( TACBrMDFe(TManifestos(Collection).ACBrMDFe) );   //Converte o DateTime do Sistema para o TimeZone configurado, para evitar divergência de Fuso Horário.
  GravaLog('Inicio da Validação');

  with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
  begin
    Erros := '';

    GravaLog('Validar: 897-Código do documento: ' + IntToStr(MDFe.Ide.nMDF));
    if not ValidarCodigoDFe(MDFe.Ide.cMDF, MDFe.Ide.nMDF) then
      AdicionaErro('897-Rejeição: Código numérico em formato inválido ');

    GravaLog('Regra: G001 - Validar: 252-Ambiente');
    if (MDFe.Ide.tpAmb <> Configuracoes.WebServices.Ambiente) then
      AdicionaErro('252-Rejeição: Tipo do ambiente do MDF-e difere do ambiente do Web Service');

    GravaLog('Regra: G002 - Validar 226-UF');
    if copy(IntToStr(MDFe.Emit.EnderEmit.cMun), 1, 2) <> IntToStr(Configuracoes.WebServices.UFCodigo) then
      AdicionaErro('226-Rejeição: Código da UF do Emitente diverge da UF autorizadora');

    GravaLog('Regra: G003 - Validar 247-UF');
    if MDFe.Emit.EnderEmit.UF <> Configuracoes.WebServices.UF then
      AdicionaErro('247-Rejeição: Sigla da UF do Emitente difere da UF do Web Service');

    GravaLog('Regra: G004 - Validar: 227-Chave de acesso');
    if not ValidarConcatChave then
      AdicionaErro('227-Rejeição: Chave de Acesso do Campo Id difere da concatenação dos campos correspondentes');

    GravaLog('Regra: G005 - Validar: 666-Ano da Chave');
    if Copy(MDFe.infMDFe.ID, 7, 2) < '12' then
      AdicionaErro('666-Rejeição: Ano da chave de acesso é inferior a 2012');

    GravaLog('Regra: G018 - Validar: 458-Tipo de Transportador');
    if (Configuracoes.Geral.VersaoDF >= ve300) and (MDFe.Ide.tpTransp <> ttNenhum) and
        (MDFe.Ide.tpEmit = teTranspCargaPropria) and
        (MDFe.Ide.modal = moRodoviario) and ((MDFe.Rodo.veicTracao.Prop.CNPJCPF = '') or
        (MDFe.Rodo.veicTracao.Prop.CNPJCPF = MDFe.emit.CNPJCPF))  then
      AdicionaErro('458-Rejeição: Tipo de transportador (tpTransp) não deve ser preenchido');

    // *************************************************************************
    // No total são 93 regras de validação, portanto faltam muitas para serem
    // acrescentadas nessa rotina.
  end;

  Result := EstaVazio(Erros);

  if not Result then
  begin
    Erros := ACBrStr('Erro(s) nas Regras de negócios do Manifesto: '+
                     IntToStr(MDFe.Ide.nMDF) + sLineBreak + Erros);
  end;

  GravaLog('Fim da Validação. Tempo: ' +
           FormatDateTime('hh:nn:ss:zzz', Now - Agora) + sLineBreak +
           'Erros:' + Erros);

  FErroRegrasdeNegocios := Erros;
end;

function TManifesto.LerXML(const AXML: String): Boolean;
{$IfNDef USE_ACBr_XMLDOCUMENT}
var
  XMLStr: String;
{$EndIf}
begin
  XMLOriginal := AXML;  // SetXMLOriginal() irá verificar se AXML está em UTF8

  {$IfDef USE_ACBr_XMLDOCUMENT}
  FMDFeR.Arquivo := XMLOriginal;
  {$Else}
  { Verifica se precisa converter "AXML" de UTF8 para a String nativa da IDE.
    Isso é necessário, para que as propriedades fiquem com a acentuação correta }
  XMLStr := ParseText(AXML, True, XmlEhUTF8(AXML));

  FMDFeR.Leitor.Arquivo := XMLStr;
  {$EndIf}
  FMDFeR.LerXml;

  Result := True;
end;

function TManifesto.GravarXML(const NomeArquivo: String; const PathArquivo: String): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  FNomeArq := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);

  Result := TACBrMDFe(TManifestos(Collection).ACBrMDFe).Gravar(FNomeArq, FXMLOriginal);
end;

function TManifesto.GravarStream(AStream: TStream): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FXMLOriginal));
  Result := True;
end;

procedure TManifesto.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings; sReplyTo: TStrings;
  ManterPDFSalvo: Boolean; sBCC: TStrings);
var
  NomeArqTemp: string;
  AnexosEmail: TStrings;
  StreamMDFe: TMemoryStream;
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
      Self.GravarStream(StreamMDFe);

      if (EnviaPDF) then
      begin
        if Assigned(DAMDFE) then
        begin
          DAMDFE.ImprimirDAMDFEPDF(FMDFe);
          NomeArqTemp := PathWithDelim(DAMDFE.PathPDF) + NumID + '-mdfe.pdf';
          AnexosEmail.Add(NomeArqTemp);
        end;
      end;

      EnviarEmail( sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamMDFe,
                   NumID + '-mdfe.xml', sReplyTo, sBCC);
    end;
  finally
    if not ManterPDFSalvo then
      DeleteFile(NomeArqTemp);

    AnexosEmail.Free;
    StreamMDFe.Free;
  end;
end;

function TManifesto.GerarMDFeIni: String;
begin
  Result := FMDFeIniW.GravarIni;
end;

function TManifesto.GerarXML: String;
var
  IdAnterior : String;
begin
  with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
  begin
    IdAnterior := MDFe.infMDFe.ID;
{$IfDef USE_ACBr_XMLDOCUMENT}
    FMDFeW.Opcoes.FormatoAlerta  := Configuracoes.Geral.FormatoAlerta;
    FMDFeW.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
    FMDFeW.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
    FMDFeW.Opcoes.IdentarXML := Configuracoes.Geral.IdentarXML;
    FMDFeW.Opcoes.QuebraLinha := Configuracoes.WebServices.QuebradeLinha;
    FMDFeW.Opcoes.NormatizarMunicipios  := Configuracoes.Arquivos.NormatizarMunicipios;
    FMDFeW.Opcoes.PathArquivoMunicipios := Configuracoes.Arquivos.PathArquivoMunicipios;
{$Else}
    FMDFeW.Gerador.Opcoes.FormatoAlerta  := Configuracoes.Geral.FormatoAlerta;
    FMDFeW.Gerador.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
    FMDFeW.Gerador.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
    FMDFeW.Gerador.Opcoes.IdentarXML     := Configuracoes.Geral.IdentarXML;
    FMDFeW.Gerador.Opcoes.QuebraLinha := Configuracoes.WebServices.QuebradeLinha;
    FMDFeW.Opcoes.NormatizarMunicipios   := Configuracoes.Arquivos.NormatizarMunicipios;
    FMDFeW.Opcoes.PathArquivoMunicipios  := Configuracoes.Arquivos.PathArquivoMunicipios;
{$EndIf}

    TimeZoneConf.Assign( Configuracoes.WebServices.TimeZoneConf );
    {
    FMDFeW.VersaoDF := Configuracoes.Geral.VersaoDF;
    FMDFeW.ModeloDF := '58';
    FMDFeW.tpAmb := Configuracoes.WebServices.Ambiente;
    FMDFeW.tpEmis := Configuracoes.Geral.FormaEmissao;
    }
    FMDFeW.idCSRT := Configuracoes.RespTec.IdCSRT;
    FMDFeW.CSRT   := Configuracoes.RespTec.CSRT;
  end;

  FMDFeW.GerarXml;

{$IfDef USE_ACBr_XMLDOCUMENT}
  XMLOriginal := FMDFeW.Document.Xml;  // SetXMLOriginal() irá converter para UTF8
{$Else}
  XMLOriginal := FMDFeW.Gerador.ArquivoFormatoXML;  // SetXMLOriginal() irá converter para UTF8
{$EndIf}


  { XML gerado pode ter nova Chave e ID, então devemos calcular novamente o
    nome do arquivo, mantendo o PATH do arquivo carregado }
  if (NaoEstaVazio(FNomeArq) and (IdAnterior <> FMDFe.infMDFe.ID)) then
    FNomeArq := CalcularNomeArquivoCompleto('', ExtractFilePath(FNomeArq));

{$IfDef USE_ACBr_XMLDOCUMENT}
  FAlertas := FMDFeW.ListaDeAlertas.Text;
{$Else}
  FAlertas := FMDFeW.Gerador.ListaDeAlertas.Text;
{$EndIf}
  Result := FXMLOriginal;
end;

function TManifesto.CalcularNomeArquivo: String;
var
  xID: String;
begin
  xID := Self.NumID;

  if EstaVazio(xID) then
    raise EACBrMDFeException.Create('ID Inválido. Impossível Salvar XML');

  Result := xID + '-mdfe.xml';
end;

function TManifesto.CalcularPathArquivo: String;
var
  Data: TDateTime;
begin
  with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
  begin
    if Configuracoes.Arquivos.EmissaoPathMDFe then
      Data := FMDFe.Ide.dhEmi
    else
      Data := Now;

    Result := PathWithDelim(Configuracoes.Arquivos.GetPathMDFe(Data, FMDFe.Emit.CNPJCPF, FMDFe.Emit.IE));
  end;
end;

function TManifesto.CalcularNomeArquivoCompleto(NomeArquivo: String;
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

function TManifesto.ValidarConcatChave: Boolean;
var
  wAno, wMes, wDia: word;
begin
  DecodeDate(MDFe.ide.dhEmi, wAno, wMes, wDia);

  Result := not
    ((Copy(MDFe.infMDFe.ID, 5, 2) <> IntToStrZero(MDFe.Ide.cUF, 2)) or
    (Copy(MDFe.infMDFe.ID, 7, 2)  <> Copy(FormatFloat('0000', wAno), 3, 2)) or
    (Copy(MDFe.infMDFe.ID, 9, 2)  <> FormatFloat('00', wMes)) or
    (Copy(MDFe.infMDFe.ID, 11, 14)<> PadLeft(OnlyNumber(MDFe.Emit.CNPJCPF), 14, '0')) or
    (Copy(MDFe.infMDFe.ID, 25, 2) <> MDFe.Ide.modelo) or
    (Copy(MDFe.infMDFe.ID, 27, 3) <> IntToStrZero(MDFe.Ide.serie, 3)) or
    (Copy(MDFe.infMDFe.ID, 30, 9) <> IntToStrZero(MDFe.Ide.nMDF, 9)) or
    (Copy(MDFe.infMDFe.ID, 39, 1) <> TpEmisToStr(MDFe.Ide.tpEmis)) or
    (Copy(MDFe.infMDFe.ID, 40, 8) <> IntToStrZero(MDFe.Ide.cMDF, 8)));
end;

function TManifesto.GetConfirmado: Boolean;
begin
  Result := TACBrMDFe(TManifestos(Collection).ACBrMDFe).cStatConfirmado(
    FMDFe.procMDFe.cStat);
end;

function TManifesto.GetcStat: Integer;
begin
  Result := FMDFe.procMDFe.cStat;
end;

function TManifesto.GetProcessado: Boolean;
begin
  Result := TACBrMDFe(TManifestos(Collection).ACBrMDFe).cStatProcessado(
    FMDFe.procMDFe.cStat);
end;

function TManifesto.GetCancelado: Boolean;
begin
  Result := TACBrMDFe(TManifestos(Collection).ACBrMDFe).cStatCancelado(
    FMDFe.procMDFe.cStat);
end;

function TManifesto.GetMsg: String;
begin
  Result := FMDFe.procMDFe.xMotivo;
end;

function TManifesto.GetNumID: String;
begin
  Result := Trim(OnlyNumber(MDFe.infMDFe.ID));
end;

function TManifesto.GetXMLAssinado: String;
begin
  if EstaVazio(FXMLAssinado) then
    Assinar;

  Result := FXMLAssinado;
end;

procedure TManifesto.SetXML(const AValue: String);
begin
  LerXML(AValue);
end;

procedure TManifesto.SetXMLOriginal(const AValue: String);
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

function TManifesto.LerArqIni(const AIniString: String): Boolean;
begin
  FMDFeIniR.VersaoDF := FConfiguracoes.Geral.VersaoDF;
  FMDFeIniR.Ambiente := Integer(FConfiguracoes.WebServices.Ambiente);
  FMDFeIniR.tpEmis := FConfiguracoes.Geral.FormaEmissaoCodigo;

  FMDFeIniR.LerIni(AIniString);

  GerarXML;

  Result := True;
end;

{ TManifestos }

constructor TManifestos.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  if not (AOwner is TACBrMDFe) then
    raise EACBrMDFeException.Create('AOwner deve ser do tipo TACBrMDFe');

  inherited Create(AOwner, ItemClass);

  FACBrMDFe := TACBrMDFe(AOwner);
  FConfiguracoes := TACBrMDFe(FACBrMDFe).Configuracoes;
end;

function TManifestos.Add: TManifesto;
begin
  Result := TManifesto(inherited Add);
end;

procedure TManifestos.Assinar;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self[i].Assinar;
end;

function TManifestos.GerarIni: String;
begin
  Result := '';
  if (Self.Count > 0) then
    Result := Self[0].GerarMDFeIni;
end;

procedure TManifestos.GerarMDFe;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self[i].GerarXML;
end;

function TManifestos.GetItem(Index: integer): TManifesto;
begin
  Result := TManifesto(inherited Items[Index]);
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
  TACBrMDFe(FACBrMDFE).DAMDFE.ImprimirDAMDFEPDF;
end;

procedure TManifestos.ImprimirPDF(AStream: TStream);
begin
  VerificarDAMDFE;
  TACBrMDFe(FACBrMDFE).DAMDFE.ImprimirDAMDFEPDF(AStream);
end;

function TManifestos.Insert(Index: integer): TManifesto;
begin
  Result := TManifesto(inherited Insert(Index));
end;

procedure TManifestos.SetItem(Index: integer; const Value: TManifesto);
begin
  Items[Index].Assign(Value);
end;

procedure TManifestos.Validar;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self[i].Validar;   // Dispara exception em caso de erro
end;

function TManifestos.VerificarAssinatura(out Erros: String): Boolean;
var
  i: integer;
begin
  Result := True;
  Erros := '';

  if Self.Count < 1 then
  begin
    Erros := 'Nenhum MDFe carregado';
    Result := False;
    Exit;
  end;

  for i := 0 to Self.Count - 1 do
  begin
    if not Self[i].VerificarAssinatura then
    begin
      Result := False;
      Erros := Erros + Self[i].ErroValidacao + sLineBreak;
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
    if not Self[i].ValidarRegrasdeNegocios then
    begin
      Result := False;
      Erros := Erros + Self[i].ErroRegrasdeNegocios + sLineBreak;
    end;
  end;
end;

function TManifestos.LoadFromFile(const CaminhoArquivo: String;
  AGerarMDFe: Boolean): Boolean;
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

  l := Self.Count; // Indice do último manifesto já existente
  Result := LoadFromString(String(XMLUTF8), AGerarMDFe);

  if Result then
  begin
    // Atribui Nome do arquivo a novos manifestos inseridos //
    for i := l to Self.Count - 1 do
      Self[i].NomeArq := CaminhoArquivo;
  end;
end;

function TManifestos.LoadFromStream(AStream: TStringStream;
  AGerarMDFe: Boolean): Boolean;
var
  AXML: AnsiString;
begin
  AStream.Position := 0;
  AXML := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(AXML), AGerarMDFe);
end;

function TManifestos.LoadFromString(const AXMLString: String;
  AGerarMDFe: Boolean): Boolean;
var
  AMDFeXML, XMLStr: AnsiString;
  P, N: integer;

  function PosMDFe: integer;
  begin
    Result := pos('</MDFe>', XMLStr);
  end;

begin
  // Verifica se precisa Converter de UTF8 para a String nativa da IDE //
  XMLStr := ConverteXMLtoNativeString(AXMLString);

  N := PosMDFe;
  while N > 0 do
  begin
    P := pos('</mdfeProc>', XMLStr);

    if P <= 0 then
      P := pos('</MDFeProc>', XMLStr);

    if P <= 0 then
      P := pos('</procMDFe>', XMLStr);  // MDFe obtido pelo Portal da Receita

    if P > 0 then
    begin
      AMDFeXML := copy(XMLStr, 1, P + 11);
      XMLStr := Trim(copy(XMLStr, P + 11, length(XMLStr)));
    end
    else
    begin
      AMDFeXML := copy(XMLStr, 1, N + 7);
      XMLStr := Trim(copy(XMLStr, N + 7, length(XMLStr)));
    end;

    with Self.Add do
    begin
      LerXML(AMDFeXML);

      if AGerarMDFe then // Recalcula o XML
        GerarXML;
    end;

    N := PosMDFe;
  end;

  Result := Self.Count > 0;
end;

function TManifestos.LoadFromIni(const AIniString: String): Boolean;
begin
  with Self.Add do
    LerArqIni(AIniString);

  Result := Self.Count > 0;
end;

function TManifestos.GravarXML(const PathNomeArquivo: String): Boolean;
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
    Result := Self[i].GravarXML(NomeArq, PathArq);
    Inc(i);
  end;
end;

end.
