{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
{                              Wemerson Souto                                  }
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

unit ACBrCTeConhecimentos;

interface

uses
  Classes, Sysutils, StrUtils,
  ACBrCTeConfiguracoes,
  ACBrCTe.Classes,
  {$IfDef USE_ACBr_XMLDOCUMENT}
//  ACBrCTe.XmlReader,
  ACBrCTe.XmlHandler,
  ACBrCTe.XmlWriter,
  {$Else}
  pcteCTeR,
  pcteCTeW,
  {$EndIf}
  ACBrCTe.IniReader, ACBrCTe.IniWriter,
  pcnConversao, pcnLeitor;

type

  { Conhecimento }

  Conhecimento = class(TCollectionItem)
  private
    FCTe: TCTe;
    // Xml
    {$IfDef USE_ACBr_XMLDOCUMENT}
    FCTeR: TCTeXmlReader;
    FCTeW: TCTeXmlWriter;
    {$Else}
    FCTeR: TCTeR;
    FCTeW: TCTeW;
    {$EndIf}
    // Ini
    FCTeIniR: TCTeIniReader;
    FCTeIniW: TCTeIniWriter;

    FXMLAssinado: String;
    FXMLOriginal: String;

    FConfiguracoes: TConfiguracoesCTe;
    FAlertas: String;
    FErroValidacao: String;
    FErroValidacaoCompleto: String;
    FErroRegrasdeNegocios: String;
    FNomeArq: String;
    FNomeArqPDF: String;

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
    procedure ImprimirPDF(AStream: TStream); overload;

    procedure Assinar;
    procedure Validar;
    function VerificarAssinatura: Boolean;
    function ValidarRegrasdeNegocios: Boolean;

    function LerXML(const AXML: String): Boolean;
    function LerArqIni(const AIniString: String): Boolean;
    function GerarCTeIni: String;

    function GerarXML: String;
    function GravarXML(const NomeArquivo: String = ''; const PathArquivo: String = ''): Boolean;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings = nil;
      EnviaPDF: Boolean = True; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil);

    function CalcularNomeArquivoCompleto(NomeArquivo: String = '';
      PathArquivo: String = ''): String;

    property NomeArq: String read FNomeArq write FNomeArq;
    property NomeArqPDF: String read FNomeArqPDF write FNomeArqPDF;

    property CTe: TCTe read FCTe;

    // Atribuir a "XML", faz o componente transferir os dados lido para as propriedades internas e "XMLAssinado"
    property XML: String         read FXMLOriginal   write SetXML;
    // Atribuir a "XMLOriginal", reflete em XMLAssinado, se existir a tag de assinatura
    property XMLOriginal: String read FXMLOriginal   write SetXMLOriginal;
    property XMLAssinado: String read GetXMLAssinado write FXMLAssinado;

    property Confirmado: Boolean read GetConfirmado;
    property Processado: Boolean read GetProcessado;
    property Cancelado: Boolean  read GetCancelado;
    property cStat: Integer      read GetcStat;
    property Msg: String         read GetMsg;
    property NumID: String       read GetNumID;

    property Alertas: String               read FAlertas;
    property ErroValidacao: String         read FErroValidacao;
    property ErroValidacaoCompleto: String read FErroValidacaoCompleto;
    property ErroRegrasdeNegocios: String  read FErroRegrasdeNegocios;
  end;

  { TConhecimentos }

  TConhecimentos = class(TOwnedCollection)
  private
    FACBrCTe: TComponent;
    FConfiguracoes: TConfiguracoesCTe;

    function GetItem(Index: Integer): Conhecimento;
    procedure SetItem(Index: Integer; const Value: Conhecimento);
    procedure VerificarDACTE;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    procedure GerarCTe;
    procedure Assinar;
    procedure Validar;
    function VerificarAssinatura(out Erros: String): Boolean;
    function ValidarRegrasdeNegocios(out Erros: String): Boolean;
    procedure Imprimir;
    procedure ImprimirPDF; overload;
    procedure ImprimirPDF(AStream: TStream); overload;

    function  Add: Conhecimento;
    function Insert(Index: Integer): Conhecimento;

    property Items[Index: Integer]: Conhecimento read GetItem write SetItem; default;

    function GetNamePath: String; override;
    // Incluido o Parametro AGerarCTe que determina se após carregar os dados do CTe
    // para o componente, será gerado ou não novamente o XML do CTe.
    function LoadFromFile(const CaminhoArquivo: String; AGerarCTe: Boolean = False): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarCTe: Boolean = False): Boolean;
    function LoadFromString(const AXMLString: String; AGerarCTe: Boolean = False): Boolean;
    function LoadFromIni(const AIniString: String): Boolean;

    function GerarIni: String;
    function GravarXML(const PathNomeArquivo: String = ''): Boolean;

    property ACBrCTe: TComponent read FACBrCTe;
  end;

implementation

uses
  dateutils, IniFiles,
  synautil,
  ACBrCTe,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBrDFeUtil, pcteConversaoCTe;

{ Conhecimento }

constructor Conhecimento.Create(Collection2: TCollection);
begin
  inherited Create(Collection2);

  FCTe := TCTe.Create;
  // Xml
  {$IfDef USE_ACBr_XMLDOCUMENT}
  FCTeR := TCTeXmlReader.Create(FCTe);
  FCTeW := TCTeXmlWriter.Create(FCTe);
  {$Else}
  FCTeR := TCTeR.Create(FCTe);
  FCTeW := TCTeW.Create(FCTe);
  {$EndIf}
  // Ini
  FCTeIniR := TCTeIniReader.Create(FCTe);
  FCTeIniW := TCTeIniWriter.Create(FCTe);

  FConfiguracoes := TACBrCTe(TConhecimentos(Collection).ACBrCTe).Configuracoes;

  FCTe.Ide.tpCTe := tcNormal;
  FCTe.Ide.verProc := 'ACBrCTe';
  FCTe.ide.indGlobalizado := tiNao;
  FCTe.infCTeNorm.infCteSub.indAlteraToma := tiNao;

  with TACBrCTe(TConhecimentos(Collection).ACBrCTe) do
  begin
    FCTe.Ide.modelo := StrToInt(ModeloCTeToStr(Configuracoes.Geral.ModeloDF));
    FCTe.infCTe.Versao := VersaoCTeToDbl(Configuracoes.Geral.VersaoDF);
    FCTe.Ide.tpAmb := Configuracoes.WebServices.Ambiente;
    FCTe.Ide.tpEmis := Configuracoes.Geral.FormaEmissao;

    if Assigned(DACTE) then
      FCTe.Ide.tpImp := DACTE.TipoDACTE;
  end;
end;

destructor Conhecimento.Destroy;
begin
  FCTe.Free;
  // Xml
  FCTeW.Free;
  FCTeR.Free;
  // Ini
  FCTeIniR.Free;
  FCTeIniW.Free;

  inherited Destroy;
end;

procedure Conhecimento.Imprimir;
begin
  with TACBrCTe(TConhecimentos(Collection).ACBrCTe) do
  begin
    if not Assigned(DACTE) then
      raise EACBrCTeException.Create('Componente DACTE não associado.')
    else
      DACTE.ImprimirDACTE(CTe);
  end;
end;

procedure Conhecimento.ImprimirPDF;
begin
  with TACBrCTe(TConhecimentos(Collection).ACBrCTe) do
  begin
    if not Assigned(DACTE) then
      raise EACBrCTeException.Create('Componente DACTE não associado.')
    else
      DACTE.ImprimirDACTEPDF(CTe);
  end;
end;

procedure Conhecimento.ImprimirPDF(AStream: TStream);
begin
  with TACBrCTe(TConhecimentos(Collection).ACBrCTe) do
  begin
    if not Assigned(DACTE) then
      raise EACBrCTeException.Create('Componente DACTE não associado.')
    else
    begin
      AStream.Size := 0;
      DACTE.ImprimirDACTEPDF(AStream, CTe);
    end;
  end;
end;

procedure Conhecimento.Assinar;
var
  XMLStr: String;
  XMLUTF8: AnsiString;
  Leitor: TLeitor;
begin
  with TACBrCTe(TConhecimentos(Collection).ACBrCTe) do
  begin
    if not Assigned(SSL.AntesDeAssinar) then
      SSL.ValidarCNPJCertificado( CTe.Emit.CNPJ );
  end;

  // Gera novamente, para processar propriedades que podem ter sido modificadas
  XMLStr := GerarXML;

  // XML já deve estar em UTF8, para poder ser assinado //
  XMLUTF8 := ConverteXMLtoUTF8(XMLStr);

  with TACBrCTe(TConhecimentos(Collection).ACBrCTe) do
  begin
    case Configuracoes.Geral.ModeloDF of
      moCTeOS: FXMLAssinado := SSL.Assinar(String(XMLUTF8), 'CTeOS', 'infCte');
      moGTVe: FXMLAssinado := SSL.Assinar(String(XMLUTF8), 'GTVe', 'infCte');
      moCTeSimp: FXMLAssinado := SSL.Assinar(String(XMLUTF8), 'CTeSimp', 'infCte');
    else
      FXMLAssinado := SSL.Assinar(String(XMLUTF8), 'CTe', 'infCte')
    end;

    // SSL.Assinar() sempre responde em UTF8...
    FXMLOriginal := FXMLAssinado;

    Leitor := TLeitor.Create;
    try
      leitor.Grupo := FXMLAssinado;
      CTe.signature.URI := Leitor.rAtributo('Reference URI=');
      CTe.signature.DigestValue := Leitor.rCampo(tcStr, 'DigestValue');
      CTe.signature.SignatureValue := Leitor.rCampo(tcStr, 'SignatureValue');
      CTe.signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');
    finally
      Leitor.Free;
    end;

    with TACBrCTe(TConhecimentos(Collection).ACBrCTe) do
    begin
      CTe.infCTeSupl.qrCodCTe := GetURLQRCode(CTe);

      GerarXML;
    end;

    if Configuracoes.Arquivos.Salvar and
      (not Configuracoes.Arquivos.SalvarApenasCTeProcessados) then
    begin
      if NaoEstaVazio(NomeArq) then
        Gravar(NomeArq, FXMLAssinado)
      else
        Gravar(CalcularNomeArquivoCompleto(), FXMLAssinado);
    end;
  end;
end;

procedure Conhecimento.Validar;
var
  Erro, AXML, AXMLModal, Grupo: String;
  CTeEhValido, ModalEhValido, ok: Boolean;
  ALayout: TLayOutCTe;
  Modelo: TModeloCTe;
begin
  AXML := FXMLAssinado;
  if AXML = '' then
    AXML := XMLOriginal;

  // Obtem o grupo referente ao Modal
  AXMLModal := Trim(RetornarConteudoEntre(AXML, '<infModal', '</infModal>'));
  // Remove NameSpace indevido incluido por alguns sistemas
  AXMLModal := StringReplace(AXMLModal, ' xmlns="' + ACBRCTE_NAMESPACE + '"', '',
                                     [rfReplaceAll, rfIgnoreCase]);

  case TACBrCTe(TConhecimentos(Collection).ACBrCTe).IdentificaSchemaModal(AXML) of
   schcteModalAereo:
     begin
       AXMLModal := '<aereo xmlns="' + ACBRCTE_NAMESPACE + '">' +
                      Trim(RetornarConteudoEntre(AXMLModal, '<aereo>', '</aereo>')) +
                    '</aereo>';
     end;
   schcteModalAquaviario:
     begin
       AXMLModal := '<aquav xmlns="' + ACBRCTE_NAMESPACE + '">' +
                      Trim(RetornarConteudoEntre(AXMLModal, '<aquav>', '</aquav>')) +
                    '</aquav>';
     end;
   schcteModalDutoviario:
     begin
       AXMLModal := '<duto xmlns="' + ACBRCTE_NAMESPACE + '">' +
                      Trim(RetornarConteudoEntre(AXMLModal, '<duto>', '</duto>')) +
                    '</duto>';
     end;
   schcteModalFerroviario:
     begin
       AXMLModal := '<ferrov xmlns="' + ACBRCTE_NAMESPACE + '">' +
                      Trim(RetornarConteudoEntre(AXMLModal, '<ferrov>', '</ferrov>')) +
                    '</ferrov>';
     end;
   schcteModalRodoviario:
     begin
       AXMLModal := '<rodo xmlns="' + ACBRCTE_NAMESPACE + '">' +
                      Trim(RetornarConteudoEntre(AXMLModal, '<rodo>', '</rodo>')) +
                    '</rodo>';
     end;
   schcteModalRodoviarioOS:
     begin
       AXMLModal := '<rodoOS xmlns="' + ACBRCTE_NAMESPACE + '">' +
                      Trim(RetornarConteudoEntre(AXMLModal, '<rodoOS>', '</rodoOS>')) +
                    '</rodoOS>';
     end;
   schcteMultiModal:
     begin
       AXMLModal := '<multimodal xmlns="' + ACBRCTE_NAMESPACE + '">' +
                      Trim(RetornarConteudoEntre(AXMLModal, '<multimodal>', '</multimodal>')) +
                    '</multimodal>';
     end;
  end;

  AXMLModal := '<?xml version="1.0" encoding="UTF-8" ?>' + AXMLModal;

  with TACBrCTe(TConhecimentos(Collection).ACBrCTe) do
  begin
    Modelo  := StrToModeloCTe(ok, IntToStr(FCTe.Ide.modelo));

    if FCTe.ide.tpCTe in [tcCTeSimp, tcSubstCTeSimpl] then
      Modelo := moCTeSimp;

    case Modelo of
      moCTeOS:
        begin
          ALayout := LayCTeRecepcaoOS;
          Grupo := 'CTeOS';
        end;
      moGTVe:
        begin
          ALayout := LayCTeRecepcaoGTVe;
          Grupo := 'GTVe';
        end;
      moCTeSimp:
        begin
          ALayout := LayCTeRecepcaoSimp;
          Grupo := 'CTeSimp';
        end;
    else
      begin
        ALayout := LayCTeRecepcao;
        Grupo := 'CTe';
      end;
    end;

    // Extraindo apenas os dados da CTe (sem cteProc)
    AXML := ObterDFeXML(AXML, Grupo, ACBRCTE_NAMESPACE);

    if EstaVazio(AXML) then
    begin
      Erro := ACBrStr(Grupo + ' não encontrada no XML');
      CTeEhValido := False;
    end
    else
    begin
      if (FCTe.ide.tpCTe in [tcNormal, tcSubstituto, tcCTeSimp, tcSubstCTeSimpl]) and
         ((FCTe.ide.modelo = 57) or ((FCTe.ide.modelo = 67) and
          (FCTe.ide.modal = mdRodoviario) and (FCTe.ide.tpServ <> tsTranspValores))) then
      begin
        ModalEhValido := SSL.Validar(AXMLModal, GerarNomeArqSchemaModal(AXML, FCTe.infCTe.Versao), Erro);

        if not ModalEhValido then
        begin
          FErroValidacao := ACBrStr('Falha na validação do Modal do Conhecimento: ') +
            IntToStr(CTe.Ide.nCT) + sLineBreak + FAlertas ;
          FErroValidacaoCompleto := FErroValidacao + sLineBreak + Erro;

          raise EACBrCTeException.CreateDef(
            IfThen(Configuracoes.Geral.ExibirErroSchema, ErroValidacaoCompleto,
            ErroValidacao));
        end;

        CTeEhValido := SSL.Validar(AXML, GerarNomeArqSchema(ALayout, FCTe.infCTe.Versao), Erro);
      end
      else
        CTeEhValido := SSL.Validar(AXML, GerarNomeArqSchema(ALayout, FCTe.infCTe.Versao), Erro);
    end;

    if not CTeEhValido then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados do Conhecimento: ') +
        IntToStr(CTe.Ide.nCT) + sLineBreak + FAlertas ;
      FErroValidacaoCompleto := FErroValidacao + sLineBreak + Erro;

      raise EACBrCTeException.CreateDef(
        IfThen(Configuracoes.Geral.ExibirErroSchema, ErroValidacaoCompleto,
        ErroValidacao));
    end;
  end;
end;

function Conhecimento.VerificarAssinatura: Boolean;
var
  Erro, AXML, Grupo: String;
  AssEhValida, Ok: Boolean;
  Modelo: TModeloCTe;
begin
  AXML := XMLAssinado;

  with TACBrCTe(TConhecimentos(Collection).ACBrCTe) do
  begin
    Modelo := StrToModeloCTe(ok, IntToStr(FCTe.Ide.modelo));

    if FCTe.ide.tpCTe in [tcCTeSimp, tcSubstCTeSimpl] then
      Modelo := moCTeSimp;

    case Modelo of
      moCTeOS:
        Grupo := 'CTeOS';
      moGTVe:
        Grupo := 'GTVe';
      moCTeSimp:
        Grupo := 'CTeSimp';
    else
      Grupo := 'CTe';
    end;

    // Extraindo apenas os dados da CTe (sem cteProc)
    AXML := ObterDFeXML(AXML, Grupo, ACBRCTE_NAMESPACE);

    if EstaVazio(AXML) then
    begin
      Erro := ACBrStr(Grupo + ' não encontrada no XML');
      AssEhValida := False;
    end
    else
      AssEhValida := SSL.VerificarAssinatura(AXML, Erro, 'infCte');

    if not AssEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação da assinatura do Conhecimento: ') +
        IntToStr(CTe.Ide.nCT) + sLineBreak + Erro;
    end;
  end;

  Result := AssEhValida;
end;

function Conhecimento.ValidarRegrasdeNegocios: Boolean;
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
  Agora := DataHoraTimeZoneModoDeteccao( TACBrCTe(TConhecimentos(Collection).ACBrCTe ));   //Converte o DateTime do Sistema para o TimeZone configurado, para evitar divergência de Fuso Horário.
  GravaLog('Inicio da Validação');

  with TACBrCTe(TConhecimentos(Collection).ACBrCTe) do
  begin
    Erros := '';

    GravaLog('Validar: 502-Chave de acesso');
    if not ValidarConcatChave then
      AdicionaErro(
        '502-Rejeição: Erro na Chave de Acesso - Campo Id não corresponde à concatenação dos campos correspondentes');

    GravaLog('Validar: 897-Código do documento: ' + IntToStr(CTe.Ide.nCT));
    if not ValidarCodigoDFe(CTe.Ide.cCT, CTe.Ide.nCT) then
      AdicionaErro('897-Rejeição: Código numérico em formato inválido ');

    GravaLog('Validar: 252-Ambiente');
    if (CTe.Ide.tpAmb <> Configuracoes.WebServices.Ambiente) then
      AdicionaErro('252-Rejeição: Ambiente informado diverge do Ambiente de recebimento '
        + '(Tipo do ambiente do CT-e difere do ambiente do Web Service)');

    GravaLog('Validar: 503-Serie');
    if (CTe.Ide.serie > 889) then
      AdicionaErro('503-Rejeição: Série utilizada fora da faixa permitida no Web Service (0-889)');

    GravaLog('Validar: 226-UF');
    if copy(IntToStr(CTe.Emit.EnderEmit.cMun), 1, 2) <> IntToStr(Configuracoes.WebServices.UFCodigo) then
      AdicionaErro('226-Rejeição: Código da UF do Emitente diverge da UF autorizadora');

  end;

  Result := EstaVazio(Erros);

  if not Result then
  begin
    Erros := ACBrStr('Erro(s) nas Regras de negócios do Conhecimento: '+
                     IntToStr(CTe.Ide.nCT) + sLineBreak + Erros);
  end;

  GravaLog('Fim da Validação. Tempo: ' +
           FormatDateTime('hh:nn:ss:zzz', Now - Agora) + sLineBreak +
           'Erros:' + Erros);

  FErroRegrasdeNegocios := Erros;
end;

function Conhecimento.LerXML(const AXML: String): Boolean;
{$IfNDef USE_ACBr_XMLDOCUMENT}
var
  XMLStr: String;
{$EndIf}
begin
  XMLOriginal := AXML;  // SetXMLOriginal() irá verificar se AXML está em UTF8

  {$IfDef USE_ACBr_XMLDOCUMENT}
  FCTeR.Arquivo := XMLOriginal;
  {$Else}
  { Verifica se precisa converter "AXML" de UTF8 para a String nativa da IDE.
    Isso é necessário, para que as propriedades fiquem com a acentuação correta }
  XMLStr := ParseText(AXML, True, XmlEhUTF8(AXML));

  FCTeR.Leitor.Arquivo := XMLStr;
  {$EndIf}
  FCTeR.LerXml;

  Result := True;
end;

function Conhecimento.GravarXML(const NomeArquivo: String; const PathArquivo: String): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  FNomeArq := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);

  Result := TACBrCTe(TConhecimentos(Collection).ACBrCTe).Gravar(FNomeArq, FXMLOriginal);
end;

function Conhecimento.GravarStream(AStream: TStream): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FXMLOriginal));
  Result := True;
end;

procedure Conhecimento.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings; sReplyTo: TStrings);
var
  NomeArqTemp: String;
  AnexosEmail: TStrings;
  StreamCTe: TMemoryStream;
begin
  if not Assigned(TACBrCTe(TConhecimentos(Collection).ACBrCTe).MAIL) then
    raise EACBrCTeException.Create('Componente ACBrMail não associado');

  AnexosEmail := TStringList.Create;
  StreamCTe := TMemoryStream.Create;
  try
    AnexosEmail.Clear;
    if Assigned(Anexos) then
      AnexosEmail.Assign(Anexos);

    with TACBrCTe(TConhecimentos(Collection).ACBrCTe) do
    begin
      Self.GravarStream(StreamCTe);

      if (EnviaPDF) then
      begin
        if Assigned(DACTE) then
        begin
          DACTE.ImprimirDACTEPDF(CTe);
          NomeArqTemp := PathWithDelim(DACTE.PathPDF) + NumID + '-cte.pdf';
          AnexosEmail.Add(NomeArqTemp);
        end;
      end;

      EnviarEmail( sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamCTe,
                   NumID + '-cte.xml', sReplyTo);
    end;
  finally
    AnexosEmail.Free;
    StreamCTe.Free;
  end;
end;

function Conhecimento.GerarCTeIni: String;
begin
  Result := FCTeIniW.GravarIni;
end;

function Conhecimento.GerarXML: String;
var
  IdAnterior : String;
begin
  with TACBrCTe(TConhecimentos(Collection).ACBrCTe) do
  begin
    IdAnterior := CTe.infCTe.ID;
{$IfDef USE_ACBr_XMLDOCUMENT}
    FCTeW.Opcoes.FormatoAlerta  := Configuracoes.Geral.FormatoAlerta;
    FCTeW.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
    FCTeW.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
    FCTeW.Opcoes.IdentarXML := Configuracoes.Geral.IdentarXML;
    FCTeW.Opcoes.QuebraLinha := Configuracoes.WebServices.QuebradeLinha;
    FCTeW.Opcoes.NormatizarMunicipios  := Configuracoes.Arquivos.NormatizarMunicipios;
    FCTeW.Opcoes.PathArquivoMunicipios := Configuracoes.Arquivos.PathArquivoMunicipios;
{$Else}
    FCTeW.Gerador.Opcoes.FormatoAlerta  := Configuracoes.Geral.FormatoAlerta;
    FCTeW.Gerador.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
    FCTeW.Gerador.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
    FCTeW.Gerador.Opcoes.IdentarXML     := Configuracoes.Geral.IdentarXML;
    FCTeW.Gerador.Opcoes.QuebraLinha    := Configuracoes.WebServices.QuebradeLinha;
    FCTeW.Opcoes.NormatizarMunicipios   := Configuracoes.Arquivos.NormatizarMunicipios;
    FCTeW.Opcoes.PathArquivoMunicipios  := Configuracoes.Arquivos.PathArquivoMunicipios;
{$EndIf}

    TimeZoneConf.Assign( Configuracoes.WebServices.TimeZoneConf );

    {
      Ao gerar o XML as tags e atributos tem que ser exatamente os da configuração
    }
    {
    FCTeW.VersaoDF := Configuracoes.Geral.VersaoDF;
    FCTeW.ModeloDF := Configuracoes.Geral.ModeloDF;
    FCTeW.tpAmb := Configuracoes.WebServices.Ambiente;
    FCTeW.tpEmis := Configuracoes.Geral.FormaEmissao;
    }
    FCTeW.idCSRT := Configuracoes.RespTec.IdCSRT;
    FCTeW.CSRT   := Configuracoes.RespTec.CSRT;
  end;

  FCTeW.GerarXml;

{$IfDef USE_ACBr_XMLDOCUMENT}
  XMLOriginal := FCTeW.Document.Xml;
{$Else}
  XMLOriginal := FCTeW.Gerador.ArquivoFormatoXML;
{$EndIf}

  { XML gerado pode ter nova Chave e ID, então devemos calcular novamente o
    nome do arquivo, mantendo o PATH do arquivo carregado }
  if (NaoEstaVazio(FNomeArq) and (IdAnterior <> FCTe.infCTe.ID)) then
    FNomeArq := CalcularNomeArquivoCompleto('', ExtractFilePath(FNomeArq));

{$IfDef USE_ACBr_XMLDOCUMENT}
  FAlertas := ACBrStr( FCTeW.ListaDeAlertas.Text );
{$Else}
  FAlertas := FCTeW.Gerador.ListaDeAlertas.Text;
{$EndIf}
  Result := FXMLOriginal;
end;

function Conhecimento.CalcularNomeArquivo: String;
var
  xID: String;
begin
  xID := Self.NumID;

  if EstaVazio(xID) then
    raise EACBrCTeException.Create('ID Inválido. Impossível Salvar XML');

  Result := xID + '-cte.xml';
end;

function Conhecimento.CalcularPathArquivo: String;
var
  Data: TDateTime;
begin
  with TACBrCTe(TConhecimentos(Collection).ACBrCTe) do
  begin
    if Configuracoes.Arquivos.EmissaoPathCTe then
      Data := FCTe.Ide.dhEmi
    else
      Data := Now;

    Result := PathWithDelim(Configuracoes.Arquivos.GetPathCTe(Data, FCTe.Emit.CNPJ, FCTe.emit.IE));
  end;
end;

function Conhecimento.CalcularNomeArquivoCompleto(NomeArquivo: String;
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

function Conhecimento.ValidarConcatChave: Boolean;
var
  wAno, wMes, wDia: Word;
begin
  DecodeDate(CTe.ide.dhEmi, wAno, wMes, wDia);
  Result := not
     ((Copy(CTe.infCTe.ID,  4,  2) <> IntToStrZero(CTe.ide.cUF, 2)) or
     (Copy(CTe.infCTe.ID,  6,  2) <> Copy(FormatFloat('0000', wAno), 3, 2)) or
     (Copy(CTe.infCTe.ID,  8,  2) <> FormatFloat('00', wMes)) or
     (Copy(CTe.infCTe.ID, 10, 14) <> copy(OnlyNumber(CTe.Emit.CNPJ) + '00000000000000', 1, 14)) or
     (Copy(CTe.infCTe.ID, 24,  2) <> IntToStr(CTe.ide.modelo)) or
     (Copy(CTe.infCTe.ID, 26,  3) <> IntToStrZero(CTe.ide.serie, 3)) or
     (Copy(CTe.infCTe.ID, 29,  9) <> IntToStrZero(CTe.ide.nCT, 9)) or
     (Copy(CTe.infCTe.ID, 38,  1) <> TpEmisToStr(CTe.ide.tpEmis)) or
     (Copy(CTe.infCTe.ID, 39,  8) <> IntToStrZero(CTe.ide.cCT, 8)));
end;

function Conhecimento.GetConfirmado: Boolean;
begin
  Result := TACBrCTe(TConhecimentos(Collection).ACBrCTe).cStatConfirmado(
    FCTe.procCTe.cStat);
end;

function Conhecimento.GetcStat: Integer;
begin
  Result := FCTe.procCTe.cStat;
end;

function Conhecimento.GetProcessado: Boolean;
begin
  Result := TACBrCTe(TConhecimentos(Collection).ACBrCTe).cStatProcessado(
    FCTe.procCTe.cStat);
end;

function Conhecimento.GetCancelado: Boolean;
begin
  Result := TACBrCTe(TConhecimentos(Collection).ACBrCTe).cStatCancelado(
    FCTe.procCTe.cStat);
end;

function Conhecimento.GetMsg: String;
begin
  Result := FCTe.procCTe.xMotivo;
end;

function Conhecimento.GetNumID: String;
begin
  Result := Trim(OnlyNumber(CTe.infCTe.ID));
end;

function Conhecimento.GetXMLAssinado: String;
begin
  if EstaVazio(FXMLAssinado) then
    Assinar;

  Result := FXMLAssinado;
end;

procedure Conhecimento.SetXML(const AValue: String);
begin
  LerXML(AValue);
end;

procedure Conhecimento.SetXMLOriginal(const AValue: String);
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

function Conhecimento.LerArqIni(const AIniString: String): Boolean;
begin
  FCTeIniR.VersaoDF := FConfiguracoes.Geral.VersaoDF;
  FCTeIniR.Ambiente := Integer(FConfiguracoes.WebServices.Ambiente);
  FCTeIniR.tpEmis := FConfiguracoes.Geral.FormaEmissaoCodigo;

  FCTeIniR.LerIni(AIniString);

  case FCTe.Ide.modelo of
    57:
      begin
        if FCTe.ide.tpCTe in [tcCTeSimp, tcSubstCTeSimpl] then
          FConfiguracoes.Geral.ModeloDF := moCTeSimp
        else
          FConfiguracoes.Geral.ModeloDF := moCTe;
      end;
    64: FConfiguracoes.Geral.ModeloDF := moGTVe;
    67: FConfiguracoes.Geral.ModeloDF := moCTeOS;
  end;

  GerarXML;

  Result := True;
end;

{ TConhecimentos }

constructor TConhecimentos.Create(AOwner: TPersistent;
  ItemClass: TCollectionItemClass);
begin
  if not (AOwner is TACBrCTe) then
     raise EACBrCTeException.Create('AOwner deve ser do tipo TACBrCTe');

  inherited Create(AOwner, ItemClass);

  FACBrCTe := TACBrCTe(AOwner);
  FConfiguracoes := TACBrCTe(FACBrCTe).Configuracoes;
end;

function TConhecimentos.Add: Conhecimento;
begin
  Result := Conhecimento(inherited Add);
end;

procedure TConhecimentos.Assinar;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
    Self.Items[i].Assinar;
end;

procedure TConhecimentos.GerarCTe;
var
 i: Integer;
begin
 for i:= 0 to Self.Count-1 do
   Self.Items[i].GerarXML;
end;

function TConhecimentos.GerarIni: String;
begin
  Result := '';
  if (Self.Count > 0) then
    Result := Self.Items[0].GerarCTeIni;
end;

function TConhecimentos.GetItem(Index: Integer): Conhecimento;
begin
  Result := Conhecimento(inherited Items[Index]);
end;

function TConhecimentos.GetNamePath: String;
begin
  Result := 'Conhecimento';
end;

procedure TConhecimentos.VerificarDACTE;
begin
  if not Assigned(TACBrCTe(FACBrCTe).DACTE) then
    raise EACBrCTeException.Create('Componente DACTE não associado.');
end;

procedure TConhecimentos.Imprimir;
begin
  VerificarDACTE;
  TACBrCTe(FACBrCTe).DACTe.ImprimirDACTe(nil);
end;

procedure TConhecimentos.ImprimirPDF;
begin
  VerificarDACTE;
  TACBrCTe(FACBrCTe).DACTe.ImprimirDACTePDF;
end;

procedure TConhecimentos.ImprimirPDF(AStream: TStream);
begin
  VerificarDACTE;
  TACBrCTe(FACBrCTe).DACTE.ImprimirDACTePDF(AStream);
end;

function TConhecimentos.Insert(Index: Integer): Conhecimento;
begin
  Result := Conhecimento(inherited Insert(Index));
end;

procedure TConhecimentos.SetItem(Index: Integer; const Value: Conhecimento);
begin
  Items[Index].Assign(Value);
end;

procedure TConhecimentos.Validar;
var
  i: Integer;
begin
  for i:= 0 to Self.Count-1 do
    Self.Items[i].Validar;
end;

function TConhecimentos.VerificarAssinatura(out Erros: String): Boolean;
var
  i: integer;
begin
  Result := True;
  Erros := '';

  if Self.Count < 1 then
  begin
    Erros := 'Nenhum CTe carregado';
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

function TConhecimentos.ValidarRegrasdeNegocios(out Erros: String): Boolean;
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

function TConhecimentos.LoadFromFile(const CaminhoArquivo: String;
  AGerarCTe: Boolean): Boolean;
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
  Result := LoadFromString(String(XMLUTF8), AGerarCTe);

  if Result then
  begin
    // Atribui Nome do arquivo a novos Conhecimentos inseridos //
    for i := l to Self.Count - 1 do
      Self.Items[i].NomeArq := CaminhoArquivo;
  end;
end;

function TConhecimentos.LoadFromStream(AStream: TStringStream;
  AGerarCTe: Boolean): Boolean;
var
  AXML: AnsiString;
begin
  AStream.Position := 0;
  AXML := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(AXML), AGerarCTe);
end;

function TConhecimentos.LoadFromString(const AXMLString: String;
  AGerarCTe: Boolean): Boolean;
var
  ACTeXML, XMLStr: AnsiString;
  P, N: integer;
  Modelo: TModeloCTe;

  function PosCTe: integer;
  begin
    case Modelo of
      moCTeOS: Result := Pos('</CTeOS>', XMLStr);
      moGTVe: Result := Pos('</GTVe>', XMLStr);
      moCTeSimp: Result := Pos('</CTeSimp>', XMLStr);
    else
      Result := pos('</CTe>', XMLStr);
    end;
  end;

begin
  // Verifica se precisa Converter de UTF8 para a String nativa da IDE //
  XMLStr := ConverteXMLtoNativeString(AXMLString);

  if Pos('</CTeOS>', XMLStr) > 0 then
    Modelo := moCTeOS
  else
  begin
    if Pos('</GTVe>', XMLStr) > 0 then
      Modelo := moGTVe
    else
    begin
      if Pos('</CTeSimp>', XMLStr) > 0 then
        Modelo := moCTeSimp
      else
        Modelo := moCTe;
    end;
  end;

  N := PosCTe;
  while N > 0 do
  begin
    case Modelo of
      moCTeOS:
        begin
          P := pos('</cteOSProc>', XMLStr);

          if P <= 0 then
            P := pos('</procCTeOS>', XMLStr);  // CTe obtido pelo Portal da Receita

          if P > 0 then
          begin
            ACTeXML := copy(XMLStr, 1, P + 12);
            XMLStr := Trim(copy(XMLStr, P + 12, length(XMLStr)));
          end
          else
          begin
            ACTeXML := copy(XMLStr, 1, N + 8);
            XMLStr := Trim(copy(XMLStr, N + 8, length(XMLStr)));
          end;
        end;

      moGTVe:
        begin
          P := pos('</GTVeProc>', XMLStr);

          if P <= 0 then
            P := pos('</procGTVe>', XMLStr);  // CTe obtido pelo Portal da Receita

          if P > 0 then
          begin
            ACTeXML := copy(XMLStr, 1, P + 12);
            XMLStr := Trim(copy(XMLStr, P + 12, length(XMLStr)));
          end
          else
          begin
            ACTeXML := copy(XMLStr, 1, N + 8);
            XMLStr := Trim(copy(XMLStr, N + 8, length(XMLStr)));
          end;
        end;

      moCTeSimp:
        begin
          P := pos('</cteSimpProc>', XMLStr);

          if P <= 0 then
            P := pos('</procCTeSimp>', XMLStr);  // CTe obtido pelo Portal da Receita

          if P > 0 then
          begin
            ACTeXML := copy(XMLStr, 1, P + 14);
            XMLStr := Trim(copy(XMLStr, P + 14, length(XMLStr)));
          end
          else
          begin
            ACTeXML := copy(XMLStr, 1, N + 9);
            XMLStr := Trim(copy(XMLStr, N + 9, length(XMLStr)));
          end;
        end;
    else
      begin
        P := pos('</cteProc>', XMLStr);

        if P <= 0 then
          P := pos('</procCTe>', XMLStr);  // CTe obtido pelo Portal da Receita

        if P > 0 then
        begin
          ACTeXML := copy(XMLStr, 1, P + 10);
          XMLStr := Trim(copy(XMLStr, P + 10, length(XMLStr)));
        end
        else
        begin
          ACTeXML := copy(XMLStr, 1, N + 6);
          XMLStr := Trim(copy(XMLStr, N + 6, length(XMLStr)));
        end;
      end;
    end;

    with Self.Add do
    begin
      LerXML(ACTeXML);

      if AGerarCTe then // Recalcula o XML
        GerarXML;
    end;

    N := PosCTe;
  end;

  Result := Self.Count > 0;
end;

function TConhecimentos.LoadFromIni(const AIniString: String): Boolean;
begin
  with Self.Add do
    LerArqIni(AIniString);

  Result := Self.Count > 0;
end;

function TConhecimentos.GravarXML(const PathNomeArquivo: String): Boolean;
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
