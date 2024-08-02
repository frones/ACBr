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

unit ACBrMDFeManifestos;

interface

uses
  Classes, 
  SysUtils, 
  StrUtils,
  ACBrMDFeConfiguracoes,
  pmdfeMDFe, 
  pmdfeMDFeW,
  {$IfDef USE_ACBr_XMLDOCUMENT}
  ACBrMDFe.XmlReader,
  {$Else}
  pmdfeMDFeR,
  {$EndIf}
  pcnConversao,
  pcnLeitor;

type

  { TManifesto }

  TManifesto = class(TCollectionItem)
  private
    FMDFe: TMDFe;
    FMDFeW: TMDFeW;
    {$IfDef USE_ACBr_XMLDOCUMENT}
    FMDFeR: TMDFeXmlReader;
    {$Else}
    FMDFeR: TMDFeR;
    {$EndIf}

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
      sReplyTo: TStrings = nil);

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
  FMDFeW := TMDFeW.Create(FMDFe);
  {$IfDef USE_ACBr_XMLDOCUMENT}
  FMDFeR := TMDFeXmlReader.Create(FMDFe);
  {$Else}
  FMDFeR := TMDFeR.Create(FMDFe);
  {$EndIf}

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
  FMDFeW.Free;
  FMDFeR.Free;
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

    // Gera o QR-Code para adicionar no XML após ter a
    // assinatura, e antes de ser salvo.

//    with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
//    begin
      MDFe.infMDFeSupl.qrCodMDFe := GetURLQRCode(MDFe.Ide.cUF, MDFe.Ide.tpAmb,
                MDFe.ide.tpEmis, MDFe.infMDFe.ID, MDFe.infMDFe.Versao);

      GerarXML;
//    end;

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
  EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings; sReplyTo: TStrings);
var
  NomeArqTemp : String;
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
                   NumID + '-mdfe.xml', sReplyTo);
    end;
  finally
    AnexosEmail.Free;
    StreamMDFe.Free;
  end;
end;

function TManifesto.GerarMDFeIni: String;
var
  i, j, k, l, m: integer;
  sSecao: string;
  INIRec: TMemIniFile;
  IniDFe: TStringList;
begin
  Result := '';

  if not ValidarChave(MDFe.infMDFe.ID) then
    raise EACBrMDFeException.Create('MDFe Inconsistente para gerar INI. Chave Inválida.');

  INIRec := TMemIniFile.Create('');
  try
    with FMDFe do
    begin
      INIRec.WriteInteger('ide', 'cUF', Ide.cUF);
      INIRec.WriteString('ide', 'tpEmit', TpEmitenteToStr(Ide.tpEmit));
      INIRec.WriteString('ide', 'mod', Ide.modelo);
      INIRec.WriteInteger('ide', 'serie', Ide.serie);
      INIRec.WriteInteger('ide', 'nMDF', Ide.nMDF);
      INIRec.WriteInteger('ide', 'cMDF', Ide.cMDF);
      INIRec.WriteString('ide', 'modal', ModalToStr(Ide.modal));
      INIRec.WriteString('ide', 'dhEmi', DateToStr(Ide.dhEmi));
      INIRec.WriteString('ide', 'tpEmis', TpEmisToStr(Ide.tpEmis));
      INIRec.WriteString('ide', 'procEmi', procEmiToStr(Ide.procEmi));
      INIRec.WriteString('ide', 'verProc', Ide.verProc);
      INIRec.WriteString('ide', 'UFIni', Ide.UFIni);
      INIRec.WriteString('ide', 'UFFim', Ide.UFFim);
      INIRec.WriteString('ide', 'dhIniViagem', DateToStr(Ide.dhIniViagem));
      INIRec.WriteString('ide', 'tpTransp', TTransportadorToStr(Ide.tpTransp));
      INIRec.WriteString('ide', 'indCanalVerde', TindicadorToStr(Ide.indCanalVerde));
      INIRec.WriteString('ide', 'indCarregaPosterior', TindicadorToStr(Ide.indCarregaPosterior));

      for i := 0 to ide.infMunCarrega.Count - 1 do
      begin
        sSecao := 'CARR' + IntToStrZero(I + 1, 3);

        INIRec.WriteInteger(sSecao, 'cMunCarrega', ide.infMunCarrega[i].cMunCarrega);
        INIRec.WriteString(sSecao, 'xMunCarrega', ide.infMunCarrega[i].xMunCarrega);
      end;

      for i := 0 to ide.infPercurso.Count - 1 do
      begin
        sSecao := 'PERC' + IntToStrZero(I + 1, 3);

        INIRec.WriteString(sSecao, 'UFPer', ide.infPercurso[i].UFPer);
      end;

      INIRec.WriteString('emit', 'CNPJ', Emit.CNPJCPF);
      INIRec.WriteString('emit', 'IE', Emit.IE);
      INIRec.WriteString('emit', 'xNome', Emit.xNome);
      INIRec.WriteString('emit', 'xFant', Emit.xFant);
      INIRec.WriteString('emit', 'xLgr', Emit.enderEmit.xLgr);
      INIRec.WriteString('emit', 'nro', Emit.enderEmit.nro);
      INIRec.WriteString('emit', 'xCpl', Emit.enderEmit.xCpl);
      INIRec.WriteString('emit', 'xBairro', Emit.enderEmit.xBairro);
      INIRec.WriteInteger('emit', 'cMun', Emit.enderEmit.cMun);
      INIRec.WriteString('emit', 'xMun', Emit.enderEmit.xMun);
      INIRec.WriteInteger('emit', 'CEP', Emit.enderEmit.CEP);
      INIRec.WriteString('emit', 'UF', Emit.enderEmit.UF);
      INIRec.WriteString('emit', 'fone', Emit.enderEmit.fone);
      INIRec.WriteString('emit', 'email', Emit.enderEmit.email);

      case Ide.modal of
        moRodoviario:
        begin
          INIRec.WriteString('Rodo', 'codAgPorto', Rodo.codAgPorto);

          INIRec.WriteString('infANTT', 'RNTRC', Rodo.infANTT.RNTRC);

          for i := 0 to rodo.infANTT.infCIOT.Count - 1 do
          begin
            sSecao := 'infCIOT' + IntToStrZero(I + 1, 3);

            INIRec.WriteString(sSecao, 'CIOT', rodo.infANTT.infCIOT[i].CIOT);
            INIRec.WriteString(sSecao, 'CNPJCPF', rodo.infANTT.infCIOT[i].CNPJCPF);
          end;

          INIRec.WriteString('valePed', 'categCombVeic', categCombVeicToStr(Rodo.infANTT.valePed.categCombVeic));

          for i := 0 to rodo.infANTT.valePed.disp.Count - 1 do
          begin
            sSecao := 'disp' + IntToStrZero(I + 1, 3);

            INIRec.WriteString(sSecao, 'CNPJForn', rodo.infANTT.valePed.disp[i].CNPJForn);
            INIRec.WriteString(sSecao, 'CNPJPg', rodo.infANTT.valePed.disp[i].CNPJPg);
            INIRec.WriteString(sSecao, 'nCompra', rodo.infANTT.valePed.disp[i].nCompra);
            INIRec.WriteFloat(sSecao, 'vValePed', rodo.infANTT.valePed.disp[i].vValePed);
            INIRec.WriteString(sSecao, 'tpValePed', tpValePedToStr(rodo.infANTT.valePed.disp[i].tpValePed));
          end;

          for i := 0 to rodo.infANTT.infContratante.Count - 1 do
          begin
            sSecao := 'infContratante' + IntToStrZero(I + 1, 3);

            INIRec.WriteString(sSecao, 'CNPJCPF', rodo.infANTT.infContratante[i].CNPJCPF);
            INIRec.WriteString(sSecao, 'idEstrangeiro', rodo.infANTT.infContratante[i].idEstrangeiro);
            INIRec.WriteString(sSecao, 'xNome', rodo.infANTT.infContratante[i].xNome);

            if (rodo.infANTT.infContratante[i].infContrato.NroContrato <> '') and
               (rodo.infANTT.infContratante[i].infContrato.vContratoGlobal <> 0) then
            begin
              INIRec.WriteString(sSecao, 'NroContrato', rodo.infANTT.infContratante[i].infContrato.NroContrato);
              INIRec.WriteFloat(sSecao, 'vContratoGlobal', rodo.infANTT.infContratante[i].infContrato.vContratoGlobal);
            end;
          end;

          for i := 0 to rodo.infANTT.infPag.Count - 1 do
          begin
            sSecao := 'infPag' + IntToStrZero(I + 1, 3);

            INIRec.WriteString(sSecao, 'xNome', rodo.infANTT.infPag[i].xNome);
            INIRec.WriteString(sSecao, 'idEstrangeiro', rodo.infANTT.infPag[i].idEstrangeiro);
            INIRec.WriteString(sSecao, 'CNPJCPF', rodo.infANTT.infPag[i].CNPJCPF);
            INIRec.WriteFloat(sSecao, 'vContrato', rodo.infANTT.infPag[i].vContrato);
            INIRec.WriteString(sSecao, 'indAltoDesemp', indAltoDesempToStr(rodo.infANTT.infPag[i].indAltoDesemp));
            INIRec.WriteString(sSecao, 'indPag', TindPagToStr(rodo.infANTT.infPag[i].indPag));
            INIRec.WriteFloat(sSecao, 'vAdiant', rodo.infANTT.infPag[i].vAdiant);
            INIRec.WriteString(sSecao, 'indAntecipaAdiant', TIndicadorToStr(rodo.infANTT.infPag[i].indAntecipaAdiant));
            INIRec.WriteString(sSecao, 'tpAntecip', tpAntecipToStr(rodo.infANTT.infPag[i].tpAntecip));

            for j := 0 to rodo.infANTT.infPag[I].Comp.Count - 1 do
            begin
              sSecao := 'Comp' + IntToStrZero(I + 1, 3) + IntToStrZero(J + 1, 3);

              INIRec.WriteString(sSecao, 'tpComp', TCompToStr(rodo.infANTT.infPag[i].Comp[j].tpComp));
              INIRec.WriteFloat(sSecao, 'vComp', rodo.infANTT.infPag[i].Comp[j].vComp);
              INIRec.WriteString(sSecao, 'xComp', rodo.infANTT.infPag[i].Comp[j].xComp);
            end;

            for j := 0 to rodo.infANTT.infPag[I].infPrazo.Count - 1 do
            begin
              sSecao := 'infPrazo' + IntToStrZero(I + 1, 3) + IntToStrZero(J + 1, 3);

              INIRec.WriteInteger(sSecao, 'nParcela', rodo.infANTT.infPag[i].infPrazo[j].nParcela);
              INIRec.WriteString(sSecao, 'dVenc', DateToStr(rodo.infANTT.infPag[i].infPrazo[j].dVenc));
              INIRec.WriteFloat(sSecao, 'vParcela', rodo.infANTT.infPag[i].infPrazo[j].vParcela);
            end;

            sSecao := 'infBanc' + IntToStrZero(I, 3);

            INIRec.WriteString(sSecao, 'PIX', rodo.infANTT.infPag[i].infBanc.PIX);
            INIRec.WriteString(sSecao, 'CNPJIPEF', rodo.infANTT.infPag[i].infBanc.CNPJIPEF);
            INIRec.WriteString(sSecao, 'codBanco', rodo.infANTT.infPag[i].infBanc.codBanco);
            INIRec.WriteString(sSecao, 'codAgencia', rodo.infANTT.infPag[i].infBanc.codAgencia);
          end;

          if (Rodo.veicTracao.placa <> '') then
          begin
            INIRec.WriteString('veicTracao', 'clInt', Rodo.veicTracao.cInt);
            INIRec.WriteString('veicTracao', 'placa', Rodo.veicTracao.placa);
            INIRec.WriteString('veicTracao', 'RENAVAN', Rodo.veicTracao.RENAVAM);
            INIRec.WriteInteger('veicTracao', 'tara', Rodo.veicTracao.tara);
            INIRec.WriteInteger('veicTracao', 'capKG', Rodo.veicTracao.capKG);
            INIRec.WriteInteger('veicTracao', 'capM3', Rodo.veicTracao.capM3);
            INIRec.WriteString('veicTracao', 'tpRod', TpRodadoToStr(Rodo.veicTracao.tpRod));
            INIRec.WriteString('veicTracao', 'tpCar', TpCarroceriaToStr(Rodo.veicTracao.tpCar));
            INIRec.WriteString('veicTracao', 'UF', Rodo.veicTracao.UF);
            // Dados do proprietário do veículo de Tração (Opcional) - Nível 2
            INIRec.WriteString('veicTracao', 'CNPJCPF', Rodo.veicTracao.prop.CNPJCPF);
            INIRec.WriteString('veicTracao', 'RNTRC', Rodo.veicTracao.prop.RNTRC);
            INIRec.WriteString('veicTracao', 'xNome', Rodo.veicTracao.prop.xNome);
            INIRec.WriteString('veicTracao', 'IE', Rodo.veicTracao.prop.IE);
            INIRec.WriteString('veicTracao', 'UFProp', Rodo.veicTracao.prop.UF);
            INIRec.WriteString('veicTracao', 'tpProp', TpPropToStr(Rodo.veicTracao.prop.tpProp));
          end;

          for i := 0 to Rodo.veicTracao.condutor.Count - 1 do
          begin
            sSecao := 'moto' + IntToStrZero(i + 1, 3);

            IniRec.WriteString(sSecao, 'CPF', rodo.veicTracao.condutor[i].CPF);
            IniRec.WriteString(sSecao, 'xNome', rodo.veicTracao.condutor[i].xNome);
          end;

          for i := 0 to Rodo.veicReboque.Count - 1 do
          begin
            sSecao := 'reboque' + IntToStrZero(i + 1, 2);

            INIRec.WriteString(sSecao, 'clInt', rodo.veicReboque[i].cInt);
            INIRec.WriteString(sSecao, 'placa', rodo.veicReboque[i].placa);
            INIRec.WriteString(sSecao, 'RENAVAN', rodo.veicReboque[i].RENAVAM);
            INIRec.WriteInteger(sSecao, 'tara', rodo.veicReboque[i].tara);
            INIRec.WriteInteger(sSecao, 'capKG', rodo.veicReboque[i].capKG);
            INIRec.WriteInteger(sSecao, 'capM3', rodo.veicReboque[i].capM3);
            INIRec.WriteString(sSecao, 'tpCar', TpCarroceriaToStr(rodo.veicReboque[i].tpCar));
            INIRec.WriteString(sSecao, 'UF', rodo.veicReboque[i].UF);
            // Dados do proprietário do veículo de Tração (Opcional) - Nível 2
            INIRec.WriteString(sSecao, 'CNPJCPF', rodo.veicReboque[i].prop.CNPJCPF);
            INIRec.WriteString(sSecao, 'RNTRC', rodo.veicReboque[i].prop.RNTRC);
            INIRec.WriteString(sSecao, 'xNome', rodo.veicReboque[i].prop.xNome);
            INIRec.WriteString(sSecao, 'IE', rodo.veicReboque[i].prop.IE);
            INIRec.WriteString(sSecao, 'UFProp', rodo.veicReboque[i].prop.UF);
            INIRec.WriteString(sSecao, 'tpProp', TpPropToStr(rodo.veicReboque[i].prop.tpProp));
          end;

          for i := 0 to Rodo.lacRodo.Count - 1 do
          begin
            sSecao := 'lacRodo' + IntToStrZero(i + 1, 3);

            INIRec.WriteString(sSecao, 'nLacre', rodo.lacRodo[i].nLacre);
          end;
        end;

        moAereo:
        begin
          sSecao := 'aereo';
          INIRec.WriteString(sSecao, 'matr', Aereo.matr);
          INIRec.WriteString(sSecao, 'nVoo', Aereo.nVoo);
          INIRec.WriteString(sSecao, 'cAerEmb', Aereo.cAerEmb);
          INIRec.WriteString(sSecao, 'cAerDes', Aereo.cAerDes);
          INIRec.WriteString(sSecao, 'dVoo', DateToStr(Aereo.dVoo));
        end;

        moAquaviario:
        begin
          sSecao := 'aquav';
          INIRec.WriteString(sSecao, 'CNPJAgeNav', Aquav.CNPJAgeNav);
          INIRec.WriteString(sSecao, 'irin', Aquav.irin);
          INIRec.WriteString(sSecao, 'tpEmb', Aquav.tpEmb);
          INIRec.WriteString(sSecao, 'cEmbar', Aquav.cEmbar);
          INIRec.WriteString(sSecao, 'xEmbar', Aquav.xEmbar);
          INIRec.WriteString(sSecao, 'cPrtEmb', Aquav.cPrtEmb);
          INIRec.WriteString(sSecao, 'cPrtDest', Aquav.cPrtDest);
          INIRec.WriteString(sSecao, 'prtTrans', Aquav.prtTrans);
          INIRec.WriteString(sSecao, 'tpNav', TpNavegacaoToStr(Aquav.tpNav));

          for i := 0 to Aquav.infTermCarreg.Count - 1 do
          begin
            sSecao := 'infTermCarreg' + IntToStrZero(i + 1, 1);

            INIRec.WriteString(sSecao, 'cTermCarreg', Aquav.infTermCarreg[i].cTermCarreg);
            INIRec.WriteString(sSecao, 'xTermCarreg', Aquav.infTermCarreg[i].xTermCarreg);
          end;

          for i := 0 to Aquav.infTermDescarreg.Count - 1 do
          begin
            sSecao := 'infTermDescarreg' + IntToStrZero(i + 1, 1);

            INIRec.WriteString(sSecao, 'cTermDescarreg', Aquav.infTermDescarreg[i].cTermDescarreg);
            INIRec.WriteString(sSecao, 'xTermDescarreg', Aquav.infTermDescarreg[i].xTermDescarreg);
          end;

          for i := 0 to Aquav.infEmbComb.Count - 1 do
          begin
            sSecao := 'infEmbComb' + IntToStrZero(i + 1, 2);

            INIRec.WriteString(sSecao, 'cEmbComb', Aquav.infEmbComb[i].cEmbComb);
            INIRec.WriteString(sSecao, 'xBalsa', Aquav.infEmbComb[i].xBalsa);
          end;

          for i := 0 to Aquav.infUnidCargaVazia.Count - 1 do
          begin
            sSecao := 'infUnidCargaVazia' + IntToStrZero(i + 1, 3);

            INIRec.WriteString(sSecao, 'idUnidCargaVazia', Aquav.infUnidCargaVazia[i].idUnidCargaVazia);
            INIRec.WriteString(sSecao, 'tpUnidCargaVazia', UnidCargaToStr(Aquav.infUnidCargaVazia[i].tpUnidCargaVazia));
          end;

          for i := 0 to Aquav.infUnidTranspVazia.Count - 1 do
          begin
            sSecao := 'infUnidTranspVazia' + IntToStrZero(i + 1, 3);

            INIRec.WriteString(sSecao, 'idUnidTranspVazia', Aquav.infUnidTranspVazia[i].idUnidTranspVazia);
            INIRec.WriteString(sSecao, 'tpUnidTranspVazia', UnidTranspToStr(Aquav.infUnidTranspVazia[i].tpUnidTranspVazia));
          end;
        end;

        moFerroviario:
        begin
          sSecao := 'ferrov';
          INIRec.WriteString(sSecao, 'xPref', Ferrov.xPref);
          INIRec.WriteString(sSecao, 'dhTrem', DateToStr(Ferrov.dhTrem));
          INIRec.WriteString(sSecao, 'xOri', Ferrov.xOri);
          INIRec.WriteString(sSecao, 'xDest', Ferrov.xDest);
          INIRec.WriteInteger(sSecao, 'qVag', Ferrov.qVag);

          for i := 0 to Ferrov.vag.Count - 1 do
          begin
            sSecao := 'vag' + IntToStrZero(i + 1, 3);

            INIRec.WriteString(sSecao, 'serie', Ferrov.vag[i].serie);
            INIRec.WriteInteger(sSecao, 'nVag', Ferrov.vag[i].nVag);
            INIRec.WriteInteger(sSecao, 'nSeq', Ferrov.vag[i].nSeq);
            INIRec.WriteFloat(sSecao, 'TU', Ferrov.vag[i].TU);
            INIRec.WriteFloat(sSecao, 'pesoBC', Ferrov.vag[i].pesoBC);
            INIRec.WriteFloat(sSecao, 'pesoR', Ferrov.vag[i].pesoR);
            INIRec.WriteString(sSecao, 'tpVag', Ferrov.vag[i].tpVag);
          end;
        end;
      end;

      for i := 0 to infDoc.infMunDescarga.Count - 1 do
      begin
        sSecao := 'DESC' + IntToStrZero(I + 1, 3);

        INIRec.WriteInteger(sSecao, 'cMunDescarga', infDoc.infMunDescarga[i].cMunDescarga);
        INIRec.WriteString(sSecao, 'xMunDescarga', infDoc.infMunDescarga[i].xMunDescarga);

        for j := 0 to infDoc.infMunDescarga[i].infCTe.Count - 1 do
        begin
          sSecao := 'infCTe' + IntToStrZero(I + 1, 3) + IntToStrZero(j + 1, 3);

          INIRec.WriteString(sSecao, 'chCTe', infDoc.infMunDescarga[i].infCTe[j].chCTe);
          INIRec.WriteString(sSecao, 'SegCodBarra', infDoc.infMunDescarga[i].infCTe[j].SegCodBarra);
          INIRec.WriteString(sSecao, 'indReentrega', infDoc.infMunDescarga[i].infCTe[j].indReentrega);

          for k := 0 to infDoc.infMunDescarga[i].infCTe[j].peri.Count - 1 do
          begin
            sSecao := 'peri' + IntToStrZero(I + 1, 3) +
                            IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3);

            INIRec.WriteString(sSecao, 'nONU', infDoc.infMunDescarga[i].infCTe[j].peri[k].nONU);
            INIRec.WriteString(sSecao, 'xNomeAE', infDoc.infMunDescarga[i].infCTe[j].peri[k].xNomeAE);
            INIRec.WriteString(sSecao, 'xClaRisco', infDoc.infMunDescarga[i].infCTe[j].peri[k].xClaRisco);
            INIRec.WriteString(sSecao, 'grEmb', infDoc.infMunDescarga[i].infCTe[j].peri[k].grEmb);
            INIRec.WriteString(sSecao, 'qTotProd', infDoc.infMunDescarga[i].infCTe[j].peri[k].qTotProd);
            INIRec.WriteString(sSecao, 'qVolTipo', infDoc.infMunDescarga[i].infCTe[j].peri[k].qVolTipo);
          end;

          sSecao := 'infEntregaParcial' + IntToStrZero(I + 1, 3) + IntToStrZero(j + 1, 3);

          INIRec.WriteFloat(sSecao, 'qtdTotal', infDoc.infMunDescarga[i].infCTe[j].infEntregaParcial.qtdTotal);
          INIRec.WriteFloat(sSecao, 'qtdParcial', infDoc.infMunDescarga[i].infCTe[j].infEntregaParcial.qtdParcial);

          for k := 0 to infDoc.infMunDescarga[i].infCTe[j].infUnidTransp.Count - 1 do
          begin
            sSecao := 'infUnidTransp' + IntToStrZero(I + 1, 3) +
                            IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3);

            INIRec.WriteString(sSecao, 'idUnidTransp', infDoc.infMunDescarga[i].infCTe[j].infUnidTransp[k].idUnidTransp);
            INIRec.WriteFloat(sSecao, 'qtdRat', infDoc.infMunDescarga[i].infCTe[j].infUnidTransp[k].qtdRat);
            INIRec.WriteString(sSecao, 'tpUnidTransp', UnidTranspToStr(infDoc.infMunDescarga[i].infCTe[j].infUnidTransp[k].tpUnidTransp));

            for l := 0 to infDoc.infMunDescarga[i].infCTe[j].infUnidTransp[k].lacUnidTransp.Count - 1 do
            begin
              sSecao := 'lacUnidTransp' + IntToStrZero(I + 1, 3) +
                         IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3) +
                         IntToStrZero(l + 1, 3);

              INIRec.WriteString(sSecao, 'nLacre', infDoc.infMunDescarga[i].infCTe[j].infUnidTransp[k].lacUnidTransp[l].nLacre);
            end;

            for l := 0 to infDoc.infMunDescarga[i].infCTe[j].infUnidTransp[k].infUnidCarga.Count - 1 do
            begin
              sSecao := 'infUnidCarga' + IntToStrZero(I + 1, 3) +
                         IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3) +
                         IntToStrZero(l + 1, 3);

              INIRec.WriteString(sSecao, 'idUnidCarga', infDoc.infMunDescarga[i].infCTe[j].infUnidTransp[k].infUnidCarga[l].idUnidCarga);
              INIRec.WriteFloat(sSecao, 'qtdRat', infDoc.infMunDescarga[i].infCTe[j].infUnidTransp[k].infUnidCarga[l].qtdRat);
              INIRec.WriteString(sSecao, 'tpUnidCarga', UnidCargaToStr(infDoc.infMunDescarga[i].infCTe[j].infUnidTransp[k].infUnidCarga[l].tpUnidCarga));

              for m := 0 to infDoc.infMunDescarga[i].infCTe[j].infUnidTransp[k].infUnidCarga[l].lacUnidCarga.Count - 1 do
              begin
                sSecao := 'lacUnidCarga' + IntToStrZero(I + 1, 3) +
                           IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3) +
                           IntToStrZero(l + 1, 3) + IntToStrZero(m + 1, 3);

                INIRec.WriteString(sSecao, 'nLacre', infDoc.infMunDescarga[i].infCTe[j].infUnidTransp[k].infUnidCarga[l].lacUnidCarga[m].nLacre);
              end;
            end;
          end;
        end;

        for j := 0 to infDoc.infMunDescarga[i].infNFe.Count - 1 do
        begin
          sSecao := 'infNFe' + IntToStrZero(I + 1, 3) + IntToStrZero(j + 1, 3);

          INIRec.WriteString(sSecao, 'chNFe', infDoc.infMunDescarga[i].infNFe[j].chNFe);
          INIRec.WriteString(sSecao, 'SegCodBarra', infDoc.infMunDescarga[i].infNFe[j].SegCodBarra);
          INIRec.WriteString(sSecao, 'indReentrega', infDoc.infMunDescarga[i].infNFe[j].indReentrega);

          for k := 0 to infDoc.infMunDescarga[i].infNFe[j].peri.Count - 1 do
          begin
            sSecao := 'peri' + IntToStrZero(I + 1, 3) +
                            IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3);

            INIRec.WriteString(sSecao, 'nONU', infDoc.infMunDescarga[i].infNFe[j].peri[k].nONU);
            INIRec.WriteString(sSecao, 'xNomeAE', infDoc.infMunDescarga[i].infNFe[j].peri[k].xNomeAE);
            INIRec.WriteString(sSecao, 'xClaRisco', infDoc.infMunDescarga[i].infNFe[j].peri[k].xClaRisco);
            INIRec.WriteString(sSecao, 'grEmb', infDoc.infMunDescarga[i].infNFe[j].peri[k].grEmb);
            INIRec.WriteString(sSecao, 'qTotProd', infDoc.infMunDescarga[i].infNFe[j].peri[k].qTotProd);
            INIRec.WriteString(sSecao, 'qVolTipo', infDoc.infMunDescarga[i].infNFe[j].peri[k].qVolTipo);
          end;

          for k := 0 to infDoc.infMunDescarga[i].infNFe[j].infUnidTransp.Count - 1 do
          begin
            sSecao := 'infUnidTransp' + IntToStrZero(I + 1, 3) +
                            IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3);

            INIRec.WriteString(sSecao, 'idUnidTransp', infDoc.infMunDescarga[i].infNFe[j].infUnidTransp[k].idUnidTransp);
            INIRec.WriteFloat(sSecao, 'qtdRat', infDoc.infMunDescarga[i].infNFe[j].infUnidTransp[k].qtdRat);
            INIRec.WriteString(sSecao, 'tpUnidTransp', UnidTranspToStr(infDoc.infMunDescarga[i].infNFe[j].infUnidTransp[k].tpUnidTransp));

            for l := 0 to infDoc.infMunDescarga[i].infNFe[j].infUnidTransp[k].lacUnidTransp.Count - 1 do
            begin
              sSecao := 'lacUnidTransp' + IntToStrZero(I + 1, 3) +
                         IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3) +
                         IntToStrZero(l + 1, 3);

              INIRec.WriteString(sSecao, 'nLacre', infDoc.infMunDescarga[i].infNFe[j].infUnidTransp[k].lacUnidTransp[l].nLacre);
            end;

            for l := 0 to infDoc.infMunDescarga[i].infNFe[j].infUnidTransp[k].infUnidCarga.Count - 1 do
            begin
              sSecao := 'infUnidCarga' + IntToStrZero(I + 1, 3) +
                         IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3) +
                         IntToStrZero(l + 1, 3);

              INIRec.WriteString(sSecao, 'idUnidCarga', infDoc.infMunDescarga[i].infNFe[j].infUnidTransp[k].infUnidCarga[l].idUnidCarga);
              INIRec.WriteFloat(sSecao, 'qtdRat', infDoc.infMunDescarga[i].infNFe[j].infUnidTransp[k].infUnidCarga[l].qtdRat);
              INIRec.WriteString(sSecao, 'tpUnidCarga', UnidCargaToStr(infDoc.infMunDescarga[i].infNFe[j].infUnidTransp[k].infUnidCarga[l].tpUnidCarga));

              for m := 0 to infDoc.infMunDescarga[i].infNFe[j].infUnidTransp[k].infUnidCarga[l].lacUnidCarga.Count - 1 do
              begin
                sSecao := 'lacUnidCarga' + IntToStrZero(I + 1, 3) +
                           IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3) +
                           IntToStrZero(l + 1, 3) + IntToStrZero(m + 1, 3);

                INIRec.WriteString(sSecao, 'nLacre', infDoc.infMunDescarga[i].infNFe[j].infUnidTransp[k].infUnidCarga[l].lacUnidCarga[m].nLacre);
              end;
            end;
          end;
        end;

        for j := 0 to infDoc.infMunDescarga[i].infMDFeTransp.Count - 1 do
        begin
          sSecao := 'infMDFeTransp' + IntToStrZero(I + 1, 3) + IntToStrZero(j + 1, 3);

          INIRec.WriteString(sSecao, 'chMDFe', infDoc.infMunDescarga[i].infMDFeTransp[j].chMDFe);
          INIRec.WriteString(sSecao, 'indReentrega', infDoc.infMunDescarga[i].infMDFeTransp[j].indReentrega);

          for k := 0 to infDoc.infMunDescarga[i].infMDFeTransp[j].peri.Count - 1 do
          begin
            sSecao := 'peri' + IntToStrZero(I + 1, 3) +
                            IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3);

            INIRec.WriteString(sSecao, 'nONU', infDoc.infMunDescarga[i].infMDFeTransp[j].peri[k].nONU);
            INIRec.WriteString(sSecao, 'xNomeAE', infDoc.infMunDescarga[i].infMDFeTransp[j].peri[k].xNomeAE);
            INIRec.WriteString(sSecao, 'xClaRisco', infDoc.infMunDescarga[i].infMDFeTransp[j].peri[k].xClaRisco);
            INIRec.WriteString(sSecao, 'grEmb', infDoc.infMunDescarga[i].infMDFeTransp[j].peri[k].grEmb);
            INIRec.WriteString(sSecao, 'qTotProd', infDoc.infMunDescarga[i].infMDFeTransp[j].peri[k].qTotProd);
            INIRec.WriteString(sSecao, 'qVolTipo', infDoc.infMunDescarga[i].infMDFeTransp[j].peri[k].qVolTipo);
          end;

          for k := 0 to infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp.Count - 1 do
          begin
            sSecao := 'infUnidTransp' + IntToStrZero(I + 1, 3) +
                            IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3);

            INIRec.WriteString(sSecao, 'idUnidTransp', infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp[k].idUnidTransp);
            INIRec.WriteFloat(sSecao, 'qtdRat', infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp[k].qtdRat);
            INIRec.WriteString(sSecao, 'tpUnidTransp', UnidTranspToStr(infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp[k].tpUnidTransp));

            for l := 0 to infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp[k].lacUnidTransp.Count - 1 do
            begin
              sSecao := 'lacUnidTransp' + IntToStrZero(I + 1, 3) +
                         IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3) +
                         IntToStrZero(l + 1, 3);

              INIRec.WriteString(sSecao, 'nLacre', infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp[k].lacUnidTransp[l].nLacre);
            end;

            for l := 0 to infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp[k].infUnidCarga.Count - 1 do
            begin
              sSecao := 'infUnidCarga' + IntToStrZero(I + 1, 3) +
                         IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3) +
                         IntToStrZero(l + 1, 3);

              INIRec.WriteString(sSecao, 'idUnidCarga', infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp[k].infUnidCarga[l].idUnidCarga);
              INIRec.WriteFloat(sSecao, 'qtdRat', infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp[k].infUnidCarga[l].qtdRat);
              INIRec.WriteString(sSecao, 'tpUnidCarga', UnidCargaToStr(infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp[k].infUnidCarga[l].tpUnidCarga));

              for m := 0 to infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp[k].infUnidCarga[l].lacUnidCarga.Count - 1 do
              begin
                sSecao := 'lacUnidCarga' + IntToStrZero(I + 1, 3) +
                           IntToStrZero(j + 1, 3) + IntToStrZero(k + 1, 3) +
                           IntToStrZero(l + 1, 3) + IntToStrZero(m + 1, 3);

                INIRec.WriteString(sSecao, 'nLacre', infDoc.infMunDescarga[i].infMDFeTransp[j].infUnidTransp[k].infUnidCarga[l].lacUnidCarga[m].nLacre);
              end;
            end;
          end;
        end;
      end;

      for i := 0 to seg.Count - 1 do
      begin
        sSecao := 'seg' + IntToStrZero(I + 1, 3);

        INIRec.WriteString(sSecao, 'respSeg', RspSeguroMDFeToStr(seg[i].respSeg));
        INIRec.WriteString(sSecao, 'CNPJCPF', seg[i].CNPJCPF);
        INIRec.WriteString(sSecao, 'xSeg', seg[i].xSeg);
        INIRec.WriteString(sSecao, 'CNPJ', seg[i].CNPJ);
        INIRec.WriteString(sSecao, 'nApol', seg[i].nApol);

        for j := 0 to seg[i].aver.Count - 1 do
        begin
          sSecao := 'aver' + IntToStrZero(I + 1, 3) + IntToStrZero(j + 1, 3);

          INIRec.WriteString(sSecao, 'nAver', seg[i].aver[j].nAver);
        end;
      end;

      sSecao := 'prodPred';

      INIRec.WriteString(sSecao, 'tpCarga', TCargaToStr(prodPred.tpCarga));
      INIRec.WriteString(sSecao, 'xProd', prodPred.xProd);
      INIRec.WriteString(sSecao, 'cEAN', prodPred.cEAN);
      INIRec.WriteString(sSecao, 'NCM', prodPred.NCM);

      sSecao := 'infLocalCarrega';

      INIRec.WriteInteger(sSecao, 'CEP', prodPred.infLocalCarrega.CEP);
      INIRec.WriteFloat(sSecao, 'latitude', prodPred.infLocalCarrega.latitude);
      INIRec.WriteFloat(sSecao, 'longitude', prodPred.infLocalCarrega.longitude);

      sSecao := 'infLocalDescarrega';

      INIRec.WriteInteger(sSecao, 'CEP', prodPred.infLocalDescarrega.CEP);
      INIRec.WriteFloat(sSecao, 'latitude', prodPred.infLocalDescarrega.latitude);
      INIRec.WriteFloat(sSecao, 'longitude', prodPred.infLocalDescarrega.longitude);

      sSecao := 'tot';

      INIRec.WriteInteger(sSecao, 'qCTe', tot.qCTe);
      INIRec.WriteInteger(sSecao, 'qNFe', tot.qNFe);
      INIRec.WriteInteger(sSecao, 'qMDFe', tot.qMDFe);
      INIRec.WriteFloat(sSecao, 'vCarga', tot.vCarga);
      INIRec.WriteFloat(sSecao, 'qCarga', tot.qCarga);
      INIRec.WriteString(sSecao, 'cUnid', UnidMedToStr(tot.cUnid));

      for i := 0 to lacres.Count - 1 do
      begin
        sSecao := 'lacres' + IntToStrZero(I + 1, 3);

        INIRec.WriteString(sSecao, 'nLacre', lacres[i].nLacre);
      end;

      for i := 0 to autXML.Count - 1 do
      begin
        sSecao := 'autXML' + IntToStrZero(I + 1, 2);

        INIRec.WriteString(sSecao, 'CNPJCPF', autXML[i].CNPJCPF);
      end;

      sSecao := 'infAdic';

      INIRec.WriteString(sSecao, 'infAdFisco', infAdic.infAdFisco);
      INIRec.WriteString(sSecao, 'infCpl', infAdic.infCpl);

      INIRec.WriteString('infRespTec', 'CNPJ', infRespTec.CNPJ);
      INIRec.WriteString('infRespTec', 'xContato', infRespTec.xContato);
      INIRec.WriteString('infRespTec', 'email', infRespTec.email);
      INIRec.WriteString('infRespTec', 'fone', infRespTec.fone);

      INIRec.WriteString('procMDFe', 'tpAmb', TpAmbToStr(procMDFe.tpAmb));
      INIRec.WriteString('procMDFe', 'verAplic', procMDFe.verAplic);
      INIRec.WriteString('procMDFe', 'chMDFe', procMDFe.chMDFe);
      INIRec.WriteString('procMDFe', 'dhRecbto', DateTimeToStr(procMDFe.dhRecbto));
      INIRec.WriteString('procMDFe', 'nProt', procMDFe.nProt);
      INIRec.WriteString('procMDFe', 'digVal', procMDFe.digVal);
      INIRec.WriteString('procMDFe', 'cStat', IntToStr(procMDFe.cStat));
      INIRec.WriteString('procMDFe', 'xMotivo', procMDFe.xMotivo);
    end;

    IniDFe := TStringList.Create;
    try
      INIRec.GetStrings(IniDFe);
      Result := StringReplace(IniDFe.Text, sLineBreak + sLineBreak, sLineBreak, [rfReplaceAll]);
    finally
      IniDFe.Free;
    end;
  finally
    INIRec.Free;
  end;
end;

function TManifesto.GerarXML: String;
var
  IdAnterior : String;
begin
  with TACBrMDFe(TManifestos(Collection).ACBrMDFe) do
  begin
    IdAnterior := MDFe.infMDFe.ID;
    FMDFeW.Gerador.Opcoes.FormatoAlerta  := Configuracoes.Geral.FormatoAlerta;
    FMDFeW.Gerador.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
    FMDFeW.Gerador.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
    FMDFeW.Gerador.Opcoes.IdentarXML     := Configuracoes.Geral.IdentarXML;
    FMDFeW.Opcoes.NormatizarMunicipios   := Configuracoes.Arquivos.NormatizarMunicipios;
    FMDFeW.Opcoes.PathArquivoMunicipios  := Configuracoes.Arquivos.PathArquivoMunicipios;

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

  XMLOriginal := FMDFeW.Gerador.ArquivoFormatoXML;  // SetXMLOriginal() irá converter para UTF8

  { XML gerado pode ter nova Chave e ID, então devemos calcular novamente o
    nome do arquivo, mantendo o PATH do arquivo carregado }
  if (NaoEstaVazio(FNomeArq) and (IdAnterior <> FMDFe.infMDFe.ID)) then
    FNomeArq := CalcularNomeArquivoCompleto('', ExtractFilePath(FNomeArq));

  FAlertas := FMDFeW.Gerador.ListaDeAlertas.Text;
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
var
  I, J, K, L, M: Integer;
  versao, sSecao, sFim: String;
  OK, GerarGrupo: boolean;
  INIRec : TMemIniFile;
  IteminfMunCarrega: TinfMunCarregaCollectionItem;
  ItemInfCIOT: TinfCIOTCollectionItem;
  ItemDisp: TdispCollectionItem;
  ItemInfContratante: TinfContratanteCollectionItem;
  ItemInfPag: TinfPagCollectionItem;
  ItemComp: TCompCollectionItem;
  ItemInfPrazo: TInfPrazoCollectionItem;
  ItemCondudor: TcondutorCollectionItem;
  ItemVeicReboque: TveicReboqueCollectionItem;
  ItemInfTermCarreg: TinfTermCarregCollectionItem;
  IteminfTermDescarreg: TinfTermDescarregCollectionItem;
  ItemInfEmbComb: TinfEmbCombCollectionItem;
  IteminfUnidCargaVazia: TinfUnidCargaVaziaCollectionItem;
  IteminfUnidTranspVazia: TinfUnidTranspVaziaCollectionItem;
  ItemVag: TvagCollectionItem;
  IteminfMunDescarga: TinfMunDescargaCollectionItem;
  ItemInfCTe: TinfCTeCollectionItem;
  ItemPeri: TPeriCollectionItem;
  IteminfunidTransp: TinfUnidTranspCollectionItem;
  ItemInfUnidCarga: TinfUnidCargaCollectionItem;
  ItemInfNFe: TinfNFeCollectionItem;
  ItemInfMDFeTransp: TinfMDFeTranspCollectionItem;
  ItemSeg: TSegCollectionItem;
begin
  Result := False;

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    with FMDFe do
    begin
      OK := True;

      infMDFe.versao := StringToFloatDef( INIRec.ReadString('infMDFe','versao', VersaoMDFeToStr(FConfiguracoes.Geral.VersaoDF)),0) ;
//      versao         := FloatToString(infMDFe.versao, '.', '#0.00');
//      FConfiguracoes.Geral.VersaoDF := StrToVersaoMDFe(OK, versao);

      Ide.tpEmit  := StrToTpEmitente(OK, INIRec.ReadString('ide', 'tpEmit', '1'));
      Ide.modelo  := INIRec.ReadString('ide', 'mod', '58');
      Ide.serie    := INIRec.ReadInteger('ide', 'serie', 1);
      Ide.nMDF     := INIRec.ReadInteger('ide', 'nMDF', 0);
      Ide.cMDF     := INIRec.ReadInteger('ide', 'cMDF', 0);
      Ide.modal    := StrToModal(OK, INIRec.ReadString('ide', 'modal', '1'));
      Ide.dhEmi    := StringToDateTime(INIRec.ReadString('ide', 'dhEmi', '0'));
      Ide.tpEmis   := StrToTpEmis(OK, INIRec.ReadString('ide', 'tpEmis', IntToStr(FConfiguracoes.Geral.FormaEmissaoCodigo)));
      Ide.procEmi  := StrToProcEmi(OK, INIRec.ReadString('ide', 'procEmi', '0'));
      Ide.verProc  := INIRec.ReadString('ide', 'verProc', 'ACBrMDFe');
      Ide.UFIni    := INIRec.ReadString('ide', 'UFIni', '');
      Ide.UFFim    := INIRec.ReadString('ide', 'UFFim', '');
      Ide.tpTransp := StrToTTransportador(OK, INIRec.ReadString('ide', 'tpTransp', '1'));

      I := 1;
      while true do
      begin
        sSecao := 'CARR' + IntToStrZero(I, 3);
        sFim   := INIRec.ReadString(sSecao, 'xMunCarrega', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        IteminfMunCarrega := Ide.infMunCarrega.New;

        IteminfMunCarrega.cMunCarrega := INIRec.ReadInteger(sSecao, 'cMunCarrega', 0);
        IteminfMunCarrega.xMunCarrega := sFim;

        Inc(I);
      end;

      I := 1;
      while true do
      begin
        sSecao := 'PERC' + IntToStrZero(I, 3);
        sFim   := INIRec.ReadString(sSecao, 'UFPer', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        Ide.infPercurso.New.UFPer := sFim;

        Inc(I);
      end;

      Ide.dhIniViagem   := StringToDateTime(INIRec.ReadString('ide', 'dhIniViagem', '0'));
      Ide.indCanalVerde := StrToTIndicador(Ok, INIRec.ReadString('ide', 'indCanalVerde', '0'));
      Ide.indCarregaPosterior := StrToTIndicador(Ok, INIRec.ReadString('ide', 'indCarregaPosterior', '0'));

      Emit.CNPJCPF := INIRec.ReadString('emit', 'CNPJCPF', INIRec.ReadString('emit', 'CNPJ', ''));
      Emit.IE      := INIRec.ReadString('emit', 'IE', '');
      Emit.xNome   := INIRec.ReadString('emit', 'xNome', '');
      Emit.xFant   := INIRec.ReadString('emit', 'xFant', '');

      Emit.enderEmit.xLgr    := INIRec.ReadString('emit', 'xLgr', '');
      Emit.enderEmit.nro     := INIRec.ReadString('emit', 'nro', '');
      Emit.enderEmit.xCpl    := INIRec.ReadString('emit', 'xCpl', '');
      Emit.enderEmit.xBairro := INIRec.ReadString('emit', 'xBairro', '');
      Emit.enderEmit.cMun    := INIRec.ReadInteger('emit', 'cMun', 0);
      Emit.enderEmit.xMun    := INIRec.ReadString('emit', 'xMun', '');
      Emit.enderEmit.CEP     := INIRec.ReadInteger('emit', 'CEP', 0);
      Emit.enderEmit.UF      := INIRec.ReadString('emit', 'UF', '');
      Emit.enderEmit.fone    := INIRec.ReadString('emit', 'fone', '');
      Emit.enderEmit.email   := INIRec.ReadString('emit', 'email', '');

      ide.cUF := INIRec.ReadInteger('ide', 'cUF', UFparaCodigoUF(Emit.enderEmit.UF));

      //*********************************************************************
      //
      // Modal Rodoviário
      //
      //*********************************************************************

      rodo.codAgPorto := INIRec.ReadString('Rodo', 'codAgPorto', '');

      // Dados sobre Informações para Agencia Reguladora (Opcional) - Nível 1 - Versão 3.00

      GerarGrupo := (INIRec.ReadString('infANTT', 'RNTRC', '') <> '') or
                    INIRec.SectionExists('infCIOT001') or
                    INIRec.SectionExists('valePed001') or
                    INIRec.SectionExists('infContratante001');

      if GerarGrupo then
      begin
        rodo.infANTT.RNTRC := INIRec.ReadString('infANTT', 'RNTRC', '');

        // Dados do CIOT (Opcional) - Nível 2 - Versão 3.00

        I := 1;
        while true do
        begin
          sSecao := 'infCIOT' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'CNPJCPF', 'FIM');

          if sFim = 'FIM' then
            break;

          ItemInfCIOT := rodo.infANTT.infCIOT.New;

          ItemInfCIOT.CIOT    := INIRec.ReadString(sSecao, 'CIOT', '');
          ItemInfCIOT.CNPJCPF := sFim;

          Inc(I);
        end;

        // Dados do Vale Pedágio (Opcional) - Nível 2 - Versão 3.00

        rodo.infANTT.valePed.categCombVeic := StrTocategCombVeic(OK, INIRec.ReadString('valePed', 'categCombVeic', ''));

        I := 1;
        while true do
        begin
          sSecao := 'disp' + IntToStrZero(I, 3);

          if not INIRec.SectionExists(sSecao) then
            sSecao := 'valePed' + IntToStrZero(I, 3);

          sFim   := INIRec.ReadString(sSecao, 'CNPJForn', 'FIM');

          if sFim = 'FIM' then
            break;

          ItemDisp := rodo.infANTT.valePed.disp.New;

          ItemDisp.CNPJForn  := sFim;
          ItemDisp.CNPJPg    := INIRec.ReadString(sSecao, 'CNPJPg', '');
          ItemDisp.nCompra   := INIRec.ReadString(sSecao, 'nCompra', '');
          ItemDisp.vValePed  := StringToFloatDef(INIRec.ReadString(sSecao, 'vValePed', ''), 0 );
          ItemDisp.tpValePed := StrTotpValePed(OK, INIRec.ReadString(sSecao, 'tpValePed', ''));

          Inc(I);
        end;

        // Dados dos Contratantes (Opcional) - Nível 2 - Versão 3.00

        I := 1;
        while true do
        begin
          sSecao := 'infContratante' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'CNPJCPF', INIRec.ReadString(sSecao, 'idEstrangeiro', 'FIM'));

          if sFim = 'FIM' then
            break;

          ItemInfContratante := rodo.infANTT.infContratante.New;

          ItemInfContratante.CNPJCPF       := INIRec.ReadString(sSecao, 'CNPJCPF', '');
          ItemInfContratante.idEstrangeiro := INIRec.ReadString(sSecao, 'idEstrangeiro', '');
          ItemInfContratante.xNome         := INIRec.ReadString(sSecao, 'xNome', '');

          ItemInfContratante.infContrato.NroContrato := INIRec.ReadString(sSecao, 'NroContrato', '');
          ItemInfContratante.infContrato.vContratoGlobal := StringToFloatDef(INIRec.ReadString(sSecao, 'vContratoGlobal', ''), 0 );

          Inc(I);
        end;

        sSecao := 'infPag001';

        if INIRec.SectionExists(sSecao) then
        begin
          I := 1;
          while true do
          begin
            sSecao := 'infPag' + IntToStrZero(I, 3);
            sFim   := INIRec.ReadString(sSecao, 'CNPJCPF', INIRec.ReadString(sSecao, 'idEstrangeiro', 'FIM'));

            if sFim = 'FIM' then
              break;

            ItemInfPag := rodo.infANTT.infPag.New;

            ItemInfPag.xNome         := INIRec.ReadString(sSecao, 'xNome', '');
            ItemInfPag.idEstrangeiro := INIRec.ReadString(sSecao, 'idEstrangeiro', '');

            if ItemInfPag.idEstrangeiro = '' then
              ItemInfPag.CNPJCPF := INIRec.ReadString(sSecao, 'CNPJCPF', '');

            ItemInfPag.vContrato     := StringToFloatDef(INIRec.ReadString(sSecao, 'vContrato', ''), 0 );
            ItemInfPag.indAltoDesemp := StrToindAltoDesemp(ok, INIRec.ReadString(sSecao, 'indAltoDesemp', ''));
            ItemInfPag.indPag        := StrToTIndPag(ok, INIRec.ReadString(sSecao, 'indPag', '0'));
            ItemInfPag.vAdiant       := StringToFloatDef(INIRec.ReadString(sSecao, 'vAdiant', ''), 0 );

            ItemInfPag.indAntecipaAdiant := StrToTIndicador(ok, INIRec.ReadString(sSecao, 'indAntecipaAdiant', '0'));
            ItemInfPag.tpAntecip := StrTotpAntecip(ok, INIRec.ReadString(sSecao, 'tpAntecip', ''));

            J := 1;
            while true do
            begin
              sSecao := 'Comp' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
              sFim   := INIRec.ReadString(sSecao, 'vComp', 'FIM');

              if sFim = 'FIM' then
                break;

              ItemComp := ItemInfPag.Comp.New;

              ItemComp.tpComp := StrToTComp(ok, INIRec.ReadString(sSecao, 'tpComp', '01'));
              ItemComp.vComp  := StringToFloatDef(INIRec.ReadString(sSecao, 'vComp', ''), 0 );
              ItemComp.xComp  := INIRec.ReadString(sSecao, 'xComp', '');

              Inc(J);
            end;

            if ItemInfPag.indPag = ipPrazo then
            begin
              j := 1;
              while true do
              begin
                sSecao := 'infPrazo' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
                sFim   := INIRec.ReadString(sSecao, 'vParcela', 'FIM');

                if sFim = 'FIM' then
                  break;

                ItemInfPrazo := ItemInfPag.infPrazo.New;

                ItemInfPrazo.nParcela := INIRec.ReadInteger(sSecao, 'nParcela', 1);
                ItemInfPrazo.dVenc    := StringToDateTime(INIRec.ReadString(sSecao, 'dVenc', '0'));
                ItemInfPrazo.vParcela := StringToFloatDef(INIRec.ReadString(sSecao, 'vParcela', ''), 0 );

                Inc(J);
              end;
            end;

            sSecao := 'infBanc' + IntToStrZero(I, 3);

            if INIRec.SectionExists(sSecao) then
            begin
              ItemInfPag.infBanc.PIX := INIRec.ReadString(sSecao, 'PIX', '');

              if ItemInfPag.infBanc.PIX = '' then
              begin
                ItemInfPag.infBanc.CNPJIPEF := INIRec.ReadString(sSecao, 'CNPJIPEF', '');

                if ItemInfPag.infBanc.CNPJIPEF = '' then
                begin
                  ItemInfPag.infBanc.codBanco   := INIRec.ReadString(sSecao, 'codBanco', '');
                  ItemInfPag.infBanc.codAgencia := INIRec.ReadString(sSecao, 'codAgencia', '');
                end;
              end;
            end;

            Inc(I);
          end;
        end;
      end;

      // Dados do veículo de Tração (Obrigatório) - Nível 1
      if INIRec.ReadString('veicTracao', 'placa', '') <> '' then
      begin
        rodo.veicTracao.cInt    := INIRec.ReadString('veicTracao', 'cInt', '');
        rodo.veicTracao.placa   := INIRec.ReadString('veicTracao', 'placa', '');
        rodo.veicTracao.RENAVAM := INIRec.ReadString('veicTracao', 'RENAVAM', '');
        rodo.veicTracao.tara    := INIRec.ReadInteger('veicTracao', 'tara', 0);
        rodo.veicTracao.capKG   := INIRec.ReadInteger('veicTracao', 'capKG', 0);
        rodo.veicTracao.capM3   := INIRec.ReadInteger('veicTracao', 'capM3', 0);
        rodo.veicTracao.tpRod   := StrToTpRodado(OK, INIRec.ReadString('veicTracao', 'tpRod', '01'));
        rodo.veicTracao.tpCar   := StrToTpCarroceria(OK, INIRec.ReadString('veicTracao', 'tpCar', '00'));
        rodo.veicTracao.UF      := INIRec.ReadString('veicTracao', 'UF', '');
      end;

      // Dados do proprietário do veículo de Tração (Opcional) - Nível 2

      if INIRec.ReadString('veicTracao', 'CNPJCPF', '') <> '' then
      begin
        rodo.veicTracao.prop.CNPJCPF := INIRec.ReadString('veicTracao', 'CNPJCPF', '');
        rodo.veicTracao.prop.RNTRC   := INIRec.ReadString('veicTracao', 'RNTRC', '');
        rodo.veicTracao.prop.xNome   := INIRec.ReadString('veicTracao', 'xNome', '');
        rodo.veicTracao.prop.IE      := INIRec.ReadString('veicTracao', 'IE', 'ISENTO');
        rodo.veicTracao.prop.UF      := INIRec.ReadString('veicTracao', 'UFProp', '');
        rodo.veicTracao.prop.tpProp  := StrToTpProp(OK, INIRec.ReadString('veicTracao', 'tpProp', '0'));
      end;

      // Dados do Condudor do veículo de Tração (Obrigatório) - Nível 2

      I := 1;
      while true do
      begin
        sSecao := 'moto' + IntToStrZero(I, 3);
        sFim   := INIRec.ReadString(sSecao, 'xNome', 'FIM');

        if sFim = 'FIM' then
          break;

        ItemCondudor := rodo.veicTracao.condutor.New;

        ItemCondudor.xNome := sFim;
        ItemCondudor.CPF   := INIRec.ReadString(sSecao, 'CPF', '');

        Inc(I);
      end;

      // Dados do veículo Reboque (Opcional) - Nível 1

      I := 1;
      while true do
      begin
        sSecao := 'reboque' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'placa', 'FIM');

        if sFim = 'FIM' then
          break;

        ItemVeicReboque := rodo.veicReboque.New;

        ItemVeicReboque.cInt    := INIRec.ReadString(sSecao, 'cInt', '');
        ItemVeicReboque.placa   := sFim;
        ItemVeicReboque.RENAVAM := INIRec.ReadString(sSecao, 'RENAVAM', '');
        ItemVeicReboque.tara    := INIRec.ReadInteger(sSecao, 'tara', 0);
        ItemVeicReboque.capKG   := INIRec.ReadInteger(sSecao, 'capKG', 0);
        ItemVeicReboque.capM3   := INIRec.ReadInteger(sSecao, 'capM3', 0);
        ItemVeicReboque.tpCar   := StrToTpCarroceria(OK, INIRec.ReadString(sSecao, 'tpCar', '00'));
        ItemVeicReboque.UF      := INIRec.ReadString(sSecao, 'UF', '');

        // Dados do proprietário do veículo Reboque (Opcional) - Nível 2 - Versão 3.00

        if INIRec.ReadString(sSecao, 'CNPJCPF', '') <> '' then
        begin
          ItemVeicReboque.prop.CNPJCPF := INIRec.ReadString(sSecao, 'CNPJCPF', '');
          ItemVeicReboque.prop.RNTRC   := INIRec.ReadString(sSecao, 'RNTRC', '');
          ItemVeicReboque.prop.xNome   := INIRec.ReadString(sSecao, 'xNome', '');
          ItemVeicReboque.prop.IE      := INIRec.ReadString(sSecao, 'IE', '');
          ItemVeicReboque.prop.UF      := INIRec.ReadString(sSecao, 'UFProp', '');
          ItemVeicReboque.prop.tpProp  := StrToTpProp(OK, INIRec.ReadString(sSecao, 'tpProp', '0'));
        end;

        Inc(I);
      end;

      // Dados do Lacre (Opcional) - Nível 1

      I := 1;
      while true do
      begin
        sSecao := 'lacRodo' + IntToStrZero(I, 3);
        sFim   := INIRec.ReadString(sSecao, 'nLacre', 'FIM');

        if sFim = 'FIM' then
          break;

        rodo.lacRodo.New.nLacre := sFim;

        Inc(I);
      end;

      //*********************************************************************
      //
      // Modal Aéreo
      //
      //*********************************************************************

      Aereo.nac := INIRec.ReadString('aereo', 'nac', '');
      if (Aereo.nac <> '') then
      begin
        Aereo.matr    := INIRec.ReadString('aereo', 'matr', '');
        Aereo.nVoo    := INIRec.ReadString('aereo', 'nVoo', '');
        Aereo.cAerEmb := INIRec.ReadString('aereo', 'cAerEmb', '');
        Aereo.cAerDes := INIRec.ReadString('aereo', 'cAerDes', '');
        Aereo.dVoo    := StringToDateTime(INIRec.ReadString('aereo', 'dVoo', '0'));
      end; // Fim do Aereoviário

      //*********************************************************************
      //
      // Modal Aquaviário
      //
      //*********************************************************************

      Aquav.CNPJAgeNav  := INIRec.ReadString('aquav', 'CNPJAgeNav', '');
      Aquav.irin        := INIRec.ReadString('aquav', 'irin', '');

      if ( (Aquav.CNPJAgeNav  <> '') or (Aquav.irin <> '') ) then
      begin
        Aquav.tpEmb    := INIRec.ReadString('aquav', 'tpEmb', '');
        Aquav.cEmbar   := INIRec.ReadString('aquav', 'cEmbar', '');
        Aquav.xEmbar   := INIRec.ReadString('aquav', 'xEmbar', '');
        Aquav.nViagem  := INIRec.ReadString('aquav', 'nViag', '');
        Aquav.cPrtEmb  := INIRec.ReadString('aquav', 'cPrtEmb', '');
        Aquav.cPrtDest := INIRec.ReadString('aquav', 'cPrtDest', '');

        //Campos MDF-e 3.0
        Aquav.prtTrans := INIRec.ReadString('aquav', 'prtTrans', '');
        Aquav.tpNav    := StrToTpNavegacao(OK, INIRec.ReadString('aquav', 'tpNav', '0') );

        I := 1;
        while true do
        begin
          sSecao := 'infTermCarreg' + IntToStrZero(I, 1);
          sFim   := INIRec.ReadString(sSecao, 'cTermCarreg', 'FIM');

          if sFim = 'FIM' then
            break;

          ItemInfTermCarreg := Aquav.infTermCarreg.New;

          ItemInfTermCarreg.cTermCarreg := sFim;
          ItemInfTermCarreg.xTermCarreg := INIRec.ReadString(sSecao, 'xTermCarreg', '');

          inc(I);
        end;

        I := 1;
        while true do
        begin
          sSecao := 'infTermDescarreg' + IntToStrZero(I, 1);
          sFim   := INIRec.ReadString(sSecao, 'cTermDescarreg', 'FIM');

          if sFim = 'FIM' then
            break;

          IteminfTermDescarreg := Aquav.infTermDescarreg.New;

          IteminfTermDescarreg.cTermDescarreg := sFim;
          IteminfTermDescarreg.xTermDescarreg := INIRec.ReadString(sSecao, 'xTermDescarreg', '');

          inc(I);
        end;

        I := 1;
        while true do
        begin
          sSecao := 'infEmbComb' + IntToStrZero(I, 2);
          sFim   := INIRec.ReadString(sSecao, 'cEmbComb', 'FIM');

          if sFim = 'FIM' then
            break;

          ItemInfEmbComb := Aquav.infEmbComb.New;

          ItemInfEmbComb.cEmbComb := sFim;
          ItemInfEmbComb.xBalsa   :=  INIRec.ReadString(sSecao, 'xBalsa', '');

          inc(I);
        end;

        I := 1;
        while true do
        begin
          sSecao := 'infUnidCargaVazia' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'idUnidCargaVazia', 'FIM');

          if sFim = 'FIM' then
            break;

          IteminfUnidCargaVazia := Aquav.infUnidCargaVazia.New;

          IteminfUnidCargaVazia.idUnidCargaVazia := sFim;
          IteminfUnidCargaVazia.tpUnidCargaVazia := StrToUnidCarga(OK, INIRec.ReadString(sSecao, 'tpUnidCargaVazia', '1'));

          inc(I);
        end;

        I := 1;
        while true do
        begin
          sSecao := 'infUnidTranspVazia' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'idUnidTranspVazia', 'FIM');

          if sFim = 'FIM' then
            break;

          IteminfUnidTranspVazia := Aquav.infUnidTranspVazia.New;

          IteminfUnidTranspVazia.idUnidTranspVazia := sFim;
          IteminfUnidTranspVazia.tpUnidTranspVazia := StrToUnidTransp (OK, INIRec.ReadString(sSecao, 'tpUnidTranspVazia', '1'));

          inc(I);
        end;
      end; // Fim do Aquaviário

      //*********************************************************************
      //
      // Modal Ferroviário
      //
      //*********************************************************************

      Ferrov.xPref  := INIRec.ReadString('ferrov', 'xPref', '');
      if (Ferrov.xPref <> '') then
      begin
        Ferrov.dhTrem := StringToDateTime(INIRec.ReadString('ferrov', 'dhTrem', '0'));
        Ferrov.xOri   := INIRec.ReadString('ferrov', 'xOri', '');
        Ferrov.xDest  := INIRec.ReadString('ferrov', 'xDest', '');
        Ferrov.qVag   := INIRec.ReadInteger('ferrov', 'qVag', 0);

        I := 1;
        while true do
        begin
          sSecao := 'vag' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'serie', 'FIM');

          if sFim = 'FIM' then
            break;

          ItemVag := Ferrov.vag.New;

          ItemVag.serie := sFim;
          ItemVag.nVag  := INIRec.ReadInteger(sSecao, 'nVag', 0);
          ItemVag.nSeq  := INIRec.ReadInteger(sSecao, 'nSeq', 0);
          ItemVag.TU    := StringToFloatDef(INIRec.ReadString(sSecao, 'TU', ''), 0);

          //Campos MDF-e 3.0
          ItemVag.pesoBC := StringToFloatDef( INIRec.ReadString(sSecao, 'pesoBC', ''), 0);
          ItemVag.pesoR  := StringToFloatDef( INIRec.ReadString(sSecao, 'pesoR', ''), 0);
          ItemVag.tpVag  := INIRec.ReadString(sSecao, 'tpVag', '');

          inc(I);
        end;
      end; // Fim do Ferroviário

      I := 1;
      while true do
      begin
        sSecao := 'DESC' + IntToStrZero(I, 3);
        sFim   := INIRec.ReadString(sSecao, 'xMunDescarga', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        IteminfMunDescarga := infDoc.infMunDescarga.New;

        IteminfMunDescarga.cMunDescarga := INIRec.ReadInteger(sSecao, 'cMunDescarga', 0);
        IteminfMunDescarga.xMunDescarga := sFim;

        J := 1;
        while true do
        begin
          sSecao := 'infCTe' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
          sFim   := INIRec.ReadString(sSecao, 'chCTe', 'FIM');

          if sFim = 'FIM' then
            break;

          ItemInfCTe := IteminfMunDescarga.infCTe.New;

          ItemInfCTe.chCTe       := sFim;
          ItemInfCTe.SegCodBarra := INIRec.ReadString(sSecao, 'SegCodBarra', '');
          ItemInfCTe.indReentrega:= INIRec.ReadString(sSecao, 'indReentrega', '');

          K := 1;
          while true do
          begin
            sSecao := 'peri'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
            sFim   := INIRec.ReadString(sSecao,'nONU','FIM');

            if sFim = 'FIM' then
              break;

            ItemPeri := ItemInfCTe.peri.New;

            ItemPeri.nONU      := sFim;
            ItemPeri.xNomeAE   := INIRec.ReadString(sSecao,'xNomeAE','');
            ItemPeri.xClaRisco := INIRec.ReadString(sSecao,'xClaRisco','');
            ItemPeri.grEmb     := INIRec.ReadString(sSecao,'grEmb','');
            ItemPeri.qTotProd  := INIRec.ReadString(sSecao,'qTotProd','');
            ItemPeri.qVolTipo  := INIRec.ReadString(sSecao,'qVolTipo','');

            inc(K);
          end;

          sSecao := 'infEntregaParcial'+IntToStrZero(I,3)+IntToStrZero(J,3);

          if INIRec.SectionExists(sSecao) then
          begin
            ItemInfCTe.infEntregaParcial.qtdTotal   := StringToFloatDef(INIRec.ReadString(sSecao, 'qtdTotal', ''), 0);
            ItemInfCTe.infEntregaParcial.qtdParcial := StringToFloatDef(INIRec.ReadString(sSecao, 'qtdParcial', ''), 0);
          end;

          K := 1;
          while true do
          begin
            sSecao := 'infUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
            sFim   := INIRec.ReadString(sSecao,'idUnidTransp','FIM');

            if sFim = 'FIM' then
              break;

            IteminfunidTransp := ItemInfCTe.infUnidTransp.New;

            IteminfunidTransp.tpUnidTransp := StrToUnidTransp(OK,INIRec.ReadString(sSecao,'tpUnidTransp','1'));
            IteminfunidTransp.idUnidTransp := sFim;
            IteminfunidTransp.qtdRat       := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);

            L := 1;
            while true do
            begin
              sSecao := 'lacUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
              sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');

              if sFim = 'FIM' then
                break;

              IteminfunidTransp.lacUnidTransp.New.nLacre := sFim;

              inc(L);
            end;

            L := 1;
            while true do
            begin
              sSecao := 'infUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
              sFim   := INIRec.ReadString(sSecao,'idUnidCarga','FIM');

              if sFim = 'FIM' then
                break;

              ItemInfUnidCarga := IteminfunidTransp.infUnidCarga.New;

              ItemInfUnidCarga.tpUnidCarga := StrToUnidCarga(OK,INIRec.ReadString(sSecao,'tpUnidCarga','1'));
              ItemInfUnidCarga.idUnidCarga := sFim;
              ItemInfUnidCarga.qtdRat      := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);

              M := 1;
              while true do
              begin
                sSecao := 'lacUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3)+IntToStrZero(M,3);
                sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');

                if sFim = 'FIM' then
                  break;

                ItemInfUnidCarga.lacUnidCarga.New.nLacre := sFim;

                inc(M);
              end;

              inc(L);
            end;

            inc(K);
          end;

          Inc(J);
        end;

        J := 1;
        while true do
        begin
          sSecao := 'infNFe' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
          sFim   := INIRec.ReadString(sSecao, 'chNFe', 'FIM');

          if sFim = 'FIM' then
            break;

          ItemInfNFe := IteminfMunDescarga.infNFe.New;

          ItemInfNFe.chNFe        := sFim;
          ItemInfNFe.SegCodBarra  := INIRec.ReadString(sSecao, 'SegCodBarra', '');
          ItemInfNFe.indReentrega := INIRec.ReadString(sSecao, 'indReentrega', '');

          K := 1;
          while true do
          begin
            sSecao := 'peri'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
            sFim   := INIRec.ReadString(sSecao,'nONU','FIM');

            if sFim = 'FIM' then
              break;

            ItemPeri := ItemInfNFe.peri.New;

            ItemPeri.nONU      := sFim;
            ItemPeri.xNomeAE   := INIRec.ReadString(sSecao,'xNomeAE','');
            ItemPeri.xClaRisco := INIRec.ReadString(sSecao,'xClaRisco','');
            ItemPeri.grEmb     := INIRec.ReadString(sSecao,'grEmb','');
            ItemPeri.qTotProd  := INIRec.ReadString(sSecao,'qTotProd','');
            ItemPeri.qVolTipo  := INIRec.ReadString(sSecao,'qVolTipo','');

            inc(K);
          end;

          K := 1;
          while true do
          begin
            sSecao := 'infUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
            sFim   := INIRec.ReadString(sSecao,'idUnidTransp','FIM');

            if sFim = 'FIM' then
              break;

            IteminfunidTransp := ItemInfNFe.infUnidTransp.New;

            IteminfunidTransp.tpUnidTransp := StrToUnidTransp(OK,INIRec.ReadString(sSecao,'tpUnidTransp','1'));
            IteminfunidTransp.idUnidTransp := sFim;
            IteminfunidTransp.qtdRat       := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);

            L := 1;
            while true do
            begin
              sSecao := 'lacUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
              sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');

              if sFim = 'FIM' then
                break;

              IteminfunidTransp.lacUnidTransp.New.nLacre := sFim;

              inc(L);
            end;

            L := 1;
            while true do
            begin
              sSecao := 'infUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
              sFim   := INIRec.ReadString(sSecao,'idUnidCarga','FIM');

              if sFim = 'FIM' then
                break;

              ItemInfUnidCarga := IteminfunidTransp.infUnidCarga.New;

              ItemInfUnidCarga.tpUnidCarga := StrToUnidCarga(OK,INIRec.ReadString(sSecao,'tpUnidCarga','1'));
              ItemInfUnidCarga.idUnidCarga := sFim;
              ItemInfUnidCarga.qtdRat      := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);

              M := 1;
              while true do
              begin
                sSecao := 'lacUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3)+IntToStrZero(M,3);
                sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');

                if sFim = 'FIM' then
                  break;

                ItemInfUnidCarga.lacUnidCarga.New.nLacre := sFim;

                inc(M);
              end;

              inc(L);
            end;

            inc(K);
          end;

          Inc(J);
        end;

        J := 1;
        while true do
        begin
          sSecao := 'infMDFeTransp' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
          sFim   := INIRec.ReadString(sSecao, 'chMDFe', 'FIM');

          if sFim = 'FIM' then
            break;

          ItemInfMDFeTransp := IteminfMunDescarga.infMDFeTransp.New;

          ItemInfMDFeTransp.chMDFe := sFim;
          ItemInfMDFeTransp.indReentrega:= INIRec.ReadString(sSecao, 'indReentrega', '');

          K := 1;
          while true do
          begin
            sSecao := 'peri'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
            sFim   := INIRec.ReadString(sSecao,'nONU','FIM');

            if sFim = 'FIM' then
              break;

            ItemPeri := ItemInfMDFeTransp.peri.New;

            ItemPeri.nONU      := sFim;
            ItemPeri.xNomeAE   := INIRec.ReadString(sSecao,'xNomeAE','');
            ItemPeri.xClaRisco := INIRec.ReadString(sSecao,'xClaRisco','');
            ItemPeri.grEmb     := INIRec.ReadString(sSecao,'grEmb','');
            ItemPeri.qTotProd  := INIRec.ReadString(sSecao,'qTotProd','');
            ItemPeri.qVolTipo  := INIRec.ReadString(sSecao,'qVolTipo','');

            inc(K);
          end;

          K := 1;
          while true do
          begin
            sSecao := 'infUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3);
            sFim   := INIRec.ReadString(sSecao,'idUnidTransp','FIM');

            if sFim = 'FIM' then
              break;

            IteminfunidTransp := ItemInfMDFeTransp.infUnidTransp.New;

            IteminfunidTransp.tpUnidTransp := StrToUnidTransp(OK,INIRec.ReadString(sSecao,'tpUnidTransp','1'));
            IteminfunidTransp.idUnidTransp := sFim;
            IteminfunidTransp.qtdRat       := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);

            L := 1;
            while true do
            begin
              sSecao := 'lacUnidTransp'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
              sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');

              if sFim = 'FIM' then
                break;

              IteminfunidTransp.lacUnidTransp.New.nLacre := sFim;

              inc(L);
            end;

            L := 1;
            while true do
            begin
              sSecao := 'infUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3);
              sFim   := INIRec.ReadString(sSecao,'idUnidCarga','FIM');

              if sFim = 'FIM' then
                break;

              ItemInfUnidCarga := IteminfunidTransp.infUnidCarga.New;

              ItemInfUnidCarga.tpUnidCarga := StrToUnidCarga(OK,INIRec.ReadString(sSecao,'tpUnidCarga','1'));
              ItemInfUnidCarga.idUnidCarga := sFim;
              ItemInfUnidCarga.qtdRat      := StringToFloatDef( INIRec.ReadString(sSecao,'qtdRat',''),0);

              M := 1;
              while true do
              begin
                sSecao := 'lacUnidCarga'+IntToStrZero(I,3)+IntToStrZero(J,3)+IntToStrZero(K,3)+IntToStrZero(L,3)+IntToStrZero(M,3);
                sFim   := INIRec.ReadString(sSecao,'nLacre','FIM');

                if sFim = 'FIM' then
                  break;

                ItemInfUnidCarga.lacUnidCarga.New.nLacre := sFim;

                inc(M);
              end;

              inc(L);
            end;

            inc(K);
          end;

          Inc(J);
        end;

        Inc(I);
      end;

      I := 1;
      while true do
      begin
        sSecao := 'seg' + IntToStrZero(I, 3);

        if (INIRec.ReadString(sSecao, 'xSeg', '') = '') and
           (INIRec.ReadString(sSecao, 'nApol', '') = '') and
           not INIRec.SectionExists('aver' + IntToStrZero(I, 3) + '001') then
          Break;

        ItemSeg := seg.New;

        ItemSeg.respSeg :=  StrToRspSeguroMDFe(OK, INIRec.ReadString(sSecao, 'respSeg', '1'));
        ItemSeg.CNPJCPF := INIRec.ReadString(sSecao, 'CNPJCPF', '');
        ItemSeg.xSeg    := INIRec.ReadString(sSecao, 'xSeg', '');
        ItemSeg.CNPJ    := INIRec.ReadString(sSecao, 'CNPJ', '');
        ItemSeg.nApol   := INIRec.ReadString(sSecao, 'nApol', '');

        J := 1;
        while true do
        begin
          sSecao := 'aver' + IntToStrZero(I, 3) + IntToStrZero(J, 3);
          sFim   := INIRec.ReadString(sSecao, 'nAver', 'FIM');

          if sFim = 'FIM' then
            break;

          ItemSeg.aver.New.nAver := sFim;

          Inc(J);
        end;

        Inc(I);
      end;

      sSecao := 'prodPred';

      if INIRec.SectionExists(sSecao) then
      begin
        MDFe.prodPred.tpCarga := StrToTCarga(ok, INIRec.ReadString(sSecao, 'tpCarga', '01'));
        MDFe.prodPred.xProd   := INIRec.ReadString(sSecao, 'xProd', '');
        MDFe.prodPred.cEAN    := INIRec.ReadString(sSecao, 'cEAN', '');
        MDFe.prodPred.NCM     := INIRec.ReadString(sSecao, 'NCM', '');

        sSecao := 'infLocalCarrega';

        if INIRec.SectionExists(sSecao) then
        begin
          MDFe.prodPred.infLocalCarrega.CEP       := INIRec.ReadInteger(sSecao, 'CEP', 0);
          MDFe.prodPred.infLocalCarrega.latitude  := StringToFloatDef( INIRec.ReadString(sSecao,'latitude',''),0);
          MDFe.prodPred.infLocalCarrega.longitude := StringToFloatDef( INIRec.ReadString(sSecao,'longitude',''),0);
        end;

        sSecao := 'infLocalDescarrega';

        if INIRec.SectionExists(sSecao) then
        begin
          MDFe.prodPred.infLocalDescarrega.CEP       := INIRec.ReadInteger(sSecao, 'CEP', 0);
          MDFe.prodPred.infLocalDescarrega.latitude  := StringToFloatDef( INIRec.ReadString(sSecao,'latitude',''),0);
          MDFe.prodPred.infLocalDescarrega.longitude := StringToFloatDef( INIRec.ReadString(sSecao,'longitude',''),0);
        end;
      end;

      tot.qCTe   := INIRec.ReadInteger('tot', 'qCTe', 0);
      tot.qCT    := INIRec.ReadInteger('tot', 'qCT', 0);
      tot.qNFe   := INIRec.ReadInteger('tot', 'qNFe', 0);
      tot.qNF    := INIRec.ReadInteger('tot', 'qNF', 0);
      tot.qMDFe  := INIRec.ReadInteger('tot', 'qMDFe', 0);
      tot.vCarga := StringToFloatDef(INIRec.ReadString('tot', 'vCarga', ''), 0);
      tot.cUnid  := StrToUnidMed(OK, INIRec.ReadString('tot', 'cUnid', '01'));
      tot.qCarga := StringToFloatDef(INIRec.ReadString('tot', 'qCarga', ''), 0);

      I := 1;
      while true do
      begin
        sSecao := 'lacres' + IntToStrZero(I, 3);
        sFim   := INIRec.ReadString(sSecao, 'nLacre', 'FIM');

        if sFim = 'FIM' then
          break;

        lacres.New.nLacre := sFim;

        Inc(I);
      end;

      I := 1;
      while true do
      begin
        sSecao := 'autXML' + IntToStrZero(I, 2);
        sFim   := INIRec.ReadString(sSecao, 'CNPJCPF', 'FIM');

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
        begin
          sSecao := 'autXML' + IntToStrZero(I, 3);
          sFim   := INIRec.ReadString(sSecao, 'CNPJCPF', 'FIM');
        end;

        if (sFim = 'FIM') or (Length(sFim) <= 0) then
          break;

        autXML.New.CNPJCPF := sFim;

        Inc(I);
      end;

      infAdic.infCpl     := INIRec.ReadString('infAdic', 'infCpl', '');
      infAdic.infAdFisco := INIRec.ReadString('infAdic', 'infAdFisco', '');

      sSecao := 'infRespTec';
      if INIRec.SectionExists(sSecao) then
      begin
        infRespTec.CNPJ     := INIRec.ReadString(sSecao, 'CNPJ', '');
        infRespTec.xContato := INIRec.ReadString(sSecao, 'xContato', '');
        infRespTec.email    := INIRec.ReadString(sSecao, 'email', '');
        infRespTec.fone     := INIRec.ReadString(sSecao, 'fone', '');
      end;
    end;

    GerarXML;

    Result := True;
  finally
    INIRec.Free;
  end;
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
