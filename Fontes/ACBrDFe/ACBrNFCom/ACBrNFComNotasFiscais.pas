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

unit ACBrNFComNotasFiscais;

interface

uses
  Classes, SysUtils, StrUtils,
  ACBrXmlBase,
  ACBrNFComConfiguracoes, ACBrNFComClass,
  ACBrNFComIniReader, ACBrNFComIniWriter,
  ACBrNFComXmlReader, ACBrNFComXmlWriter;

type

  { TNotaFiscal }

  TNotaFiscal = class(TCollectionItem)
  private
    FNFCom: TNFCom;
    // Xml
    FNFComW: TNFComXmlWriter;
    FNFComR: TNFComXmlReader;
    // Ini
    FNFComIniR: TNFComIniReader;
    FNFComIniW: TNFComIniWriter;
    FConfiguracoes: TConfiguracoesNFCom;
    FXMLAssinado: string;
    FXMLOriginal: string;
    FAlertas: string;
    FErroValidacao: string;
    FErroValidacaoCompleto: string;
    FErroRegrasdeNegocios: string;
    FNomeArq: string;
    FNomeArqPDF: string;

    function GetConfirmada: Boolean;
    function GetcStat: Integer;
    function GetProcessada: Boolean;
    function GetCancelada: Boolean;

    function GetMsg: string;
    function GetNumID: string;
    function GetXMLAssinado: string;
    procedure SetXML(const AValue: string);
    procedure SetXMLOriginal(const AValue: string);
    function ValidarConcatChave: Boolean;
    function CalcularNomeArquivo: string;
    function CalcularPathArquivo: string;
  public
    constructor Create(Collection2: TCollection); override;
    destructor Destroy; override;
    procedure Imprimir;
    procedure ImprimirPDF;

    procedure Assinar;
    procedure Validar;
    function VerificarAssinatura: Boolean;
    function ValidarRegrasdeNegocios: Boolean;

    function LerXML(const AXML: string): Boolean;
    function LerArqIni(const AIniString: string): Boolean;
    function GerarNFComIni: string;

    function GerarXML: string;
    function GravarXML(const NomeArquivo: string = ''; const PathArquivo: string = ''): Boolean;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(const sPara, sAssunto: string; sMensagem: TStrings = nil;
      EnviaPDF: Boolean = True; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil);

    property NomeArq: string read FNomeArq write FNomeArq;
    property NomeArqPDF: string read FNomeArqPDF write FNomeArqPDF;
    function CalcularNomeArquivoCompleto(NomeArquivo: string = '';
      PathArquivo: string = ''): string;

    property NFCom: TNFCom read FNFCom;

    // Atribuir a "XML", faz o componente transferir os dados lido para as propriedades internas e "XMLAssinado"
    property XML: string read FXMLOriginal write SetXML;
    // Atribuir a "XMLOriginal", reflete em XMLAssinado, se existir a tag de assinatura
    // Sempre deve estar em UTF8
    property XMLOriginal: string read FXMLOriginal write SetXMLOriginal;
    // Sempre deve estar em UTF8
    property XMLAssinado: string read GetXMLAssinado write FXMLAssinado;
    property Confirmada: Boolean read GetConfirmada;
    property Processada: Boolean read GetProcessada;
    property Cancelada: Boolean read GetCancelada;
    property cStat: Integer read GetcStat;
    property Msg: string read GetMsg;
    property NumID: string read GetNumID;

    property Alertas: string read FAlertas;
    property ErroValidacao: string read FErroValidacao;
    property ErroValidacaoCompleto: string read FErroValidacaoCompleto;
    property ErroRegrasdeNegocios: string read FErroRegrasdeNegocios;
  end;

  { TNotasFiscais }

  TNotasFiscais = class(TOwnedCollection)
  private
    FACBrNFCom: TComponent;
    FConfiguracoes: TConfiguracoesNFCom;

    function GetItem(Index: integer): TNotaFiscal;
    procedure SetItem(Index: integer; const Value: TNotaFiscal);

    procedure VerificarDANFCom;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    procedure GerarNFCom;
    procedure Assinar;
    procedure Validar;
    function VerificarAssinatura(out Erros: string): Boolean;
    function ValidarRegrasdeNegocios(out Erros: string): Boolean;
    procedure Imprimir;
    procedure ImprimirCancelado;
    procedure ImprimirResumido;
    procedure ImprimirPDF;
    procedure ImprimirResumidoPDF;
    function Add: TNotaFiscal;
    function Insert(Index: integer): TNotaFiscal;

    property Items[Index: integer]: TNotaFiscal read GetItem write SetItem; default;

    function GetNamePath: string; override;
    // Incluido o Parametro AGerarNFCom que determina se após carregar os dados da NFCom
    // para o componente, será gerado ou não novamente o XML da NFCom.
    function LoadFromFile(const CaminhoArquivo: string; AGerarNFCom: Boolean = False): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarNFCom: Boolean = False): Boolean;
    function LoadFromString(const AXMLString: string; AGerarNFCom: Boolean = False): Boolean;
    function LoadFromIni(const AIniString: string): Boolean;

    function GerarIni: string;
    function GravarXML(const APathNomeArquivo: string = ''): Boolean;

    property ACBrNFCom: TComponent read FACBrNFCom;
  end;

implementation

uses
  dateutils, IniFiles,
  synautil,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.XMLHTML, ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBrDFeUtil,
  ACBrNFCom, ACBrNFComConversao,
  ACBrXmlDocument;

{ TNotaFiscal }

constructor TNotaFiscal.Create(Collection2: TCollection);
begin
  inherited Create(Collection2);

  FNFCom := TNFCom.Create;
  // Xml
  FNFComW := TNFComXmlWriter.Create(FNFCom);
  FNFComR := TNFComXmlReader.Create(FNFCom);
  // Ini
  FNFComIniR := TNFComIniReader.Create(FNFCom);
  FNFComIniW := TNFComIniWriter.Create(FNFCom);

  FConfiguracoes := TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom).Configuracoes;

  FNFCom.Ide.verProc := 'ACBrNFCom';

  with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
  begin
    FNFCom.Ide.modelo := 62;
    FNFCom.infNFCom.Versao := VersaoNFComToDbl(Configuracoes.Geral.VersaoDF);
    FNFCom.Ide.tpAmb   := TACBrTipoAmbiente(Configuracoes.WebServices.Ambiente);
    FNFCom.Ide.tpEmis  := TACBrTipoEmissao(Configuracoes.Geral.FormaEmissao);
  end;
end;

destructor TNotaFiscal.Destroy;
begin
  // Xml
  FNFComW.Free;
  FNFComR.Free;
  // Ini
  FNFComIniR.Free;
  FNFComIniW.Free;

  FNFCom.Free;

  inherited Destroy;
end;

procedure TNotaFiscal.Imprimir;
begin
  with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
  begin
    if not Assigned(DANFCom) then
      raise EACBrNFComException.Create('Componente DANFCom não associado.')
    else
      DANFCom.ImprimirDANFCom(NFCom);
  end;
end;

procedure TNotaFiscal.ImprimirPDF;
begin
  with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
  begin
    if not Assigned(DANFCom) then
      raise EACBrNFComException.Create('Componente DANFCom não associado.')
    else
      DANFCom.ImprimirDANFComPDF(NFCom);
  end;
end;

procedure TNotaFiscal.Assinar;
var
  XMLStr: string;
  XMLUTF8: AnsiString;
  Document: TACBrXmlDocument;
  ANode, SignatureNode, ReferenceNode, X509DataNode: TACBrXmlNode;
begin
  with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
  begin
    if not Assigned(SSL.AntesDeAssinar) then
      SSL.ValidarCNPJCertificado(NFCom.Emit.CNPJ);
  end;

  // Gera novamente, para processar propriedades que podem ter sido modificadas
  XMLStr := GerarXML;

  // XML já deve estar em UTF8, para poder ser assinado //
  XMLUTF8 := ConverteXMLtoUTF8(XMLStr);

  with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
  begin
    FXMLAssinado := SSL.Assinar(String(XMLUTF8), 'NFCom', 'infNFCom');
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

        NFCom.signature.URI := ObterConteudoTag(ReferenceNode.Attributes.Items['URI']);
        NFCom.signature.DigestValue := ObterConteudoTag(ReferenceNode.Childrens.FindAnyNs('DigestValue'), tcStr);
        NFCom.signature.SignatureValue := ObterConteudoTag(SignatureNode.Childrens.FindAnyNs('SignatureValue'), tcStr);
        NFCom.signature.X509Certificate := ObterConteudoTag(X509DataNode.Childrens.FindAnyNs('X509Certificate'), tcStr);
      end;
    finally
      FreeAndNil(Document);
    end;

//    with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
//    begin
      NFCom.infNFComSupl.qrCodNFCom := GetURLQRCode(NFCom);

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
  Erro, AXML: string;
  NotaEhValida: Boolean;
  ALayout: TLayOutNFCom;
  VerServ: Real;
begin
  AXML := FXMLAssinado;

  if AXML = '' then
    AXML := XMLOriginal;

  with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
  begin
    VerServ := FNFCom.infNFCom.Versao;
    ALayout := LayNFComRecepcao;

    // Extraindo apenas os dados da NFCom (sem nfComProc)
    AXML := ObterDFeXML(AXML, 'NFCom', ACBRNFCom_NAMESPACE);

    if EstaVazio(AXML) then
    begin
      Erro := ACBrStr('NFCom não encontrada no XML');
      NotaEhValida := False;
    end
    else
      NotaEhValida := SSL.Validar(AXML, GerarNomeArqSchema(ALayout, VerServ), Erro);

    if not NotaEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados da nota: ') +
        IntToStr(NFCom.Ide.nNF) + sLineBreak + FAlertas;
      FErroValidacaoCompleto := FErroValidacao + sLineBreak + Erro;

      raise EACBrNFComException.CreateDef(
        IfThen(Configuracoes.Geral.ExibirErroSchema, ErroValidacaoCompleto,
        ErroValidacao));
    end;
  end;
end;

function TNotaFiscal.VerificarAssinatura: Boolean;
var
  Erro, AXML: string;
  AssEhValida: Boolean;
begin
  AXML := FXMLAssinado;

  if AXML = '' then
    AXML := XMLOriginal;

  with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
  begin
    // Extraindo apenas os dados da NFCom (sem nfComProc)
    AXML := ObterDFeXML(AXML, 'NFCom', ACBRNFCom_NAMESPACE);

    if EstaVazio(AXML) then
    begin
      Erro := ACBrStr('NFCom não encontrada no XML');
      AssEhValida := False;
    end
    else
      AssEhValida := SSL.VerificarAssinatura(AXML, Erro, 'infNFCom');

    if not AssEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação da assinatura da nota: ') +
        IntToStr(NFCom.Ide.nNF) + sLineBreak + Erro;
    end;
  end;

  Result := AssEhValida;
end;

function TNotaFiscal.ValidarRegrasdeNegocios: Boolean;
var
  Erros: string;
  Inicio, Agora: TDateTime;

  procedure GravaLog(AString: string);
  begin
    //DEBUG
//    Log := Log + FormatDateTime('hh:nn:ss:zzz', Now) + ' - ' + AString + sLineBreak;
  end;

  procedure AdicionaErro(const Erro: string);
  begin
    Erros := Erros + Erro + sLineBreak;
  end;

begin
  // Converte o DateTime do Sistema para o TimeZone configurado,
  // para evitar divergência de Fuso Horário.
  Inicio := DataHoraTimeZoneModoDeteccao(TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom));
  // Aceita uma tolerância de até 5 minutos, devido ao sincronismo de horário
  // do servidor da Empresa e o servidor da SEFAZ.
  Agora := IncMinute(Inicio, 5);
  GravaLog('Inicio da Validação');

  with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
  begin
    Erros := '';

    GravaLog('Validar: 703-Data hora');
    if (NFCom.Ide.dhEmi > Agora) then
      AdicionaErro('703-Rejeição: Data-Hora de Emissão posterior ao horário de recebimento');
    {
      Incluir as regras aqui
    }
  end;

  Result := EstaVazio(Erros);

  if not Result then
  begin
    Erros := ACBrStr('Erro(s) nas Regras de negócios da nota: '+
                     IntToStr(NFCom.Ide.nNF) + sLineBreak + Erros);
  end;

  GravaLog('Fim da Validação. Tempo: ' +
           FormatDateTime('hh:nn:ss:zzz', Now - Inicio) + sLineBreak +
           'Erros:' + Erros);

  //DEBUG
  //WriteToTXT('c:\temp\Notafiscal.txt', Log);

  FErroRegrasdeNegocios := Erros;
end;

function TNotaFiscal.LerXML(const AXML: string): Boolean;
begin
  XMLOriginal := AXML;

  FNFComR.Arquivo := XMLOriginal;
  FNFComR.LerXml;
  Result := True;
end;

function TNotaFiscal.LerArqIni(const AIniString: string): Boolean;
begin
  FNFComIniR.VersaoDF := FConfiguracoes.Geral.VersaoDF;
  FNFComIniR.Ambiente := Integer(FConfiguracoes.WebServices.Ambiente);
  FNFComIniR.tpEmis := FConfiguracoes.Geral.FormaEmissaoCodigo;

  FNFComIniR.LerIni(AIniString);

  GerarXML;

  Result := True;
end;

function TNotaFiscal.GerarNFComIni: string;
begin
  Result := FNFComIniW.GravarIni;
end;

function TNotaFiscal.GravarXML(const NomeArquivo: string; const PathArquivo: string): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  FNomeArq := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);

  Result := TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom).Gravar(FNomeArq, FXMLOriginal);
end;

function TNotaFiscal.GravarStream(AStream: TStream): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FXMLOriginal));

  Result := True;
end;

procedure TNotaFiscal.EnviarEmail(const sPara, sAssunto: string; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings; sReplyTo: TStrings);
var
  NomeArq_temp : string;
  AnexosEmail:TStrings;
  StreamNFCom : TMemoryStream;
begin
  if not Assigned(TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom).MAIL) then
    raise EACBrNFComException.Create('Componente ACBrMail não associado');

  AnexosEmail := TStringList.Create;
  StreamNFCom := TMemoryStream.Create;
  try
    AnexosEmail.Clear;

    if Assigned(Anexos) then
      AnexosEmail.Assign(Anexos);

    with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
    begin
      Self.GravarStream(StreamNFCom);

      if (EnviaPDF) then
      begin
        if Assigned(DANFCom) then
        begin
          DANFCom.ImprimirDANFComPDF(FNFCom);
          NomeArq_temp := PathWithDelim(DANFCom.PathPDF) + NumID + '-NFCom.pdf';
          AnexosEmail.Add(NomeArq_temp);
        end;
      end;

      EnviarEmail(sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamNFCom,
                   NumID +'-NFCom.xml', sReplyTo);
    end;
  finally
    AnexosEmail.Free;
    StreamNFCom.Free;
  end;
end;

function TNotaFiscal.GerarXML: string;
var
  IdAnterior : string;
begin
  with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
  begin
    IdAnterior := NFCom.infNFCom.ID;

    FNFComW.Opcoes.FormatoAlerta  := Configuracoes.Geral.FormatoAlerta;
    FNFComW.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
    FNFComW.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
    FNFComW.Opcoes.IdentarXML := Configuracoes.Geral.IdentarXML;
    FNFComW.Opcoes.NormatizarMunicipios  := Configuracoes.Arquivos.NormatizarMunicipios;
    FNFComW.Opcoes.PathArquivoMunicipios := Configuracoes.Arquivos.PathArquivoMunicipios;

    TimeZoneConf.Assign(Configuracoes.WebServices.TimeZoneConf);

    {
      Ao gerar o XML as tags e atributos tem que ser exatamente os da configuração
    }
    {
    FNFComW.modeloDF := 62;
    FNFComW.VersaoDF := Configuracoes.Geral.VersaoDF;
    FNFComW.tpAmb   := TACBrTipoAmbiente(Configuracoes.WebServices.Ambiente);
    FNFComW.tpEmis  := TACBrTipoEmissao(Configuracoes.Geral.FormaEmissao);
    }
    FNFComW.idCSRT := Configuracoes.RespTec.IdCSRT;
    FNFComW.CSRT   := Configuracoes.RespTec.CSRT;
  end;

  FNFComW.GerarXml;
  //DEBUG
  //WriteToTXT('c:\temp\Notafiscal.xml', FNFComW.Document.Xml, False, False);

  XMLOriginal := FNFComW.Document.Xml;

  { XML gerado pode ter nova Chave e ID, então devemos calcular novamente o
    nome do arquivo, mantendo o PATH do arquivo carregado }
  if (NaoEstaVazio(FNomeArq) and (IdAnterior <> FNFCom.infNFCom.ID)) then
    FNomeArq := CalcularNomeArquivoCompleto('', ExtractFilePath(FNomeArq));

  FAlertas := ACBrStr(FNFComW.ListaDeAlertas.Text);

  Result := FXMLOriginal;
end;

function TNotaFiscal.CalcularNomeArquivo: string;
var
  xID: string;
  NomeXML: string;
begin
  xID := Self.NumID;

  if EstaVazio(xID) then
    raise EACBrNFComException.Create('ID Inválido. Impossível Salvar XML');

  NomeXML := '-NFCom.xml';

  Result := xID + NomeXML;
end;

function TNotaFiscal.CalcularPathArquivo: string;
var
  Data: TDateTime;
begin
  with TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom) do
  begin
    if Configuracoes.Arquivos.EmissaoPathNFCom then
      Data := FNFCom.Ide.dhEmi
    else
      Data := Now;

    Result := PathWithDelim(Configuracoes.Arquivos.GetPathNFCom(Data, FNFCom.Emit.CNPJ));
  end;
end;

function TNotaFiscal.CalcularNomeArquivoCompleto(NomeArquivo: string;
  PathArquivo: string): string;
var
  PathNoArquivo: string;
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
  chaveNFCom : string;
begin
  DecodeDate(NFCom.ide.dhEmi, wAno, wMes, wDia);

  chaveNFCom := OnlyNumber(NFCom.infNFCom.ID);
  {(*}
  Result := not
    ((Copy(chaveNFCom, 1, 2) <> IntToStrZero(NFCom.Ide.cUF, 2)) or
    (Copy(chaveNFCom, 3, 2)  <> Copy(FormatFloat('0000', wAno), 3, 2)) or
    (Copy(chaveNFCom, 5, 2)  <> FormatFloat('00', wMes)) or
    (Copy(chaveNFCom, 7, 14)<> PadLeft(OnlyNumber(NFCom.Emit.CNPJ), 14, '0')) or
    (Copy(chaveNFCom, 21, 2) <> IntToStrZero(NFCom.Ide.modelo, 2)) or
    (Copy(chaveNFCom, 23, 3) <> IntToStrZero(NFCom.Ide.serie, 3)) or
    (Copy(chaveNFCom, 26, 9) <> IntToStrZero(NFCom.Ide.nNF, 9)) or
    (Copy(chaveNFCom, 35, 1) <> TipoEmissaoToStr(NFCom.Ide.tpEmis)) or
    (Copy(chaveNFCom, 36, 1) <> SiteAutorizadorToStr(NFCom.Ide.nSiteAutoriz)) or
    (Copy(chaveNFCom, 37, 7) <> IntToStrZero(NFCom.Ide.cNF, 7)));
  {*)}
end;

function TNotaFiscal.GetConfirmada: Boolean;
begin
  Result := TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom).CstatConfirmada(
    FNFCom.procNFCom.cStat);
end;

function TNotaFiscal.GetcStat: Integer;
begin
  Result := FNFCom.procNFCom.cStat;
end;

function TNotaFiscal.GetProcessada: Boolean;
begin
  Result := TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom).CstatProcessado(
    FNFCom.procNFCom.cStat);
end;

function TNotaFiscal.GetCancelada: Boolean;
begin
  Result := TACBrNFCom(TNotasFiscais(Collection).ACBrNFCom).CstatCancelada(
    FNFCom.procNFCom.cStat);
end;

function TNotaFiscal.GetMsg: string;
begin
  Result := FNFCom.procNFCom.xMotivo;
end;

function TNotaFiscal.GetNumID: string;
begin
  Result := OnlyNumber(NFCom.infNFCom.ID);
end;

function TNotaFiscal.GetXMLAssinado: string;
begin
  if EstaVazio(FXMLAssinado) then
    Assinar;

  Result := FXMLAssinado;
end;

procedure TNotaFiscal.SetXML(const AValue: string);
begin
  LerXML(AValue);
end;

procedure TNotaFiscal.SetXMLOriginal(const AValue: string);
var
  XMLUTF8: string;
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
  if not (AOwner is TACBrNFCom) then
    raise EACBrNFComException.Create('AOwner deve ser do tipo TACBrNFCom');

  inherited Create(AOwner, ItemClass);

  FACBrNFCom := TACBrNFCom(AOwner);
  FConfiguracoes := TACBrNFCom(FACBrNFCom).Configuracoes;
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

procedure TNotasFiscais.GerarNFCom;
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

function TNotasFiscais.GetNamePath: string;
begin
  Result := 'NotaFiscal';
end;

procedure TNotasFiscais.VerificarDANFCom;
begin
  if not Assigned(TACBrNFCom(FACBrNFCom).DANFCom) then
    raise EACBrNFComException.Create('Componente DANFCom não associado.');
end;

procedure TNotasFiscais.Imprimir;
begin
  VerificarDANFCom;
  TACBrNFCom(FACBrNFCom).DANFCom.ImprimirDANFCom(nil);
end;

procedure TNotasFiscais.ImprimirCancelado;
begin
  VerificarDANFCom;
  TACBrNFCom(FACBrNFCom).DANFCom.ImprimirDANFComCancelado(nil);
end;

procedure TNotasFiscais.ImprimirResumido;
begin
  VerificarDANFCom;
  TACBrNFCom(FACBrNFCom).DANFCom.ImprimirDANFComResumido(nil);
end;

procedure TNotasFiscais.ImprimirPDF;
begin
  VerificarDANFCom;
  TACBrNFCom(FACBrNFCom).DANFCom.ImprimirDANFComPDF;
end;

procedure TNotasFiscais.ImprimirResumidoPDF;
begin
  VerificarDANFCom;
  TACBrNFCom(FACBrNFCom).DANFCom.ImprimirDANFComResumidoPDF(nil);
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

function TNotasFiscais.VerificarAssinatura(out Erros: string): Boolean;
var
  i: integer;
begin
  Result := True;
  Erros := '';

  if Self.Count < 1 then
  begin
    Erros := 'Nenhuma NFCom carregada';
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

function TNotasFiscais.ValidarRegrasdeNegocios(out Erros: string): Boolean;
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

function TNotasFiscais.LoadFromFile(const CaminhoArquivo: string;
  AGerarNFCom: Boolean): Boolean;
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
  Result := LoadFromString(String(XMLUTF8), AGerarNFCom);

  if Result then
  begin
    // Atribui Nome do arquivo a novas notas inseridas //
    for i := l to Self.Count - 1 do
      Self.Items[i].NomeArq := CaminhoArquivo;
  end;
end;

function TNotasFiscais.LoadFromStream(AStream: TStringStream;
  AGerarNFCom: Boolean): Boolean;
var
  AXML: AnsiString;
begin
  AStream.Position := 0;
  AXML := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(AXML), AGerarNFCom);
end;

function TNotasFiscais.LoadFromString(const AXMLString: string;
  AGerarNFCom: Boolean): Boolean;
var
  ANFComXML, XMLStr: AnsiString;
  P, N: integer;

  function PosNFCom: integer;
  begin
    Result := pos('</NFCom>', XMLStr);
  end;

begin
  // Verifica se precisa Converter de UTF8 para a string nativa da IDE //
  XMLStr := ConverteXMLtoNativeString(AXMLString);

  N := PosNFCom;
  while N > 0 do
  begin
    P := pos('</NFComProc>', XMLStr);

    if P = 0 then
      P := pos('</nfcomProc>', XMLStr);

    if P <= 0 then
      P := pos('</procNFCom>', XMLStr);  // NFCom obtida pelo Portal da Receita

    if P > 0 then
    begin
      ANFComXML := copy(XMLStr, 1, P + 11);
      XMLStr := Trim(copy(XMLStr, P + 12, length(XMLStr)));
    end
    else
    begin
      ANFComXML := copy(XMLStr, 1, N + 7);
      XMLStr := Trim(copy(XMLStr, N + 8, length(XMLStr)));
    end;

    with Self.Add do
    begin
      LerXML(ANFComXML);

      if AGerarNFCom then // Recalcula o XML
        GerarXML;
    end;

    N := PosNFCom;
  end;

  Result := Self.Count > 0;
end;

function TNotasFiscais.LoadFromIni(const AIniString: string): Boolean;
begin
  with Self.Add do
    LerArqIni(AIniString);

  Result := Self.Count > 0;
end;

function TNotasFiscais.GerarIni: string;
begin
  Result := '';

  if (Self.Count > 0) then
    Result := Self.Items[0].GerarNFComIni;
end;

function TNotasFiscais.GravarXML(const APathNomeArquivo: string): Boolean;
var
  i: integer;
  NomeArq, PathArq : string;
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
