{******************************************************************************}
{ Projeto: Componente ACBrCTe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Conhecimen-}
{ to de Transporte eletrônico - CTe - http://www.cte.fazenda.gov.br            }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Desenvolvimento                                                              }
{         de Cte: Wiliam Zacarias da Silva Rosa                                }
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

{$I ACBr.inc}

unit ACBrCTeConhecimentos;

interface

uses
  Classes, Sysutils, Dialogs, Forms, StrUtils,
  ACBrCTeConfiguracoes, ACBrDFeUtil,
  pcteCTe, pcteCTeR, pcteCTeW, pcnConversao, pcnAuxiliar, pcnLeitor;

type

  { Conhecimento }

  Conhecimento = class(TCollectionItem)
  private
    FCTe: TCTe;
    FCTeW: TCTeW;
    FCTeR: TCTeR;

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
    procedure SetXML(AValue: String);
    procedure SetXMLOriginal(AValue: String);
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

    function LerXML(AXML: String): Boolean;
    function GerarXML: String;
    function GravarXML(NomeArquivo: String = ''; PathArquivo: String = ''): Boolean;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(sPara, sAssunto: String; sMensagem: TStrings = nil;
      EnviaPDF: Boolean = True; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil);

    function CalcularNomeArquivoCompleto(NomeArquivo: String = '';
      PathArquivo: String = ''): String;

    property NomeArq: String read FNomeArq write FNomeArq;

    property CTe: TCTe read FCTe;

    // Atribuir a "XML", faz o componente transferir os dados lido para as propriedades internas e "XMLAssinado"
    property XML: String         read FXMLOriginal   write SetXML;
    // Atribuir a "XMLOriginal", reflete em XMLAssinado, se existir a tag de assinatura
    property XMLOriginal: String read FXMLOriginal   write SetXMLOriginal;
    property XMLAssinado: String read GetXMLAssinado write FXMLAssinado;

    property Confirmado: Boolean read GetConfirmado;
    property Processado: Boolean read GetProcessado;
    property Cancelado: Boolean  read GetCancelado;
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
    procedure ImprimirPDF;

    function  Add: Conhecimento;
    function Insert(Index: Integer): Conhecimento;

    property Items[Index: Integer]: Conhecimento read GetItem write SetItem;

    function GetNamePath: String; override;
    // Incluido o Parametro AGerarCTe que determina se após carregar os dados do CTe
    // para o componente, será gerado ou não novamente o XML do CTe.
    function LoadFromFile(CaminhoArquivo: String; AGerarCTe: Boolean = True): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarCTe: Boolean = True): Boolean;
    function LoadFromString(AXMLString: String; AGerarCTe: Boolean = True): Boolean;
    function GravarXML(PathNomeArquivo: String = ''): Boolean;

    property ACBrCTe: TComponent read FACBrCTe;
  end;

implementation

uses
  ACBrCTe, ACBrUtil, pcteConversaoCTe, synautil;

{ Conhecimento }

constructor Conhecimento.Create(Collection2: TCollection);
begin
  inherited Create(Collection2);

  FCTe := TCTe.Create;
  FCTeW := TCTeW.Create(FCTe);
  FCTeR := TCTeR.Create(FCTe);

  with TACBrCTe(TConhecimentos(Collection).ACBrCTe) do
  begin
    FCTe.Ide.modelo := StrToInt(ModeloCTeToStr(Configuracoes.Geral.ModeloDF));
    FCTe.infCTe.Versao := VersaoCTeToDbl(Configuracoes.Geral.VersaoDF);

    FCTe.Ide.tpCTe := tcNormal;
    FCTe.Ide.verProc := 'ACBrCTe';
    FCTe.Ide.tpAmb := Configuracoes.WebServices.Ambiente;
    FCTe.Ide.tpEmis := Configuracoes.Geral.FormaEmissao;

    if Assigned(DACTE) then
      FCTe.Ide.tpImp := DACTE.TipoDACTE;
  end;
end;

destructor Conhecimento.Destroy;
begin
  FCTe.Free;
  FCTeW.Free;
  FCTeR.Free;

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

procedure Conhecimento.Assinar;
var
  XMLStr: String;
  XMLUTF8: AnsiString;
  Leitor: TLeitor;
begin
  TACBrCTe(TConhecimentos(Collection).ACBrCTe).SSL.ValidarCNPJCertificado( CTe.Emit.CNPJ );

  // Gera novamente, para processar propriedades que podem ter sido modificadas
  XMLStr := GerarXML;

  // XML já deve estar em UTF8, para poder ser assinado //
  XMLUTF8 := ConverteXMLtoUTF8(XMLStr);

  with TACBrCTe(TConhecimentos(Collection).ACBrCTe) do
  begin
    if Configuracoes.Geral.ModeloDF = moCTe then
      FXMLAssinado := SSL.Assinar(String(XMLUTF8), 'CTe', 'infCte')
    else
      FXMLAssinado := SSL.Assinar(String(XMLUTF8), 'CTeOS', 'infCte');
      
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
  Erro, AXML, DeclaracaoXML, AXMLModal: String;
  CTeEhValido, ModalEhValido, ok: Boolean;
  ALayout: TLayOutCTe;
  Modelo: TModeloCTe;
begin
  AXML := XMLAssinado;

  AXMLModal := Trim(RetornarConteudoEntre(AXML, '<infModal', '</infModal>'));
  case TACBrCTe(TConhecimentos(Collection).ACBrCTe).IdentificaSchemaModal(AXML) of
   schcteModalAereo: begin
                       AXMLModal := '<aereo xmlns="' + ACBRCTE_NAMESPACE + '">' +
                                      Trim(RetornarConteudoEntre(AXML, '<aereo>', '</aereo>')) +
                                    '</aereo>';
                     end;
   schcteModalAquaviario: begin
                            AXMLModal := '<aquav xmlns="' + ACBRCTE_NAMESPACE + '">' +
                                           Trim(RetornarConteudoEntre(AXML, '<aquav>', '</aquav>')) +
                                         '</aquav>';
                          end;
   schcteModalDutoviario: begin
                            AXMLModal := '<duto xmlns="' + ACBRCTE_NAMESPACE + '">' +
                                           Trim(RetornarConteudoEntre(AXML, '<duto>', '</duto>')) +
                                         '</duto>';
                          end;
   schcteModalFerroviario: begin
                             AXMLModal := '<ferrov xmlns="' + ACBRCTE_NAMESPACE + '">' +
                                            Trim(RetornarConteudoEntre(AXML, '<ferrov>', '</ferrov>')) +
                                          '</ferrov>';
                           end;
   schcteModalRodoviario: begin
                            AXMLModal := '<rodo xmlns="' + ACBRCTE_NAMESPACE + '">' +
                                           Trim(RetornarConteudoEntre(AXML, '<rodo>', '</rodo>')) +
                                         '</rodo>';
                          end;
   schcteModalRodoviarioOS: begin
                              AXMLModal := '<rodoOS xmlns="' + ACBRCTE_NAMESPACE + '">' +
                                             Trim(RetornarConteudoEntre(AXML, '<rodoOS>', '</rodoOS>')) +
                                           '</rodoOS>';
                            end;
   schcteMultiModal: begin
                       AXMLModal := '<multimodal xmlns="' + ACBRCTE_NAMESPACE + '">' +
                                      Trim(RetornarConteudoEntre(AXML, '<multimodal>', '</multimodal>')) +
                                    '</multimodal>';
                     end;
  end;

  AXMLModal := '<?xml version="1.0" encoding="UTF-8" ?>' + AXMLModal;

  with TACBrCTe(TConhecimentos(Collection).ACBrCTe) do
  begin
    Modelo  := StrToModeloCTe(ok, IntToStr(FCTe.Ide.modelo));

    // Extraindo apenas os dados do CTe (sem cteProc)
    DeclaracaoXML := ObtemDeclaracaoXML(AXML);

    if Modelo = moCTe then
    begin
      ALayout := LayCTeRecepcao;
      AXML := DeclaracaoXML + '<CTe xmlns' +
              RetornarConteudoEntre(AXML, '<CTe xmlns', '</CTe>') +
              '</CTe>';
    end
    else begin
      ALayout := LayCTeRecepcaoOS;
      AXML := DeclaracaoXML + '<CTeOS xmlns' +
              RetornarConteudoEntre(AXML, '<CTeOS xmlns', '</CTeOS>') +
              '</CTeOS>';
    end;

    if ((FCTe.ide.tpCTe = tcNormal) or (FCTe.ide.tpCTe = tcSubstituto)) and
        (FCTe.Ide.tpServ <> tsTranspValores) then
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
  Erro, AXML: String;
  AssEhValida: Boolean;
begin
  AXML := XMLAssinado;

  with TACBrCTe(TConhecimentos(Collection).ACBrCTe) do
  begin
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
  Agora := Now;
  GravaLog('Inicio da Validação');

  with TACBrCTe(TConhecimentos(Collection).ACBrCTe) do
  begin
    Erros := '';

    GravaLog('Validar: 502-Chave de acesso');
    if not ValidarConcatChave then
      AdicionaErro(
        '502-Rejeição: Erro na Chave de Acesso - Campo Id não corresponde à concatenação dos campos correspondentes');

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

function Conhecimento.LerXML(AXML: String): Boolean;
var
  XMLStr: String;
begin
  XMLOriginal := AXML;  // SetXMLOriginal() irá verificar se AXML está em UTF8

  { Verifica se precisa converter "AXML" de UTF8 para a String nativa da IDE.
    Isso é necessário, para que as propriedades fiquem com a acentuação correta }
  XMLStr := ParseText(AXML, True, XmlEhUTF8(AXML));

  FCTeR.Leitor.Arquivo := XMLStr;
  FCTeR.LerXml;

  Result := True;
end;

function Conhecimento.GravarXML(NomeArquivo: String; PathArquivo: String): Boolean;
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

procedure Conhecimento.EnviarEmail(sPara, sAssunto: String; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings; sReplyTo: TStrings);
var
  NomeArq: String;
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
      GravarStream(StreamCTe);

      if (EnviaPDF) then
      begin
        if Assigned(DACTE) then
        begin
          DACTE.ImprimirDACTEPDF(CTe);
          NomeArq := PathWithDelim(DACTE.PathPDF) + NumID + '-cte.pdf';
          AnexosEmail.Add(NomeArq);
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

function Conhecimento.GerarXML: String;
var
  IdAnterior : String;
begin
  with TACBrCTe(TConhecimentos(Collection).ACBrCTe) do
  begin
    IdAnterior := CTe.infCTe.ID;
    FCTeW.Gerador.Opcoes.FormatoAlerta  := Configuracoes.Geral.FormatoAlerta;
    FCTeW.Gerador.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
    FCTeW.Gerador.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
    FCTeW.Gerador.Opcoes.IdentarXML := Configuracoes.Geral.IdentarXML;
    pcnAuxiliar.TimeZoneConf.Assign( Configuracoes.WebServices.TimeZoneConf );
  end;

  FCTeW.GerarXml;

  XMLOriginal := FCTeW.Gerador.ArquivoFormatoXML;  // SetXMLOriginal() irá converter para UTF8

  { XML gerado pode ter nova Chave e ID, então devemos calcular novamente o
    nome do arquivo, mantendo o PATH do arquivo carregado }
  if (NaoEstaVazio(FNomeArq) and (IdAnterior <> FCTe.infCTe.ID)) then
    FNomeArq := CalcularNomeArquivoCompleto('', ExtractFilePath(FNomeArq));

  FAlertas := FCTeW.Gerador.ListaDeAlertas.Text;
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

    Result := PathWithDelim(Configuracoes.Arquivos.GetPathCTe(Data, FCTe.Emit.CNPJ));
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

procedure Conhecimento.SetXML(AValue: String);
begin
  LerXML(AValue);
end;

procedure Conhecimento.SetXMLOriginal(AValue: String);
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

{ TConhecimentos }

constructor TConhecimentos.Create(AOwner: TPersistent;
  ItemClass: TCollectionItemClass);
begin
  if not (AOwner is TACBrCTe) then
     raise EACBrCTeException.Create('AOwner deve ser do tipo TACBrCTe');

  inherited;

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
  TACBrCTe(FACBrCTe).DACTe.ImprimirDACTePDF(nil);
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

function TConhecimentos.LoadFromFile(CaminhoArquivo: String;
  AGerarCTe: Boolean = True): Boolean;
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
  AGerarCTe: Boolean = True): Boolean;
var
  AXML: AnsiString;
begin
  AStream.Position := 0;
  AXML := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(AXML), AGerarCTe);
end;

function TConhecimentos.LoadFromString(AXMLString: String;
  AGerarCTe: Boolean = True): Boolean;
var
  ACTeXML, XMLStr: AnsiString;
  P, N: integer;

  function PosCTe: integer;
  begin
    Result := pos('</CTe>', XMLStr);
  end;

begin
  // Verifica se precisa Converter de UTF8 para a String nativa da IDE //
  XMLStr := ConverteXMLtoNativeString(AXMLString);

  N := PosCTe;
  while N > 0 do
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

function TConhecimentos.GravarXML(PathNomeArquivo: String): Boolean;
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
