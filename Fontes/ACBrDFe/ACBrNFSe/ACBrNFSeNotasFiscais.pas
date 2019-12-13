{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{  de Serviço eletrônica - NFSe                                                }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

unit ACBrNFSeNotasFiscais;

interface

uses
  Classes, SysUtils, Dialogs, Forms, StrUtils,
  ACBrNFSeConfiguracoes, ACBrDFeUtil, ACBrDFeSSL,
  pnfsNFSe, pnfsNFSeR, pnfsNFSeW, pcnConversao, pcnAuxiliar, pcnLeitor;

type

  { NotaFiscal }

  NotaFiscal = class(TCollectionItem)
  private
    FNFSe: TNFSe;
    FNFSeW: TNFSeW;
    FNFSeR: TNFSeR;

    FXMLNFSe: String;
    FXMLAssinado: String;
    FXMLOriginal: String;
    FAlertas: String;
    FErroRegrasdeNegocios: String;
    FNomeArq: String;
    FNomeArqRps: String;
    FConfirmada: Boolean;

    function GetProcessada: Boolean;

    function GetMsg: String;
    function CalcularNomeArquivo: String;
    function CalcularPathArquivo: String;
    function CalcularNomeArquivoCompleto(NomeArquivo: String = '';
      PathArquivo: String = ''): String;

    procedure Assinar(Assina: Boolean);
    function GetXMLAssinado: String;
    procedure SetXML(const Value: String);
    procedure SetXMLOriginal(const Value: String);

    procedure AssinaturaAdicional;

    function CorrigirAssinatura(const AXML: string): string;
  public
    constructor Create(Collection2: TCollection); override;
    destructor Destroy; override;
    procedure Imprimir;
    procedure ImprimirPDF;

    function VerificarAssinatura: Boolean;
    function ValidarRegrasdeNegocios: Boolean;

    function LerXML(const AXML: AnsiString): Boolean;

    function GerarXML: String;
    function GravarXML(const NomeArquivo: String = ''; const PathArquivo: String = ''): Boolean;

    function GravarStream(AStream: TStream): Boolean;

    procedure EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings = nil;
      EnviaPDF: Boolean = True; sCC: TStrings = nil; Anexos: TStrings = nil;
      sReplyTo: TStrings = nil; ManterPDFSalvo: Boolean = True);

    property NomeArq: String    read FNomeArq    write FNomeArq;
    property NomeArqRps: String read FNomeArqRps write FNomeArqRps;

    property NFSe: TNFSe read FNFSe;

    // Atribuir a "XML", faz o componente transferir os dados lido para as propriedades internas e "XMLAssinado"
    property XML: String         read FXMLOriginal   write SetXML;
    // Atribuir a "XMLOriginal", reflete em XMLAssinado, se existir a tag de assinatura
    property XMLOriginal: String read FXMLOriginal   write SetXMLOriginal;
    property XMLAssinado: String read GetXMLAssinado write FXMLAssinado;

    property XMLNFSe: String read FXMLNFSe write FXMLNFSe;

    property Confirmada: Boolean read FConfirmada write FConfirmada;
    property Processada: Boolean read GetProcessada;
    property Msg: String read GetMsg;
    property Alertas: String read FAlertas;
    property ErroRegrasdeNegocios: String read FErroRegrasdeNegocios;
  end;

  { TNotasFiscais }

  TNotasFiscais = class(TOwnedCollection)
  private
    FTransacao: Boolean;
    FNumeroLote: String;
    FACBrNFSe: TComponent;
    FConfiguracoes: TConfiguracoesNFSe;
    FXMLLoteOriginal: String;
    FXMLLoteAssinado: String;
    FErroValidacao: String;
    FErroValidacaoCompleto: String;
    FAlertas: String;

    function GetItem(Index: integer): NotaFiscal;
    procedure SetItem(Index: integer; const Value: NotaFiscal);

    procedure VerificarDANFSE;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    procedure GerarNFSe;
    function VerificarAssinatura(out Erros: String): Boolean;
    function ValidarRegrasdeNegocios(out Erros: String): Boolean;

    procedure Assinar(Assina: Boolean);
    // Usado para assinar o Lote de RPS
    function AssinarLote(const XMLLote, docElemento, infElemento: String;
      Assina: Boolean; const SignatureNode: String = ''; const SelectionNamespaces: String = '';
      const IdSignature: String = ''  ): String;
    // Usado para assinar os XMLs de Consulta e Cancelamento
    function AssinarXML(const AXML, docElemento, infElemento: String;
      Assina: Boolean; const SignatureNode: String = ''; const SelectionNamespaces: String = '';
      const IdSignature: String = ''  ): String;

    procedure ValidarLote(const XMLLote, NomeArqSchema: String);
    procedure Imprimir;
    procedure ImprimirPDF;

    function Add: NotaFiscal;
    function Insert(Index: integer): NotaFiscal;

    property Items[Index: integer]: NotaFiscal read GetItem write SetItem; default;

    function GetNamePath: String; override;
    // Incluido o Parametro AGerarNFSe que determina se após carregar os dados da NFSe
    // para o componente, será gerado ou não novamente o XML da NFSe.
    function LoadFromFile(const CaminhoArquivo: String; AGerarNFSe: Boolean = True): Boolean;
    function LoadFromStream(AStream: TStringStream; AGerarNFSe: Boolean = True): Boolean;
    function LoadFromString(AXMLString: String; AGerarNFSe: Boolean = True): Boolean;
    function GravarXML(const PathNomeArquivo: String = ''): Boolean;

    property XMLLoteOriginal: String read FXMLLoteOriginal write FXMLLoteOriginal;
    property XMLLoteAssinado: String read FXMLLoteAssinado write FXMLLoteAssinado;
    property ErroValidacao: String read FErroValidacao;
    property ErroValidacaoCompleto: String read FErroValidacaoCompleto;
    property Alertas: String read FAlertas;
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
    DANFSE.Provedor := FNFSeR.Provedor;
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
    DANFSE.Provedor := FNFSeR.Provedor;
    if not Assigned(DANFSE) then
      raise EACBrNFSeException.Create('Componente DANFSE não associado.')
    else
      DANFSE.ImprimirDANFSEPDF(NFSe);
  end;
end;

procedure NotaFiscal.Assinar(Assina: Boolean);
var
  XMLStr, DocElemento, InfElemento, IdAttr: String;
  XMLUTF8: AnsiString;
  Ok: Boolean;
begin
  // Verifica se foi informado o Numero de Série do Certificado.
  if ( TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe).SSL.NumeroSerie <> '' ) then
  begin
    // Verifica somente se for ambiente de produção.
    // Tem provedor que devemos informar um CNPJ de emitente padrão para testes.
    if TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe).Configuracoes.WebServices.Ambiente = taProducao then
    begin
      with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
      begin
        if not Assigned(SSL.AntesDeAssinar) then
          SSL.ValidarCNPJCertificado( NFSe.Prestador.CNPJ );
      end;
    end;
  end;

  // Gera novamente, para processar propriedades que podem ter sido modificadas
  XMLStr := GerarXML;

  // XML já deve estar em UTF8, para poder ser assinado //
  XMLUTF8 := ConverteXMLtoUTF8(XMLStr);
  FXMLOriginal := XMLUTF8;

  with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
  begin
    case StrToVersaoNFSe(Ok, Configuracoes.Geral.ConfigXML.VersaoXML) of
      ve110,
      ve200: InfElemento := Configuracoes.Geral.ConfigGeral.Prefixo4 + 'InfDeclaracaoPrestacaoServico';

    // Os RPS versão 1.00 tem como InfElement = InfRps
    else
      InfElemento := Configuracoes.Geral.ConfigGeral.Prefixo4 + 'InfRps';
    end;

    case Configuracoes.Geral.Provedor of
      proIPM:     DocElemento := 'nfse';
      proNotaBlu: DocElemento := 'RPS';
      proSMARAPD: DocElemento := 'tbnfd';
      proGiap:    DocElemento := 'nfe';
      proInfiscv11: DocElemento := 'infNFSe';
    else
      DocElemento := 'Rps';
    end;

    case Configuracoes.Geral.Provedor of
      proEGoverneISS: InfElemento := Configuracoes.Geral.ConfigGeral.Prefixo4 + 'NotaFiscal';

      pro4R:          InfElemento := 'Rps';

      proCTA,
      proNotaBlu:     InfElemento := 'RPS';

      proIPM:         InfElemento := 'nfse';

      proSMARAPD:     InfElemento := 'nfd';

      proGiap:        InfElemento := 'notaFiscal';
    end;

    if Configuracoes.Geral.ConfigAssinar.URI then
      IdAttr := Configuracoes.Geral.ConfigGeral.Identificador
    else
      IdAttr := '';

    if Assina then
      FXMLAssinado := SSL.Assinar(String(XMLUTF8), DocElemento, InfElemento,
                                  '', '', '', IdAttr)
    else
      FXMLAssinado := XMLOriginal;

    FXMLAssinado := CorrigirAssinatura(FXMLAssinado);

    if Configuracoes.Arquivos.Salvar and
      (not Configuracoes.Arquivos.SalvarApenasNFSeProcessadas)  then
    begin
      if NaoEstaVazio(NomeArqRps) then
        Gravar(NomeArqRps, FXMLAssinado)
      else
      begin
        FNomeArqRps := CalcularNomeArquivoCompleto(NomeArqRps, '');
        Gravar(NomeArqRps, ifThen(Assina, FXMLAssinado, FXMLOriginal));
      end;
    end;
  end;
end;

function NotaFiscal.VerificarAssinatura: Boolean;
//var
//  Erro, AXML: String;
//  AssEhValida: Boolean;
begin
(*
  AXML := FXMLOriginal;

  if EstaVazio(AXML) then
  begin

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
*)
  Result := True;  
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
      if (NFSe.Ide.indPres = pcEntregaDomicilio) then //B25b-10
        AdicionaErro('794-Rejeição: NF-e com indicativo de NFC-e com entrega a domicílio');
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

function NotaFiscal.LerXML(const AXML: AnsiString): Boolean;
begin
  FNFSeR.Leitor.Arquivo := AXML;
  with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
  begin
    FNFSeR.Provedor       := Configuracoes.Geral.Provedor;
    FNFSeR.ProvedorConf   := Configuracoes.Geral.Provedor;
    FNFSeR.PathIniCidades := Configuracoes.Geral.PathIniCidades;
    FNFSeR.TabServicosExt := Configuracoes.Arquivos.TabServicosExt;
    FNFSeR.VersaoXML      := Configuracoes.Geral.ConfigXML.VersaoXML; //Alterado Dalvan
  end;
  FNFSeR.LerXml;

  FXMLOriginal := String(AXML);

  Result := True;
end;

function NotaFiscal.GravarXML(const NomeArquivo: String; const PathArquivo: String): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  FNomeArqRps := CalcularNomeArquivoCompleto(NomeArquivo, PathArquivo);
  Result := TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe).Gravar(FNomeArqRps, FXMLOriginal);
end;

function NotaFiscal.GravarStream(AStream: TStream): Boolean;
begin
  if EstaVazio(FXMLOriginal) then
    GerarXML;

  AStream.Size := 0;
  WriteStrToStream(AStream, AnsiString(FXMLOriginal));
  Result := True;
end;

procedure NotaFiscal.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  EnviaPDF: Boolean; sCC: TStrings; Anexos: TStrings; sReplyTo: TStrings;
  ManterPDFSalvo: Boolean);
var
  NomeArqTemp: String;
  AnexosEmail:TStrings;
  StreamNFSe: TMemoryStream;
begin
  if not Assigned(TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe).MAIL) then
    raise EACBrNFSeException.Create('Componente ACBrMail não associado');

  AnexosEmail := TStringList.Create;
  StreamNFSe  := TMemoryStream.Create;
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
          NomeArqTemp := DANFSE.ArquivoPDF;
          AnexosEmail.Add(NomeArqTemp);
        end;
      end;

      EnviarEmail( sPara, sAssunto, sMensagem, sCC, AnexosEmail, StreamNFSe,
                   NumID[FNFSe] +'-nfse.xml', sReplyTo);
    end;
  finally
    if not ManterPDFSalvo then
      DeleteFile(NomeArqTemp);

    AnexosEmail.Free;
    StreamNFSe.Free;
  end;
end;

function NotaFiscal.GerarXML: String;
var
  Ok: Boolean;
begin
  with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
  begin
    FNFSeW.LayOutXML := ProvedorToLayoutXML(Configuracoes.Geral.Provedor);

    FNFSeW.NFSeWClass.Provedor      := Configuracoes.Geral.Provedor;
    FNFSeW.NFSeWClass.Prefixo3      := Configuracoes.Geral.ConfigGeral.Prefixo3;
    FNFSeW.NFSeWClass.Prefixo4      := Configuracoes.Geral.ConfigGeral.Prefixo4;
    FNFSeW.NFSeWClass.Identificador := Configuracoes.Geral.ConfigGeral.Identificador;
    FNFSeW.NFSeWClass.QuebradeLinha := Configuracoes.Geral.ConfigGeral.QuebradeLinha;
    FNFSeW.NFSeWClass.URL           := Configuracoes.Geral.ConfigXML.NameSpace;
    FNFSeW.NFSeWClass.VersaoNFSe    := StrToVersaoNFSe(Ok, Configuracoes.Geral.ConfigXML.VersaoXML);
    FNFSeW.NFSeWClass.DefTipos      := Configuracoes.Geral.ConfigSchemas.DefTipos;
    FNFSeW.NFSeWClass.ServicoEnviar := Configuracoes.Geral.ConfigSchemas.ServicoEnviar;
    FNFSeW.NFSeWClass.VersaoDados   := Configuracoes.Geral.ConfigXML.VersaoDados;
    FNFSeW.NFSeWClass.Ambiente      := Configuracoes.WebServices.Ambiente;

    FNFSeW.NFSeWClass.Gerador.Opcoes.FormatoAlerta  := Configuracoes.Geral.FormatoAlerta;
    FNFSeW.NFSeWClass.Gerador.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
    FNFSeW.NFSeWClass.Gerador.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
    FNFSeW.NFSeWClass.Gerador.Opcoes.IdentarXML     := Configuracoes.Geral.IdentarXML;

    pcnAuxiliar.TimeZoneConf.Assign( Configuracoes.WebServices.TimeZoneConf );
  end;

  FNFSeW.GerarXml;
  XMLOriginal  := FNFSeW.NFSeWClass.Gerador.ArquivoFormatoXML;
  FXMLAssinado := '';

  FAlertas := FNFSeW.NFSeWClass.Gerador.ListaDeAlertas.Text;

  Result := XMLOriginal;
end;

function NotaFiscal.CalcularNomeArquivo: String;
var
  xID: String;
begin
  xID := TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe).NumID[NFSe];

  if EstaVazio(xID) then
    raise EACBrNFSeException.Create('ID Inválido. Impossível Salvar XML');

  Result := xID + '-rps.xml';
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

    Result := PathWithDelim(Configuracoes.Arquivos.GetPathRPS(Data, FNFSe.Prestador.Cnpj, FNFSe.Prestador.InscricaoEstadual));
  end;
end;

function NotaFiscal.CorrigirAssinatura(const AXML: string): string;
var
  XML:string;
begin
  XML := StringReplace(AXML,
      '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"></CanonicalizationMethod>',
      '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/>', [rfReplaceAll]);

  XML := StringReplace(XML,
      '<SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1"></SignatureMethod>',
      '<SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1"/>', [rfReplaceAll]);

  XML := StringReplace(XML,
      '<Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature"></Transform>',
      '<Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature"/>', [rfReplaceAll]);

  XML := StringReplace(XML,
      '<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"></Transform>',
      '<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/>', [rfReplaceAll]);

  XML := StringReplace(XML,
      '<DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1"></DigestMethod>',
      '<DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1"/>', [rfReplaceAll]);

  Result := XML;
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

function NotaFiscal.GetProcessada: Boolean;
begin
//  Result := TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe).CstatProcessado(
//    FNFSe.procNFSe.cStat);
  Result := True;
end;

function NotaFiscal.GetMsg: String;
begin
//  Result := FNFSe.procNFSe.xMotivo;
  Result := '';
end;

function NotaFiscal.GetXMLAssinado: String;
begin
//  if EstaVazio(FXMLAssinado) then
//    Assinar;

  Result := FXMLAssinado;
end;

procedure NotaFiscal.SetXML(const Value: String);
begin
  LerXML(Value);
end;

procedure NotaFiscal.SetXMLOriginal(const Value: String);
begin
  FXMLOriginal := Value;

  if XmlEstaAssinado(FXMLOriginal) then
    FXMLAssinado := FXMLOriginal
  else
    FXMLAssinado := '';
end;

procedure NotaFiscal.AssinaturaAdicional;
var
  sSituacao, sISSRetido, sCPFCNPJTomador, sIndTomador, sTomador,
  sCPFCNPJInter, sIndInter, sISSRetidoInter, sInter, sAssinatura: String;
begin
  sSituacao := EnumeradoToStr(NFSe.Status, ['N', 'C'], [srNormal, srCancelado]);

  sISSRetido := EnumeradoToStr(NFSe.Servico.Valores.IssRetido,
                               ['N', 'S'], [stNormal, stRetencao]);

  // Tomador do Serviço
  sCPFCNPJTomador := OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj);

  if Length(sCPFCNPJTomador) = 11 then
    sIndTomador := '1'
  else
    if Length(sCPFCNPJTomador) = 14 then
      sIndTomador := '2'
    else
      sIndTomador := '3';

  sTomador := sIndTomador + Poem_Zeros(sCPFCNPJTomador, 14);
  (*
  if sIndTomador <> '3' then
    sTomador := sIndTomador + Poem_Zeros(sCPFCNPJTomador, 14)
  else
    sTomador := '';
  *)
  
  // Prestador Intermediario
  sCPFCNPJInter := OnlyNumber(NFSe.IntermediarioServico.CpfCnpj);

  if Length(sCPFCNPJInter) = 11 then
    sIndInter := '1'
  else
    if Length(sCPFCNPJInter) = 14 then
      sIndInter := '2'
    else
      sIndInter := '3';

  sISSRetidoInter := EnumeradoToStr(NFSe.IntermediarioServico.IssRetido,
                                    ['N', 'S'], [stNormal, stRetencao]);

  if sIndInter <> '3' then
    sInter := sIndInter + Poem_Zeros(sCPFCNPJInter, 14) + sISSRetidoInter
  else
    sInter := '';

  sAssinatura := Poem_Zeros(NFSe.Prestador.InscricaoMunicipal, 8) +
                 PadRight(NFSe.IdentificacaoRps.Serie, 5 , ' ') +
                 Poem_Zeros(NFSe.IdentificacaoRps.Numero, 12) +
                 FormatDateTime('yyyymmdd', NFse.DataEmissao) +
                 TTributacaoRPSToStr(NFSe.TipoTributacaoRPS) +
                 sSituacao +
                 sISSRetido +
                 Poem_Zeros(OnlyNumber(FormatFloat('#0.00', NFSe.Servico.Valores.ValorServicos)), 15 ) +
                 Poem_Zeros(OnlyNumber(FormatFloat('#0.00', NFSe.Servico.Valores.ValorDeducoes)), 15 ) +
                 Poem_Zeros(OnlyNumber(NFSe.Servico.ItemListaServico ), 5 ) +
                 sTomador +
                 sInter;

  with TACBrNFSe(TNotasFiscais(Collection).ACBrNFSe) do
    NFSe.Assinatura := SSL.CalcHash(sAssinatura, dgstSHA1, outBase64, True);
end;

{ TNotasFiscais }

constructor TNotasFiscais.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  if not (AOwner is TACBrNFSe) then
    raise EACBrNFSeException.Create('AOwner deve ser do tipo TACBrNFSe');

  inherited Create(AOwner, ItemClass);

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
  begin
    if Self.FConfiguracoes.Geral.Provedor in [proSP, proNotaBlu] then
      Self.Items[i].AssinaturaAdicional;

    Self.Items[i].Assinar(Assina);
  end;
end;

function TNotasFiscais.AssinarLote(const XMLLote, docElemento, infElemento: String;
  Assina: Boolean; const SignatureNode: String; const SelectionNamespaces: String;
  const IdSignature: String): String;
var
  XMLAss, ArqXML, IdAttr: String;
begin
  // XMLLote já deve estar em UTF8, para poder ser assinado //
  ArqXML := ConverteXMLtoUTF8(XMLLote);
  FXMLLoteOriginal := ArqXML;
  Result := FXMLLoteOriginal;

  with TACBrNFSe(FACBrNFSe) do
  begin
    if Configuracoes.Geral.ConfigAssinar.URI then
      IdAttr := Configuracoes.Geral.ConfigGeral.Identificador
    else
      IdAttr := '';

    if Assina then
    begin
      XMLAss := SSL.Assinar(ArqXML, docElemento, infElemento,
                            SignatureNode, SelectionNamespaces, IdSignature, IdAttr);
      FXMLLoteAssinado := XMLAss;

      FXMLLoteAssinado := Self.Items[0].CorrigirAssinatura(FXMLLoteAssinado);

      Result := FXMLLoteAssinado;
    end;
  end;
end;

function TNotasFiscais.AssinarXML(const AXML, docElemento, infElemento: String;
  Assina: Boolean; const SignatureNode, SelectionNamespaces, IdSignature: String): String;
var
  XMLAss, ArqXML, IdAttr: String;
begin
  if AXML = '' then
    exit;

  // AXML já deve estar em UTF8, para poder ser assinado //
  ArqXML := ConverteXMLtoUTF8(AXML);
  Result := ArqXML;

  with TACBrNFSe(FACBrNFSe) do
  begin
    if Configuracoes.Geral.ConfigAssinar.URI then
      IdAttr := Configuracoes.Geral.ConfigGeral.Identificador
    else
      IdAttr := '';

    if Assina then
    begin
      XMLAss := SSL.Assinar(ArqXML, docElemento, infElemento,
                            SignatureNode, SelectionNamespaces, IdSignature, IdAttr);

      XMLAss := Self.Items[0].CorrigirAssinatura(XMLAss);

      Result := XMLAss;
    end;
  end;
end;

procedure TNotasFiscais.ValidarLote(const XMLLote, NomeArqSchema: String);
var
  Erro, AXML: String;
  NotaEhValida: Boolean;
begin
  AXML := XMLLote;

//    raise EACBrNFSeException.Create(AXML);

  with TACBrNFSe(FACBrNFSe) do
  begin
    NotaEhValida := SSL.Validar(AXML, NomeArqSchema, Erro);

    if not NotaEhValida then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados do lote: ') +
        NumeroLote + sLineBreak + FAlertas ;
      FErroValidacaoCompleto := FErroValidacao + sLineBreak + Erro;

      raise EACBrNFSeException.CreateDef(
        IfThen(Configuracoes.Geral.ExibirErroSchema, ErroValidacaoCompleto,
        ErroValidacao));
    end;
  end;
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

function TNotasFiscais.VerificarAssinatura(out Erros: String): Boolean;
//var
//  i: integer;
//  Erro: String;
begin
  Result := True;
  (*
  Erros := '';

  for i := 0 to Self.Count - 1 do
  begin
    if not Self.Items[i].VerificarAssinatura then
    begin
      Result := False;
      Erros := Erros + Self.Items[i].ErroValidacao + sLineBreak;
    end;
  end;
*)
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
  AGerarNFSe: Boolean = True): Boolean;
var
  XMLStr: String;
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

  // Converte de UTF8 para a String nativa da IDE //
  XMLStr := DecodeToString(XMLUTF8, True);
  Result := LoadFromString(XMLStr, AGerarNFSe);

  if Result then
  begin
    // Atribui Nome do arquivo a novas notas inseridas //
    for i := l to Self.Count - 1 do
    begin
      if Pos('-rps.xml', CaminhoArquivo) > 0 then
        Self.Items[i].NomeArqRps := CaminhoArquivo
      else
        Self.Items[i].NomeArq := CaminhoArquivo;
    end;
  end;
end;

function TNotasFiscais.LoadFromStream(AStream: TStringStream;
  AGerarNFSe: Boolean = True): Boolean;
var
  AXML: AnsiString;
begin
  AStream.Position := 0;
  AXML := ReadStrFromStream(AStream, AStream.Size);

  Result := Self.LoadFromString(String(AXML), AGerarNFSe);
end;

function TNotasFiscais.LoadFromString(AXMLString: String;
  AGerarNFSe: Boolean = True): Boolean;
var
  AProvedor: TnfseProvedor;
  VersaoNFSe: TVersaoNFSe;
  Ok: Boolean;
  AXML: AnsiString;
  N, TamTAG, i: integer;
  TagF: Array[1..12] of String;

  function PosNFSe: Integer;
  begin
    TagF[01] := '</CompNfse>';
    TagF[02] := '</ComplNfse>';
    TagF[03] := '</NFS-e>';
    TagF[04] := '</Nfse>';
    TagF[05] := '</nfse>'; // IPM
    TagF[06] := '</Nota>';
    TagF[07] := '</NFe>';
    TagF[08] := '</tbnfd>';
    TagF[09] := '</nfs>';
    TagF[10] := '</nfeRpsNotaFiscal>'; // Provedor EL
    TagF[11] := '</notasFiscais>';     // Provedor EL
    TagF[12] := '</notaFiscal>';       // Provedor GIAP

    i := 0;

    repeat
      inc(i);
      TamTAG := Length(TagF[i]) -1;
      Result := Pos(TagF[i], AXMLString);
    until (i = High(TagF)) or (Result <> 0);

  end;

  //provedor SimplISS, FISSLex
  function PosNFSeCancelamento: Integer;
  begin
    TamTAG := 18;
    Result := Pos('</NfseCancelamento>', AXMLString);
    if Result = 0 then
      Result := Pos('</CancelamentoNfse>', AXMLString);
  end;

  function PosRPS(AProvedor: TnfseProvedor): Integer;
  begin
    TamTAG := 5;
    if (VersaoNFSe < ve200) and (AProvedor <> proAgili) then
    begin
      Result := Pos('</Rps>', AXMLString);
      // Provedor ISSDSF
      if Result = 0 then
        Result := Pos('</RPS>', AXMLString);

      if Result = 0 then  //Equiplano
        Result := Pos('</rps>', AXMLString);

      // Provedor Governa
      if ((Result = 0) and (AProvedor = proGoverna)) then
      begin
        Result := Pos('</LoteRps>', AXMLString);
        TamTAG := 9;
      end;
    end
    else
    begin
      // Se a versão do XML do RPS for 2.00 ou posterior existem 2 TAGs <Rps>,
      // neste caso devemos buscar a posição da segunda.
      Result := Pos('</Rps>', AXMLString);
      Result := PosEx('</Rps>', AXMLString, Result + 1);
    end;
  end;

begin
  with TACBrNFSe(FACBrNFSe) do
  begin
    AProvedor := Configuracoes.Geral.Provedor;

    VersaoNFSe := StrToVersaoNFSe(Ok, Configuracoes.Geral.ConfigXML.VersaoXML);

    AXMLString := StringReplace(StringReplace( AXMLString, '&lt;', '<', [rfReplaceAll]), '&gt;', '>', [rfReplaceAll]);
  end;

  if AProvedor <> proISSCuritiba then
    AXMLString := RetirarPrefixos(AXMLString, AProvedor);

  N := PosNFSe;

  if N > 0 then
  begin
    // Ler os XMLs das NFS-e
    while N > 0 do
    begin
      AXML := copy(AXMLString, 1, N + TamTAG);
      AXMLString := Trim(copy(AXMLString, N + TamTAG + 1, length(AXMLString)));

      // Abaixo a lista de provedores cujo grupo NfseCancelamento não se encontra
      // dentro do grupo CompNfse
      if AProvedor in [proSimplISS, {proBetha,} proTecnos, proFISSLEX] then
      begin
        N:= PosNFSeCancelamento;
        if N > 0 then
        begin
          // concatena o grupo NfseCancelamento abaixo do grupo Nfse
          AXML:= AXML + copy(AXMLString, 1, N + TamTAG);
          AXMLString := Trim(copy(AXMLString, N + TamTAG + 1, length(AXMLString)));
        end;
      end;

      with Self.Add do
      begin
        LerXML(AXML);
      end;

      N := PosNFSe;
    end;
  end
  else
  begin
    N := PosRPS(AProvedor);
    // Ler os XMLs dos RPS
    while N > 0 do
    begin
      AXML := copy(AXMLString, 1, N + TamTAG);
      AXMLString := Trim(copy(AXMLString, N + TamTAG + 1, length(AXMLString)));
      with Self.Add do
      begin
        LerXML(AXML);

        if AGerarNFSe then // Recalcula o XML
          GerarXML;
      end;

      N := PosRPS(AProvedor);
    end;
  end;

  Result := Self.Count > 0;
end;

function TNotasFiscais.GravarXML(const PathNomeArquivo: String): Boolean;
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
