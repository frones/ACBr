{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Dias                                     }
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

unit ACBrNFSeXWebserviceBase;

interface

uses
  Classes, SysUtils,
  {$IFNDEF NOGUI}
   {$IFDEF CLX}
     QDialogs,
   {$ELSE}
     {$IFDEF FMX}
       FMX.Dialogs,
     {$ELSE}
       Dialogs,
     {$ENDIF}
   {$ENDIF}
  {$ENDIF}
  ACBrBase, ACBrDFe, ACBrDFeConfiguracoes, ACBrDFeSSL,
  ACBrXmlDocument, ACBrNFSeXConversao;

resourcestring
  ERR_NAO_IMP = 'Serviço não implementado para este provedor.';
  ERR_SEM_URL_PRO = 'Não informado a URL de Produção, favor entrar em contato com a Prefeitura ou Provedor.';
  ERR_SEM_URL_HOM = 'Não informado a URL de Homologação, favor entrar em contato com a Prefeitura ou Provedor.';

type

  TACBrNFSeXWebservice = class
  private
    FPrefixo: string;
    FPath: string;
    FHtmlRetorno: string;

    function GetBaseUrl: string;

  protected
    FPHttpClient: TDFeSSLHttpClass;
    FPMethod: string;

    FPFaultNode: string;
    FPFaultCodeNode: string;
    FPFaultMsgNode: string;

    FPConfiguracoes: TConfiguracoes;
    FPDFeOwner: TACBrDFe;
    FPURL: string;
    FPMimeType: string;
    FPEnvio: string;
    FPRetorno: string;
    FUseOuterXml: Boolean;

    FPArqEnv: string;
    FPArqResp: string;
    FPMsgOrig: string;

    procedure FazerLog(const Msg: string; Exibir: Boolean = False); virtual;
    procedure GerarException(const Msg: string; E: Exception = nil); virtual;
    function GetSoapBody(const Response: string): string; virtual;

    function GerarPrefixoArquivo: string; virtual;

    function GravarJSON(NomeArquivo: String; ConteudoXML: String;
      const aPath: String = ''): Boolean;

    procedure SalvarEnvio(ADadosSoap, ADadosMsg: string); virtual;
    procedure SalvarRetornoWebService(ADadosSoap: string); virtual;
    procedure SalvarRetornoDadosMsg(ADadosMsg: string); virtual;

    procedure SetHeaders(aHeaderReq: THTTPHeader); virtual;

    function DefinirMsgEnvio(const Message, SoapAction, SoapHeader: string;
                                  namespace: array of string): string; virtual; abstract;

    function ExtrairRetorno(const ARetorno: string; responseTag: array of string): string; virtual;
    function TratarXmlRetornado(const aXML: string): string; virtual;
    function RetornaHTMLNota(const Retorno: string): String;

    procedure VerificarErroNoRetorno(const ADocument: TACBrXmlDocument); virtual;
    procedure UsarCertificado; virtual;
    procedure EnviarDados(const SoapAction: string); virtual;
    procedure EnvioInterno(var CodigoErro, CodigoInterno: Integer); virtual;
    procedure ConfigurarHttpClient; virtual;
    procedure LevantarExcecaoHttp; virtual;

    function Executar(const SoapAction, Message, responseTag: string): string; overload;
    function Executar(const SoapAction, Message, responseTag, namespace: string): string; overload;
    function Executar(const SoapAction, Message, responseTag: string;
                                  namespace: array of string): string; overload;
    function Executar(const SoapAction, Message: string;
                      responseTag, namespace: array of string): string; overload;
    function Executar(const SoapAction, Message, SoapHeader: string;
                      responseTag, namespace: array of string): string; overload;
  public
    constructor Create(AOwner: TACBrDFe; AMetodo: TMetodo; const AURL: string;
      const AMethod: string = 'POST');

    function Recepcionar(const ACabecalho, AMSG: string): string; virtual;
    function ConsultarLote(const ACabecalho, AMSG: string): string; virtual;
    function ConsultarSituacao(const ACabecalho, AMSG: string): string; virtual;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: string): string; virtual;
    function ConsultarNFSe(const ACabecalho, AMSG: string): string; virtual;
    function ConsultarNFSePorChave(const ACabecalho, AMSG: string): string; virtual;
    function ConsultarNFSePorFaixa(const ACabecalho, AMSG: string): string; virtual;
    function ConsultarNFSeServicoPrestado(const ACabecalho, AMSG: string): string; virtual;
    function ConsultarNFSeServicoTomado(const ACabecalho, AMSG: string): string; virtual;
    function ConsultarLinkNFSe(const ACabecalho, AMSG: string): string; virtual;
    function Cancelar(const ACabecalho, AMSG: string): string; virtual;
    function GerarNFSe(const ACabecalho, AMSG: string): string; virtual;
    function RecepcionarSincrono(const ACabecalho, AMSG: string): string; virtual;
    function SubstituirNFSe(const ACabecalho, AMSG: string): string; virtual;
    function GerarToken(const ACabecalho, AMSG: string): string; virtual;
    function AbrirSessao(const ACabecalho, AMSG: string): string; virtual;
    function FecharSessao(const ACabecalho, AMSG: string): string; virtual;
    function TesteEnvio(const ACabecalho, AMSG: string): string; virtual;
    function EnviarEvento(const ACabecalho, AMSG: string): string; virtual;
    function ConsultarEvento(const ACabecalho, AMSG: string): string; virtual;
    function ConsultarDFe(const ACabecalho, AMSG: string): string; virtual;
    function ConsultarParam(const ACabecalho, AMSG: string): string; virtual;
    function ConsultarSeqRps(const ACabecalho, AMSG: string): string; virtual;

    property URL: string read FPURL;
    property BaseURL: string read GetBaseUrl;
    property MimeType: string read FPMimeType;
    property Envio: string read FPEnvio;
    property Retorno: string read FPRetorno;
    property Prefixo: string read FPrefixo write FPrefixo;
    property Method: string read FPMethod;
    property Path: string read FPath write FPath;
    property HtmlRetorno: string read FHtmlRetorno;

  end;

  TACBrNFSeXWebserviceSoap11 = class(TACBrNFSeXWebservice)
  protected
    function DefinirMsgEnvio(const Message, SoapAction, SoapHeader: string;
                                  namespace: array of string): string; override;
  public
    constructor Create(AOwner: TACBrDFe; AMetodo: TMetodo; const AURL: string;
      const AMethod: string = 'POST'; const AMimeType: string = 'text/xml');

  end;

  TACBrNFSeXWebserviceSoap12 = class(TACBrNFSeXWebservice)
  protected
    function DefinirMsgEnvio(const Message, SoapAction, SoapHeader: string;
                                  namespace: array of string): string; override;
  public
    constructor Create(AOwner: TACBrDFe; AMetodo: TMetodo; const AURL: string;
      const AMethod: string = 'POST'; const AMimeType: string = 'application/soap+xml');

  end;

  TACBrNFSeXWebserviceNoSoap = class(TACBrNFSeXWebservice)
  protected
    function GetSoapBody(const Response: string): string; override;

    function DefinirMsgEnvio(const Message, SoapAction, SoapHeader: string;
                                  namespace: array of string): string; override;
  public
    constructor Create(AOwner: TACBrDFe; AMetodo: TMetodo; const AURL: string;
      const AMethod: string = 'POST'; const AMimeType: string = 'application/xml');

  end;

  TACBrNFSeXWebserviceRest = class(TACBrNFSeXWebserviceNoSoap)
  protected
    function DefinirMsgEnvio(const Message, SoapAction, SoapHeader: string;
                           namespace: array of string): string; override;

  public
    constructor Create(AOwner: TACBrDFe; AMetodo: TMetodo; const AURL: string;
      const AMethod: string = 'POST'; const AMimeType: string = 'application/json');

  end;

  TACBrNFSeXWebserviceRest2 = class(TACBrNFSeXWebserviceNoSoap)
  protected
    function DefinirMsgEnvio(const Message, SoapAction, SoapHeader: string;
                           namespace: array of string): string; override;

  public
    constructor Create(AOwner: TACBrDFe; AMetodo: TMetodo; const AURL: string;
      const AMethod: string = 'POST'; const AMimeType: string = 'application/json');

  end;

  TACBrNFSeXWebserviceMulti1 = class(TACBrNFSeXWebserviceNoSoap)
  protected
    FPBound: string;

    function DefinirMsgEnvio(const Message, SoapAction, SoapHeader: string;
                                  namespace: array of string): string; override;
  public
    constructor Create(AOwner: TACBrDFe; AMetodo: TMetodo; const AURL: string;
      const AMethod: string = 'POST'; const AMimeType: string = 'text/xml');

  end;

  TACBrNFSeXWebserviceMulti2 = class(TACBrNFSeXWebserviceNoSoap)
  protected
    FPBound: string;

    function DefinirMsgEnvio(const Message, SoapAction, SoapHeader: string;
                           namespace: array of string): string; override;

  public
    constructor Create(AOwner: TACBrDFe; AMetodo: TMetodo; const AURL: string;
      const AMethod: string = 'POST'; const AMimeType: string = 'text/xml');

  end;

  TInfConsultaNFSe = class
 private
   FtpConsulta: TtpConsulta;
   FtpPeriodo: TtpPeriodo;
   FtpRetorno: TtpRetorno;
   FNumeroIniNFSe: string;
   FNumeroFinNFSe: string;
   FSerieNFSe: string;
   FNumeroLote: string;
   FDataInicial: TDateTime;
   FDataFinal: TDateTime;

   FCNPJPrestador: string;
   FIMPrestador: string;
   FCNPJTomador: string;
   FIMTomador: string;
   FCNPJInter: string;
   FIMInter: string;
   FRazaoInter: string;
   FCadEconomico: string;
   FCodServ: string;
   FCodVerificacao: string;
   FChaveNFSe: string;
   FPagina: Integer;

 public
   constructor Create;

   function LerFromIni(const AIniStr: string): Boolean;

   property tpConsulta: TtpConsulta read FtpConsulta    write FtpConsulta;
   property tpPeriodo: TtpPeriodo   read FtpPeriodo     write FtpPeriodo;
   property tpRetorno: TtpRetorno   read FtpRetorno     write FtpRetorno;
   property NumeroIniNFSe: string   read FNumeroIniNFSe write FNumeroIniNFSe;
   property NumeroFinNFSe: string   read FNumeroFinNFSe write FNumeroFinNFSe;
   property SerieNFSe: string       read FSerieNFSe     write FSerieNFSe;
   property NumeroLote: string      read FNumeroLote    write FNumeroLote;
   property DataInicial: TDateTime  read FDataInicial   write FDataInicial;
   property DataFinal: TDateTime    read FDataFinal     write FDataFinal;
   property CNPJPrestador: string   read FCNPJPrestador write FCNPJPrestador;
   property IMPrestador: string     read FIMPrestador   write FIMPrestador;
   property CNPJTomador: string     read FCNPJTomador   write FCNPJTomador;
   property IMTomador: string       read FIMTomador     write FIMTomador;
   property CNPJInter: string       read FCNPJInter     write FCNPJInter;
   property IMInter: string         read FIMInter       write FIMInter;
   property RazaoInter: string      read FRazaoInter    write FRazaoInter;
   property CadEconomico: string    read FCadEconomico  write FCadEconomico;
   property CodServ: string         read FCodServ       write FCodServ;
   property CodVerificacao: string  read FCodVerificacao write FCodVerificacao;
   property ChaveNFSe: string       read FChaveNFSe     write FChaveNFSe;
   property Pagina: Integer         read FPagina        write FPagina;
 end;

  TInfConsultaLinkNFSe = class
  private
    FCompetencia: TDateTime;
    FDtEmissao: TDateTime;
    FNumeroNFSe: string;
    FSerieNFSe: string;
    FNumeroRps: Integer;
    FSerieRps: string;
    FTipoRps: string;
    FPagina: Integer;
  public
    constructor Create;

    function LerFromIni(const AIniStr: string): Boolean;

    property Competencia: TDateTime read FCompetencia write FCompetencia;
    property DtEmissao: TDateTime read FDtEmissao write FDtEmissao;
    property NumeroNFSe: string read FNumeroNFSe write FNumeroNFSe;
    property SerieNFSe: string read FSerieNFSe write FSerieNFSe;
    property NumeroRps: Integer read FNumeroRps write FNumeroRps;
    property SerieRps: string read FSerieRps write FSerieRps;
    property TipoRps: string read FTipoRps write FTipoRps;
    property Pagina: Integer read FPagina write FPagina;
  end;

  TInfCancelamento = class
  private
    FNumeroNFSe: string;
    FSerieNFSe: string;
    FChaveNFSe: string;
    FDataEmissaoNFSe: TDateTime;
    FCodCancelamento: string;
    FMotCancelamento: string;
    FNumeroLote: string;
    FNumeroRps: Integer;
    FSerieRps: string;
    FValorNFSe: Double;
    FCodVerificacao: string;
    Femail: string;
    FNumeroNFSeSubst: string;
    FSerieNFSeSubst: string;
    FCodServ: string;

  public
    constructor Create;

    function LerFromIni(const AIniStr: string): Boolean;

    property NumeroNFSe: string      read FNumeroNFSe      write FNumeroNFSe;
    property SerieNFSe: string       read FSerieNFSe       write FSerieNFSe;
    property ChaveNFSe: string       read FChaveNFSe       write FChaveNFSe;
    property DataEmissaoNFSe: TDateTime read FDataEmissaoNFSe write FDataEmissaoNFSe;
    property CodCancelamento: string read FCodCancelamento write FCodCancelamento;
    property MotCancelamento: string read FMotCancelamento write FMotCancelamento;
    property NumeroLote: string      read FNumeroLote      write FNumeroLote;
    property NumeroRps: Integer      read FNumeroRps       write FNumeroRps;
    property SerieRps: string        read FSerieRps        write FSerieRps;
    property ValorNFSe: Double       read FValorNFSe       write FValorNFSe;
    property CodVerificacao: string  read FCodVerificacao  write FCodVerificacao;
    property email: string           read Femail           write Femail;
    property NumeroNFSeSubst: string read FNumeroNFSeSubst write FNumeroNFSeSubst;
    property SerieNFSeSubst: string  read FSerieNFSeSubst  write FSerieNFSeSubst;
    property CodServ: string         read FCodServ         write FCodServ;
  end;

   TpedRegEvento = class
  private
    FID: string;
    FtpAmb: Integer;
    FverAplic: string;
    FdhEvento: TDateTime;
    FchNFSe: string;
    FnPedRegEvento: Integer;
    FtpEvento: TtpEvento;
    FcMotivo: Integer;
    FxMotivo: string;
    FchSubstituta: string;
  public
    constructor Create;

    property ID: string             read FID            write FID;
    property tpAmb: Integer         read FtpAmb         write FtpAmb;
    property verAplic: string       read FverAplic      write FverAplic;
    property dhEvento: TDateTime    read FdhEvento      write FdhEvento;
    property chNFSe: string         read FchNFSe        write FchNFSe;
    property nPedRegEvento: Integer read FnPedRegEvento write FnPedRegEvento;
    property tpEvento: TtpEvento    read FtpEvento      write FtpEvento;
    property cMotivo: Integer       read FcMotivo       write FcMotivo;
    property xMotivo: string        read FxMotivo       write FxMotivo;
    property chSubstituta: string   read FchSubstituta  write FchSubstituta;
  end;

 TInfEvento = class
  private
    FID: string;
    FverAplic: string;
    FambGer: Integer;
    FnSeqEvento: Integer;
    FdhProc: TDateTime;
    FnDFe: string;
    FpedRegEvento: TpedRegEvento;
  public
    constructor Create;
    destructor Destroy; override;

    function LerFromIni(const AIniStr: string): Boolean;

    property ID: string                  read FID;
    property verAplic: string            read FverAplic     write FverAplic;
    property ambGer: Integer             read FambGer       write FambGer;
    property nSeqEvento: Integer         read FnSeqEvento   write FnSeqEvento;
    property dhProc: TDateTime           read FdhProc       write FdhProc;
    property nDFe: string                read FnDFe         write FnDFe;
    property pedRegEvento: TpedRegEvento read FpedRegEvento write FpedRegEvento;
  end;

implementation

uses
  IniFiles, StrUtils, synautil,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.XMLHTML, ACBrUtil.DateTime,
  ACBrUtil.FilesIO,
  ACBrConsts, ACBrDFeException, ACBrXmlBase,
  ACBrNFSeX, ACBrNFSeXConfiguracoes;

{ TACBrNFSeXWebservice }

constructor TACBrNFSeXWebservice.Create(AOwner: TACBrDFe; AMetodo: TMetodo;
  const AURL: string; const AMethod: string);
begin
  FPDFeOwner := AOwner;
  if Assigned(AOwner) then
    FPConfiguracoes := AOwner.Configuracoes;

  FUseOuterXml := True;

  FPFaultNode := 'Fault';
  FPFaultCodeNode := 'faultcode';
  FPFaultMsgNode := 'faultstring';
  Path := '';

  case AMetodo of
    tmRecepcionar:
      begin
        FPArqEnv := 'env-lot';
        FPArqResp := 'rec';
      end;

    tmTeste:
      begin
        FPArqEnv := 'env-lot-teste';
        FPArqResp := 'rec-teste';
      end;

    tmConsultarSituacao:
      begin
        FPArqEnv := 'con-sit';
        FPArqResp := 'sit';
      end;

    tmConsultarLote:
      begin
        FPArqEnv := 'con-lot';
        FPArqResp := 'lista-nfse-con-lot';
      end;

    tmConsultarNFSePorRps:
      begin
        FPArqEnv := 'con-nfse-rps';
        FPArqResp := 'comp-nfse';
      end;

    tmConsultarNFSe:
      begin
        FPArqEnv := 'con-nfse';
        FPArqResp := 'lista-nfse-con';
      end;

    tmConsultarNFSePorChave:
      begin
        FPArqEnv := 'con-nfse-chv';
        FPArqResp := 'lista-nfse-chv';
      end;

    tmConsultarNFSePorFaixa:
      begin
        FPArqEnv := 'con-nfse-fai';
        FPArqResp := 'lista-nfse-fai';
      end;

    tmConsultarNFSeServicoPrestado:
      begin
        FPArqEnv := 'con-nfse-ser-pres';
        FPArqResp := 'lista-nfse-ser-pres';
      end;

    tmConsultarNFSeServicoTomado:
      begin
        FPArqEnv := 'con-nfse-ser-tom';
        FPArqResp := 'lista-nfse-ser-tom';
      end;

    tmCancelarNFSe:
      begin
        FPArqEnv := 'ped-can';
        FPArqResp := 'can';
      end;

    tmGerar:
      begin
        FPArqEnv := 'ger-nfse';
        FPArqResp := 'lista-nfse-ger';
      end;

    tmRecepcionarSincrono:
      begin
        FPArqEnv := 'env-lot-sinc';
        FPArqResp := 'lista-nfse-sinc';
      end;

    tmSubstituirNFSe:
      begin
        FPArqEnv := 'ped-sub';
        FPArqResp := 'sub';
      end;

    tmGerarToken:
      begin
        FPArqEnv := 'ped-token';
        FPArqResp := 'ret-token';
      end;

    tmAbrirSessao:
      begin
        FPArqEnv := 'abr-ses';
        FPArqResp := 'ret-abr';
      end;

    tmFecharSessao:
      begin
        FPArqEnv := 'fec-ses';
        FPArqResp := 'ret-fec';
      end;

    tmEnviarEvento:
      begin
        FPArqEnv := 'env-eve';
        FPArqResp := 'eve';
      end;

    tmConsultarEvento:
      begin
        FPArqEnv := 'con-eve';
        FPArqResp := 'eve';
      end;

    tmConsultarDFe:
      begin
        FPArqEnv := 'con-dfe';
        FPArqResp := 'dfe';
      end;

    tmConsultarParam:
      begin
        FPArqEnv := 'con-param';
        FPArqResp := 'param';
      end;

    tmConsultarSeqRps:
      begin
        FPArqEnv := 'con-seqrps';
        FPArqResp := 'seqrps';
      end;

    tmConsultarLinkNFSe:
      begin
        FPArqEnv := 'con-link';
        FPArqResp := 'link';
      end;
  end;

  FPURL := AURL;
  FPMethod := AMethod;
end;

procedure TACBrNFSeXWebservice.FazerLog(const Msg: string; Exibir: Boolean);
var
  Tratado: Boolean;
begin
  if (Msg <> '') then
  begin
    FPDFeOwner.FazerLog(Msg, Tratado);

    if Tratado then Exit;

    {$IFNDEF NOGUI}
    if Exibir and FPConfiguracoes.WebServices.Visualizar then
      ShowMessage(ACBrStr(Msg));
    {$ENDIF}
  end;
end;

procedure TACBrNFSeXWebservice.GerarException(const Msg: string; E: Exception);
begin
  FPDFeOwner.GerarException(ACBrStr(Msg), E);
end;

function TACBrNFSeXWebservice.GetBaseUrl: string;
var
  i:Integer;
begin
  Result := '';

  if EstaVazio(Url) then Exit;

  i := Pos('//', Url);

  if i>0 then
    i := PosEx('/', Url, i+2)
  else
    i := Pos('/', Url);

  if i=0 then
    i := Length(Url);

  Result := Copy(Url, 1, i);
end;

function TACBrNFSeXWebservice.GerarPrefixoArquivo: string;
begin
  if FPrefixo = '' then
    Result := FormatDateTime('yyyymmddhhnnss', Now)
  else
    Result := TiraPontos(FPrefixo);
end;

function TACBrNFSeXWebservice.GravarJSON(NomeArquivo, ConteudoXML: String;
  const aPath: String): Boolean;
var
  SoNome, SoPath: String;
begin
  Result := False;
  try
    SoNome := ExtractFileName(NomeArquivo);
    if EstaVazio(SoNome) then
      raise EACBrDFeException.Create('Nome de arquivo não informado');

    SoPath := ExtractFilePath(NomeArquivo);
    if EstaVazio(SoPath) then
      SoPath := aPath;
    if EstaVazio(SoPath) then
      SoPath := FPConfiguracoes.Arquivos.PathSalvar;

    SoPath := PathWithDelim(SoPath);

    ConteudoXML := StringReplace(ConteudoXML, '<-><->', '', [rfReplaceAll]);

    if not DirectoryExists(SoPath) then
      ForceDirectories(SoPath);

    NomeArquivo := SoPath + SoNome;

    WriteToTXT(NomeArquivo, ConteudoXML, False, False);
    Result := True;
  except
    on E: Exception do
      GerarException('Erro ao salvar.', E);
  end;
end;

procedure TACBrNFSeXWebservice.SalvarEnvio(ADadosSoap, ADadosMsg: string);
var
  ArqEnv, Extensao, ExtensaoSoap: string;
  ConteudoEhXml: Boolean;
begin
  { Sobrescrever apenas se necessário }

  if FPArqEnv = '' then Exit;

  {
    Tem provedor que é gerando um JSON em vez de XML e o método Gravar acaba
    incluindo na primeira linha do arquivo o encoding do XML.
    Para contornar isso a variável ConteudoEhXml recebe o valor false quando é
    um JSON e o método Gravar não inclui o encoding.
  }
  case TACBrNFSeX(FPDFeOwner).Provider.ConfigGeral.FormatoArqEnvio of
    tfaJson:
      Extensao := '.json';
    tfaTxt:
      Extensao := '.txt';
  else
    Extensao := '.xml';
  end;

  case TACBrNFSeX(FPDFeOwner).Provider.ConfigGeral.FormatoArqEnvioSoap of
    tfaJson:
      ExtensaoSoap := '.json';
    tfaTxt:
      ExtensaoSoap := '.txt';
  else
    ExtensaoSoap := '.xml';
  end;

  if TACBrNFSeX(FPDFeOwner).Provider.ConfigGeral.FormatoArqEnvio <> tfaXml then
  begin
    ADadosMsg := RemoverDeclaracaoXML(ADadosMsg);
    ConteudoEhXml := False;
  end
  else
    ConteudoEhXml := True;

  if FPConfiguracoes.Geral.Salvar then
  begin
    Prefixo := GerarPrefixoArquivo;
    ArqEnv := Prefixo + '-' + FPArqEnv;

    if ConteudoEhXml then
    begin
      if not XmlEhUTF8(ADadosMsg) then
        ADadosMsg := RemoverDeclaracaoXML(ADadosMsg);

      FPDFeOwner.Gravar(ArqEnv + Extensao, ADadosMsg, Path);
    end
    else
      GravarJSON(ArqEnv + Extensao, ADadosMsg, Path);
  end;

  if FPConfiguracoes.WebServices.Salvar then
  begin
    Prefixo := GerarPrefixoArquivo;
    ArqEnv := Prefixo + '-' + FPArqEnv;

    if ConteudoEhXml then
    begin
      if not XmlEhUTF8(ADadosSoap) then
        ADadosSoap := RemoverDeclaracaoXML(ADadosSoap);

      FPDFeOwner.Gravar(ArqEnv + '-soap' + ExtensaoSoap, ADadosSoap, Path);
    end
    else
      GravarJSON(ArqEnv + '-soap' + ExtensaoSoap, ADadosSoap, Path);
  end;
end;

procedure TACBrNFSeXWebservice.SalvarRetornoDadosMsg(ADadosMsg: string);
var
  ArqEnv, Extensao: string;
  ConteudoEhXml: Boolean;
begin
  { Sobrescrever apenas se necessário }

  if FPArqResp = '' then Exit;

  {
    Tem provedor que é gerando um JSON em vez de XML e o método Gravar acaba
    incluindo na primeira linha do arquivo o encoding do XML.
    Para contornar isso a variável ConteudoEhXml recebe o valor false quando é
    um JSON e o método Gravar não inclui o encoding.
  }
  case TACBrNFSeX(FPDFeOwner).Provider.ConfigGeral.FormatoArqRetorno of
    tfaJson:
      Extensao := '.json';
    tfaTxt:
      Extensao := '.txt';
  else
    Extensao := '.xml';
  end;

  if TACBrNFSeX(FPDFeOwner).Provider.ConfigGeral.FormatoArqRetorno <> tfaXml then
  begin
    if not StringIsPDF(ADadosMsg) then
      ADadosMsg := RemoverDeclaracaoXML(ADadosMsg);

    ConteudoEhXml := False;
  end
  else
    ConteudoEhXml := True;

  if FPDFeOwner.Configuracoes.Geral.Salvar then
  begin
    Prefixo := GerarPrefixoArquivo;
    ArqEnv := Prefixo + '-' + FPArqResp;

    if ConteudoEhXml then
    begin
      if not XmlEhUTF8(ADadosMsg) then
        ADadosMsg := RemoverDeclaracaoXML(ADadosMsg);

      FPDFeOwner.Gravar(ArqEnv + Extensao, ADadosMsg, Path);
    end
    else
      GravarJSON(ArqEnv + Extensao, ADadosMsg, Path);
  end;
end;

procedure TACBrNFSeXWebservice.SalvarRetornoWebService(ADadosSoap: string);
var
  ArqEnv, Extensao: string;
  ConteudoEhXml: Boolean;
begin
  { Sobrescrever apenas se necessário }

  if FPArqResp = '' then Exit;

  {
    Tem provedor que é gerando um JSON em vez de XML e o método Gravar acaba
    incluindo na primeira linha do arquivo o encoding do XML.
    Para contornar isso a variável ConteudoEhXml recebe o valor false quando é
    um JSON e o método Gravar não inclui o encoding.
  }
  case TACBrNFSeX(FPDFeOwner).Provider.ConfigGeral.FormatoArqRetornoSoap of
    tfaJson:
      Extensao := '.json';
    tfaTxt:
      Extensao := '.txt';
  else
    Extensao := '.xml';
  end;

  if TACBrNFSeX(FPDFeOwner).Provider.ConfigGeral.FormatoArqRetornoSoap <> tfaXml then
  begin
    if TACBrNFSeX(FPDFeOwner).WebService.ConsultaNFSe.InfConsultaNFSe.tpRetorno <> trPDF then
      ADadosSoap := RemoverDeclaracaoXML(ADadosSoap);

    ConteudoEhXml := False;
  end
  else
    ConteudoEhXml := True;

  if FPDFeOwner.Configuracoes.WebServices.Salvar then
  begin
    Prefixo := GerarPrefixoArquivo;
    ArqEnv := Prefixo + '-' + FPArqResp;

    if ConteudoEhXml then
    begin
      if not XmlEhUTF8(ADadosSoap) then
        ADadosSoap := RemoverDeclaracaoXML(ADadosSoap);

      FPDFeOwner.Gravar(ArqEnv + '-soap' + Extensao, ADadosSoap, Path);
    end
    else
      GravarJSON(ArqEnv + '-soap' + Extensao, ADadosSoap, Path);
  end;
end;

procedure TACBrNFSeXWebservice.SetHeaders(aHeaderReq: THTTPHeader);
begin
  if TACBrNFSeX(FPDFeOwner).Provider.ConfigGeral.UseAuthorizationHeader then
    aHeaderReq.AddHeader('Authorization', TConfiguracoesNFSe(FPConfiguracoes).Geral.Emitente.WSChaveAutoriz);
end;

function TACBrNFSeXWebservice.GetSoapBody(const Response: string): string;
begin
  Result := SeparaDados(Response, 'Body');

  if Result = '' then
    Result := Response;
end;

procedure TACBrNFSeXWebservice.LevantarExcecaoHttp;
var
  aRetorno: TACBrXmlDocument;
begin
  // Verifica se o ResultCode é: 200 OK; 201 Created; 202 Accepted
  // https://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html

//  if not (HttpClient.HTTPResultCode in [200..202]) then
//    raise EACBrDFeException.Create('Erro de Conexão.');

  if not XmlEhUTF8(FPRetorno) then
    Exit;

  if not (FPHttpClient.HTTPResultCode in [200..202]) then
  begin
    aRetorno := TACBrXmlDocument.Create;
    try
      aRetorno.LoadFromXml(FPRetorno);
      VerificarErroNoRetorno(aRetorno);
    finally
      aRetorno.Free;
    end;
  end;
end;

procedure TACBrNFSeXWebservice.VerificarErroNoRetorno(const ADocument: TACBrXmlDocument);
var
  ANode: TACBrXmlNode;
  aMsg, xFaultCodeNode, xFaultMsgNode, xCode, xReason, xDetail: string;
begin
  if ADocument.Root.LocalName = FPFaultNode then
    ANode := ADocument.Root
  else
    ANode := ADocument.Root.Childrens.FindAnyNs(FPFaultNode);

  {
    Se o ANode for igual a nil significa que não foi retornado nenhum
    Grupo/Elemento de erro no Soap, logo não tem erro Soap a ser apresentado.
  }

  if ANode = nil then
    Exit;

  xFaultCodeNode := ObterConteudoTag(ANode.Childrens.FindAnyNs(FPFaultCodeNode), tcStr);
  xFaultMsgNode := ObterConteudoTag(ANode.Childrens.FindAnyNs(FPFaultMsgNode), tcStr);

  xCode := ObterConteudoTag(ANode.Childrens.FindAnyNs('Code'), tcStr);
  xReason := ObterConteudoTag(ANode.Childrens.FindAnyNs('Reason'), tcStr);
  xDetail := ObterConteudoTag(ANode.Childrens.FindAnyNs('Detail'), tcStr);

  if (xFaultCodeNode <> '') or (xFaultMsgNode <> '') then
    aMsg := IfThen(xFaultCodeNode = '', '999', xFaultCodeNode) + ' - ' +
            IfThen(xFaultMsgNode = '', 'Erro Desconhecido.', xFaultMsgNode)
  else
    aMsg := xCode + ' - ' + xReason + ' - ' + xDetail;

  raise EACBrDFeException.Create(aMsg);
end;

function TACBrNFSeXWebservice.ExtrairRetorno(const ARetorno: string;
  responseTag: array of string): string;
var
  Document: TACBrXmlDocument;
  ANode: TACBrXmlNode;
  I: Integer;
  xRetorno: string;
begin
  Result := '';

  xRetorno := TratarXmlRetornado(ARetorno);

  if xRetorno = '' then
    Exit;

  if not StringIsXML(xRetorno) then
  begin
    Result := xRetorno;
    Exit;
  end;

  if (Length(responseTag) = 0) then
  begin
    Result := xRetorno;
    Exit;
  end;

  Document := TACBrXmlDocument.Create;
  try
    Document.LoadFromXml(xRetorno);

    VerificarErroNoRetorno(Document);
    ANode := Document.Root;

    if ANode.Name <> 'a' then
    begin
      for I := Low(responseTag) to High(responseTag) do
      begin
        if ANode <> nil then
          ANode := ANode.Childrens.FindAnyNs(responseTag[I]);
      end;

      if ANode = nil then
        ANode := Document.Root.Childrens.FindAnyNs(responseTag[0]);

      if ANode = nil then
        ANode := Document.Root;
    end;

    if ANode <> nil then
    begin
      if FUseOuterXml then
        Result := ANode.OuterXml
      else
        Result := ANode.Content;
    end;
  finally
    Document.Free;
  end;
end;

function TACBrNFSeXWebservice.TratarXmlRetornado(const aXML: string): string;
begin
  // Reescrever na Unit Provider do Provedor se necessário;
  Result := GetSoapBody(aXML);
end;

procedure TACBrNFSeXWebservice.UsarCertificado;
var
  TemCertificadoConfigurado: Boolean;
begin
  FPDFeOwner.SSL.UseCertificateHTTP := TACBrNFSeX(FPDFeOwner).Provider.ConfigGeral.UseCertificateHTTP;

  if FPDFeOwner.SSL.UseCertificateHTTP then
  begin
    TemCertificadoConfigurado := (FPConfiguracoes.Certificados.NumeroSerie <> '') or
                                 (FPConfiguracoes.Certificados.DadosPFX <> '') or
                                 (FPConfiguracoes.Certificados.ArquivoPFX <> '');

    if TemCertificadoConfigurado then
      if FPConfiguracoes.Certificados.VerificarValidade then
        if (FPDFeOwner.SSL.CertDataVenc < Now) then
          raise EACBrDFeException.Create('Data de Validade do Certificado já expirou: '+
                                            FormatDateBr(FPDFeOwner.SSL.CertDataVenc));
  end;
end;

function TACBrNFSeXWebservice.Executar(const SoapAction, Message, responseTag: string): string;
begin
  Result := Executar(SoapAction, Message, '', [], []);
end;

function TACBrNFSeXWebservice.Executar(const SoapAction, Message, responseTag, namespace: string): string;
begin
  Result := Executar(SoapAction, Message, '', [responseTag], [namespace]);
end;

function TACBrNFSeXWebservice.Executar(const SoapAction, Message, responseTag: string;
  namespace: array of string): string;
begin
  Result := Executar(SoapAction, Message, '', [responseTag], namespace);
end;

function TACBrNFSeXWebservice.Executar(const SoapAction, Message: string;
  responseTag, namespace: array of string): string;
begin
  Result := Executar(SoapAction, Message, '', responseTag, namespace);
end;

procedure TACBrNFSeXWebservice.EnviarDados(const SoapAction: string);
var
  Tentar, Tratado: Boolean;
  HTTPResultCode: Integer;
  InternalErrorCode: Integer;
begin
  Tentar := True;

  while Tentar do
  begin
    Tentar  := False;
    Tratado := False;
    FPRetorno := '';
    HTTPResultCode := 0;
    InternalErrorCode := 0;

    try
      // Envio por Evento... Aplicação cuidará do envio
      if Assigned(FPDFeOwner.OnTransmit) then
      begin
        FPDFeOwner.OnTransmit(FPEnvio, FPURL, SoapAction,
                              FPMimeType, FPRetorno,
                              HTTPResultCode, InternalErrorCode);

        if (InternalErrorCode <> 0) then
          raise EACBrDFeException.Create('Erro ao Transmitir');
      end
      else   // Envio interno, por TDFeSSL
      begin
        EnvioInterno(HTTPResultCode, InternalErrorCode);
      end;
    except
      if Assigned(FPDFeOwner.OnTransmitError) then
        FPDFeOwner.OnTransmitError(HTTPResultCode, InternalErrorCode,
                                   FPURL, FPEnvio, SoapAction,
                                   Tentar, Tratado);

      if not (Tentar or Tratado) then raise;
    end;
  end;
end;

procedure TACBrNFSeXWebservice.ConfigurarHttpClient;
begin
  FPHttpClient.URL := FPURL;
  FPHttpClient.Method := FPMethod;
  FPHttpClient.MimeType := FPMimeType;

  SetHeaders(FPHttpClient.HeaderReq);

  if FPMethod = 'POST' then
    WriteStrToStream(FPHttpClient.DataReq, AnsiString(FPEnvio));
end;

procedure TACBrNFSeXWebservice.EnvioInterno(var CodigoErro, CodigoInterno: Integer);
const
  UTF_8 = #$C3;
begin
  ConfigurarHttpClient;

  try
    try
      FPHttpClient.Execute;
    finally
      CodigoErro := FPHttpClient.HTTPResultCode;
      CodigoInterno := FPHttpClient.InternalErrorCode;
    end;

    FPHttpClient.DataResp.Position := 0;

    FPRetorno := ReadStrFromStream(FPHttpClient.DataResp, FPHttpClient.DataResp.Size);

    if FPRetorno = '' then
      raise EACBrDFeException.Create('WebService retornou um XML vazio.');

    {
      Se o retorno for um XML mas o seu encoding for iso-8859-1 ou se não constar
      a declaração do encoding logo no inicio do XML assume que o mesmo não
      esta no formato URF-8
    }
    if ((Pos('iso-8859-1', LowerCase(FPRetorno)) > 0) or
        (Pos('encoding', LowerCase(FPRetorno)) = 0) or
        (Pos('encoding', LowerCase(FPRetorno)) > 40)
       ) and StringIsXML(FPRetorno) then
    begin
      {
        Se não encontrar o caracter que diz que uma vogal acentuada ou cedilha
        esta no formato UTF-8, converte o XML para UTF-8
      }
      if Pos(UTF_8, FPRetorno) = 0 then
      begin
        FPRetorno := RemoverDeclaracaoXML(FPRetorno);
        FPRetorno := AnsiToNativeString(FPRetorno);
        FPRetorno := NativeStringToUTF8(FPRetorno);
      end;

      FPRetorno := StringReplace(FPRetorno, '<?xml version="1.0" ?>', '', [rfReplaceAll]);
      FPRetorno := '<?xml version="1.0" encoding="UTF-8"?>' + FPRetorno;
    end;

    if StringIsXML(FPRetorno) then
      LevantarExcecaoHttp;
  except
    on E:Exception do
    begin
      raise EACBrDFeException.CreateDef(Format(ACBrStr(cACBrDFeSSLEnviarException),
        [FPHttpClient.InternalErrorCode, FPHttpClient.HTTPResultCode, FPHttpClient.URL])
        + sLineBreak + FPHttpClient.LastErrorDesc+ sLineBreak + E.Message);
    end;
  end;
end;

function TACBrNFSeXWebservice.Executar(const SoapAction, Message, SoapHeader: string;
  responseTag, namespace: array of string): string;
begin
  FPEnvio := DefinirMsgEnvio(Message, SoapAction, SoapHeader, namespace);
  SalvarEnvio(FPEnvio, FPMsgOrig);

  UsarCertificado;

  EnviarDados(SoapAction);
  SalvarRetornoWebService(FPRetorno);

  FHtmlRetorno := RetornaHTMLNota(FPRetorno);

  Result := ExtrairRetorno(FPRetorno, responseTag);
  SalvarRetornoDadosMsg(Result);
end;

function TACBrNFSeXWebservice.Recepcionar(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.ConsultarDFe(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.ConsultarEvento(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.ConsultarLote(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.ConsultarSeqRps(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.ConsultarSituacao(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.ConsultarNFSePorRps(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.ConsultarNFSe(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.ConsultarNFSePorChave(const ACabecalho,
  AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.ConsultarNFSePorFaixa(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.ConsultarNFSeServicoPrestado(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.ConsultarNFSeServicoTomado(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.ConsultarParam(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.ConsultarLinkNFSe(const ACabecalho,
  AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.Cancelar(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.GerarNFSe(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.RecepcionarSincrono(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.RetornaHTMLNota(const Retorno: string): String;
var pInicio, pFim: Integer;
begin
  Result := EmptyStr;

  pInicio := Pos('<codigo_html>', Retorno);
  pFim    := Pos('</codigo_html>', Retorno);

  if pInicio > 0 then
  begin
    Result := Copy(Retorno, pInicio, pFim -1) + '</codigo_html>';
    Result := StringReplace(Result, '&lt;', '<', [rfReplaceAll]);
    Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll]);
    Result := StringReplace(Result, '&quot;', '"', [rfReplaceAll]);
  end;
end;

function TACBrNFSeXWebservice.SubstituirNFSe(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.GerarToken(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.AbrirSessao(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.FecharSessao(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.TesteEnvio(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.EnviarEvento(const ACabecalho, AMSG: string): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

{ TACBrNFSeXWebserviceSoap11 }

constructor TACBrNFSeXWebserviceSoap11.Create(AOwner: TACBrDFe; AMetodo: TMetodo;
  const AURL: string; const AMethod: string; const AMimeType: string);
begin
  inherited Create(AOwner, AMetodo, AURL, AMethod);

  FPMimeType := AMimeType;
end;

function TACBrNFSeXWebserviceSoap11.DefinirMsgEnvio(const Message, SoapAction,
  SoapHeader: string; namespace: array of string): string;
var
  i: Integer;
  ns: string;
begin
  Result := '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"';

  for i := Low(namespace) to High(namespace) do
  begin
    ns := namespace[i];
    Result := Result + ' ' + ns;
  end;

  Result := Result + '>';

  if NaoEstaVazio(SoapHeader) then
    Result := Result + '<soapenv:Header>' + soapHeader + '</soapenv:Header>'
  else
    Result := Result + '<soapenv:Header/>';

  Result := Result + '<soapenv:Body>' + Message + '</soapenv:Body></soapenv:Envelope>';
  Result := string(NativeStringToUTF8(Result));

  FPHttpClient := FPDFeOwner.SSL.SSLHttpClass;

  FPHttpClient.Clear;
  FPHttpClient.HeaderReq.AddHeader('SOAPAction', SoapAction);
end;

{ TACBrNFSeXWebserviceSoap12 }

constructor TACBrNFSeXWebserviceSoap12.Create(AOwner: TACBrDFe; AMetodo: TMetodo;
  const AURL: string; const AMethod: string; const AMimeType: string);
begin
  inherited Create(AOwner, AMetodo, AURL, AMethod);

  FPMimeType := AMimeType;
end;

function TACBrNFSeXWebserviceSoap12.DefinirMsgEnvio(const Message, SoapAction,
  SoapHeader: string; namespace: array of string): string;
var
  i: Integer;
  ns: string;
begin
  Result := '<soapenv:Envelope xmlns:soapenv="http://www.w3.org/2003/05/soap-envelope"';

  for i := Low(namespace) to High(namespace) do
  begin
    ns := namespace[i];
    Result := Result + ' ' + ns;
  end;

  Result := Result + '>';

  if NaoEstaVazio(SoapHeader) then
    Result := Result + '<soapenv:Header>' + soapHeader + '</soapenv:Header>'
  else
    Result := Result + '<soapenv:Header/>';

  Result := Result + '<soapenv:Body>' + message + '</soapenv:Body></soapenv:Envelope>';
  Result := string(NativeStringToUTF8(Result));

  FPMimeType := FPMimeType + ';action="' + SoapAction + '"';

  FPHttpClient := FPDFeOwner.SSL.SSLHttpClass;

  FPHttpClient.Clear;
end;

{ TACBrNFSeXWebserviceNoSoap }

constructor TACBrNFSeXWebserviceNoSoap.Create(AOwner: TACBrDFe;
  AMetodo: TMetodo; const AURL: string; const AMethod: string; const AMimeType: string);
begin
  inherited Create(AOwner, AMetodo, AURL, AMethod);

  FPMimeType := AMimeType;
end;

function TACBrNFSeXWebserviceNoSoap.DefinirMsgEnvio(const Message, SoapAction,
  SoapHeader: string; namespace: array of string): string;
begin
  Result := Message;

  Result := string(NativeStringToUTF8(Result));

  FPHttpClient := FPDFeOwner.SSL.SSLHttpClass;

  FPHttpClient.Clear;
  FPHttpClient.HeaderReq.AddHeader('SOAPAction', SoapAction);
end;

function TACBrNFSeXWebserviceNoSoap.GetSoapBody(const Response: string): string;
begin
  Result := Response;
end;

{ TACBrNFSeXWebserviceRest }

constructor TACBrNFSeXWebserviceRest.Create(AOwner: TACBrDFe; AMetodo: TMetodo;
  const AURL: string; const AMethod: string; const AMimeType: string);
begin
  inherited Create(AOwner, AMetodo, AURL, AMethod);

  FPMimeType := AMimeType;
end;

function TACBrNFSeXWebserviceRest.DefinirMsgEnvio(const Message, SoapAction,
  SoapHeader: string; namespace: array of string): string;
begin
  Result := Message;

  FPHttpClient := FPDFeOwner.SSL.SSLHttpClass;

  FPHttpClient.Clear;
end;

{ TACBrNFSeXWebserviceRest2 }

constructor TACBrNFSeXWebserviceRest2.Create(AOwner: TACBrDFe; AMetodo: TMetodo;
  const AURL: string; const AMethod: string; const AMimeType: string);
begin
  inherited Create(AOwner, AMetodo, AURL, AMethod);

  FPMimeType := AMimeType;
end;

function TACBrNFSeXWebserviceRest2.DefinirMsgEnvio(const Message, SoapAction,
  SoapHeader: string; namespace: array of string): string;
var
  UsuarioWeb, SenhaWeb, Texto: string;
begin
  UsuarioWeb := Trim(TConfiguracoesNFSe(FPConfiguracoes).Geral.Emitente.WSUser);

  if UsuarioWeb = '' then
    GerarException(ACBrStr('O provedor ' + TConfiguracoesNFSe(FPConfiguracoes).Geral.xProvedor +
      ' necessita que a propriedade: Configuracoes.Geral.Emitente.WSUser seja informada.'));

  SenhaWeb := Trim(TConfiguracoesNFSe(FPConfiguracoes).Geral.Emitente.WSSenha);

  if SenhaWeb = '' then
    GerarException(ACBrStr('O provedor ' + TConfiguracoesNFSe(FPConfiguracoes).Geral.xProvedor +
      ' necessita que a propriedade: Configuracoes.Geral.Emitente.WSSenha seja informada.'));

  Texto := StringReplace(Message, '"', '\"', [rfReplaceAll]);
  Texto := StringReplace(Texto, #10, '', [rfReplaceAll]);
  Texto := StringReplace(Texto, #13, '', [rfReplaceAll]);

  Result := Format('{"xml": "%s", "usuario": "%s", "senha": "%s"}', [Texto, UsuarioWeb, SenhaWeb]);

  FPHttpClient := FPDFeOwner.SSL.SSLHttpClass;

  FPHttpClient.Clear;
end;

{ TACBrNFSeXWebserviceMulti1 }

constructor TACBrNFSeXWebserviceMulti1.Create(AOwner: TACBrDFe; AMetodo: TMetodo;
  const AURL: string; const AMethod: string; const AMimeType: string);
begin
  inherited Create(AOwner, AMetodo, AURL, AMethod);

  FPBound := IntToHex(Random(MaxInt), 8) + '_Synapse_boundary';
  FPMimeType := 'multipart/form-data; boundary=' + AnsiQuotedStr(FPBound, '"');
end;

function TACBrNFSeXWebserviceMulti1.DefinirMsgEnvio(const Message, SoapAction,
  SoapHeader: string; namespace: array of string): string;
var
  UsuarioWeb, SenhaWeb: string;
begin
  UsuarioWeb := Trim(TConfiguracoesNFSe(FPConfiguracoes).Geral.Emitente.WSUser);

  if UsuarioWeb = '' then
    GerarException(ACBrStr('O provedor ' + TConfiguracoesNFSe(FPConfiguracoes).Geral.xProvedor +
      ' necessita que a propriedade: Configuracoes.Geral.Emitente.WSUser seja informada.'));

  SenhaWeb := Trim(TConfiguracoesNFSe(FPConfiguracoes).Geral.Emitente.WSSenha);

  if SenhaWeb = '' then
    GerarException(ACBrStr('O provedor ' + TConfiguracoesNFSe(FPConfiguracoes).Geral.xProvedor +
      ' necessita que a propriedade: Configuracoes.Geral.Emitente.WSSenha seja informada.'));

  Result := '--' + FPBound + sLineBreak +
            'Content-Disposition: form-data; name=' +
            AnsiQuotedStr( 'login', '"') + sLineBreak + sLineBreak + UsuarioWeb + sLineBreak +
            '--' + FPBound + sLineBreak +
            'Content-Disposition: form-data; name=' +
            AnsiQuotedStr( 'senha', '"') + sLineBreak + sLineBreak + SenhaWeb + sLineBreak +
            '--' + FPBound + sLineBreak +
            'Content-Disposition: form-data; name=' +
            AnsiQuotedStr('f1', '"' ) + '; ' + 'filename=' +
            AnsiQuotedStr(GerarPrefixoArquivo + '-' + FPArqEnv + '.xml', '"') + sLineBreak +
            'Content-Type: text/xml' + sLineBreak + sLineBreak + Message + sLineBreak +
            '--' + FPBound + '--' + sLineBreak;

  FPHttpClient := FPDFeOwner.SSL.SSLHttpClass;

  FPHttpClient.Clear;
end;

{ TACBrNFSeXWebserviceMulti2 }

constructor TACBrNFSeXWebserviceMulti2.Create(AOwner: TACBrDFe; AMetodo: TMetodo;
  const AURL: string; const AMethod: string; const AMimeType: string);
begin
  inherited Create(AOwner, AMetodo, AURL, AMethod);

  FPBound := '----=_Part_3_' + IntToHex(Random(MaxInt), 8);
  FPMimeType := 'multipart/form-data; boundary=' + AnsiQuotedStr(FPBound, '"');
end;

function TACBrNFSeXWebserviceMulti2.DefinirMsgEnvio(const Message, SoapAction,
  SoapHeader: string; namespace: array of string): string;
var
  NomeArq: string;
begin
  NomeArq := GerarPrefixoArquivo + '-' + FPArqEnv + '.xml';

  Result := '--' + FPBound + sLineBreak +
            'Content-Type: text/xml; charset=Cp1252; name=' +
            NomeArq + sLineBreak +
            'Content-Transfer-Encoding: binary' + sLineBreak +
            'Content-Disposition: form-data; name=' + AnsiQuotedStr(NomeArq, '"') +
            '; filename=' + AnsiQuotedStr(NomeArq, '"') + sLineBreak +
            sLineBreak +
            Message + sLineBreak +
            '--' + FPBound + '--' + sLineBreak;

  FPHttpClient := FPDFeOwner.SSL.SSLHttpClass;

  FPHttpClient.Clear;
end;

{ TInfConsulta }

constructor TInfConsultaNFSe.Create;
begin
  tpConsulta    := tcPorNumero;
  tpPeriodo     := tpEmissao;
  tpRetorno     := trXml;
  NumeroIniNFSe := '';
  NumeroFinNFSe := '';
  SerieNFSe     := '';
  NumeroLote    := '';
  DataInicial   := 0;
  DataFinal     := 0;
  CNPJPrestador := '';
  IMPrestador   := '';
  CNPJTomador   := '';
  IMTomador     := '';
  CNPJInter     := '';
  IMInter       := '';
  RazaoInter    := '';
  CadEconomico  := '';
  CodServ       := '';
  CodVerificacao:= '';
  ChaveNFSe     := '';
  Pagina        := 1;
end;

function TInfConsultaNFSe.LerFromIni(const AIniStr: string): Boolean;
var
  sSecao: string;
  INIRec: TMemIniFile;
  Ok: Boolean;
begin
{$IFNDEF COMPILER23_UP}
  Result := False;
{$ENDIF}

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniStr, INIRec);

    sSecao := 'ConsultarNFSe';

    tpConsulta := StrTotpConsulta(Ok, INIRec.ReadString(sSecao, 'tpConsulta', '1'));
    tpPeriodo  := StrTotpPeriodo(Ok, INIRec.ReadString(sSecao, 'tpPeriodo', '1'));
    tpRetorno  := StrTotpRetorno(Ok, INIRec.ReadString(sSecao, 'tpRetorno', 'XML'));

    NumeroIniNFSe := INIRec.ReadString(sSecao, 'NumeroIniNFSe', '');
    NumeroFinNFSe := INIRec.ReadString(sSecao, 'NumeroFinNFSe', '');
    SerieNFSe     := INIRec.ReadString(sSecao, 'SerieNFSe', '');
    NumeroLote    := INIRec.ReadString(sSecao, 'NumeroLote', '');
    DataInicial   := StringToDateTime(INIRec.ReadString(sSecao, 'DataInicial', '0'));
    DataFinal     := StringToDateTime(INIRec.ReadString(sSecao, 'DataFinal', '0'));

    CNPJPrestador := INIRec.ReadString(sSecao, 'CNPJPrestador', '');
    IMPrestador   := INIRec.ReadString(sSecao, 'IMPrestador', '');
    CNPJTomador   := INIRec.ReadString(sSecao, 'CNPJTomador', '');
    IMTomador     := INIRec.ReadString(sSecao, 'IMTomador', '');
    CNPJInter     := INIRec.ReadString(sSecao, 'CNPJInter', '');
    IMInter       := INIRec.ReadString(sSecao, 'IMInter', '');
    RazaoInter    := INIRec.ReadString(sSecao, 'RazaoInter', '');
    CadEconomico  := INIRec.ReadString(sSecao, 'CadEconomico', '');
    CodServ       := INIRec.ReadString(sSecao, 'CodServ', '');
    CodVerificacao := INIRec.ReadString(sSecao, 'CodVerificacao', '');
    ChaveNFSe     := INIRec.ReadString(sSecao, 'ChaveNFSe', '');
    Pagina        := INIRec.ReadInteger(sSecao, 'Pagina', 1);

    Result := True;
  finally
    INIRec.Free;
  end;
end;

{ TInfCancelamento }

constructor TInfCancelamento.Create;
begin
  FNumeroNFSe := '';
  FSerieNFSe := '';
  FChaveNFSe := '';
  FDataEmissaoNFSe := 0;
  FCodCancelamento := '';
  FMotCancelamento := '';
  FNumeroLote := '';
  FNumeroRps := 0;
  FSerieRps := '';
  FValorNFSe := 0.0;
  FCodVerificacao := '';
  Femail := '';
  FNumeroNFSeSubst := '';
  FSerieNFSeSubst := '';
  FCodServ := '';
end;

function TInfCancelamento.LerFromIni(const AIniStr: string): Boolean;
var
  sSecao: string;
  INIRec: TMemIniFile;
begin
{$IFNDEF COMPILER23_UP}
  Result := False;
{$ENDIF}

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniStr, INIRec);

    sSecao := 'CancelarNFSe';

    NumeroNFSe      := INIRec.ReadString(sSecao, 'NumeroNFSe', '');
    SerieNFSe       := INIRec.ReadString(sSecao, 'SerieNFSe', '');
    ChaveNFSe       := INIRec.ReadString(sSecao, 'ChaveNFSe', '');
    DataEmissaoNFSe := INIRec.ReadDateTime(sSecao, 'DataEmissaoNFSe', 0);
    CodCancelamento := INIRec.ReadString(sSecao, 'CodCancelamento', '');
    MotCancelamento := INIRec.ReadString(sSecao, 'MotCancelamento', '');
    NumeroLote      := INIRec.ReadString(sSecao, 'NumeroLote', '');
    NumeroRps       := INIRec.ReadInteger(sSecao, 'NumeroRps', 0);
    SerieRps        := INIRec.ReadString(sSecao, 'SerieRps', '');
    ValorNFSe       := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorNFSe', ''), 0);
    CodVerificacao  := INIRec.ReadString(sSecao, 'CodVerificacao', '');
    email           := INIRec.ReadString(sSecao, 'email', '');
    NumeroNFSeSubst := INIRec.ReadString(sSecao, 'NumeroNFSeSubst', '');
    SerieNFSeSubst  := INIRec.ReadString(sSecao, 'SerieNFSeSubst', '');
    CodServ         := INIRec.ReadString(sSecao, 'CodServ', '');

    Result := True;
  finally
    INIRec.Free;
  end;
end;

{ TInfEvento }

constructor TInfEvento.Create;
begin
  FverAplic := '';
  FambGer := 0;
  FnSeqEvento := 0;
  FnDFe := '';

  FpedRegEvento := TpedRegEvento.Create;
end;

destructor TInfEvento.Destroy;
begin
  FpedRegEvento.Free;

  inherited Destroy;
end;

function TInfEvento.LerFromIni(const AIniStr: string): Boolean;
var
  sSecao: string;
  INIRec: TMemIniFile;
  Ok: Boolean;
begin
{$IFNDEF COMPILER23_UP}
  Result := False;
{$ENDIF}

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniStr, INIRec);

    sSecao := 'Evento';

    with pedRegEvento do
    begin
      tpAmb := INIRec.ReadInteger(sSecao, 'tpAmb', 0);
      verAplic := INIRec.ReadString(sSecao, 'verAplic', '');
      dhEvento := INIRec.ReadDateTime(sSecao, 'dhEvento', 0);
      chNFSe := INIRec.ReadString(sSecao, 'chNFSe', '');
      nPedRegEvento := INIRec.ReadInteger(sSecao, 'nPedRegEvento', 0);
      tpEvento := StrTotpEvento(Ok, INIRec.ReadString(sSecao, 'tpEvento', 'e101101'));
      cMotivo := INIRec.ReadInteger(sSecao, 'cMotivo', 0);
      xMotivo := INIRec.ReadString(sSecao, 'xMotivo', '');
      chSubstituta := INIRec.ReadString(sSecao, 'chSubstituta', '');
    end;

    Result := True;
  finally
    INIRec.Free;
  end;
end;

{ TpedRegEvento }

constructor TpedRegEvento.Create;
begin
  FtpAmb := 0;
  FverAplic := '';
  FdhEvento := 0;
  FchNFSe := '';
  FnPedRegEvento := 0;
  FtpEvento := teCancelamento;
  FcMotivo := 0;
  FxMotivo := '';
  FchSubstituta := '';
end;

{ TInfConsultaLinkNFSe }

constructor TInfConsultaLinkNFSe.Create;
begin
  FNumeroNFSe := '';
  FSerieNFSe := '';
  FNumeroRps := 0;
  FSerieRps := '';
  FCompetencia := 0;
  FDtEmissao := 0;
  FTipoRps := '';
  FPagina := 0;
end;

function TInfConsultaLinkNFSe.LerFromIni(const AIniStr: string): Boolean;
var
  sSecao: string;
  INIRec: TMemIniFile;
begin
{$IFNDEF COMPILER23_UP}
  Result := False;
{$ENDIF}

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniStr, INIRec);

    sSecao := 'ConsultarLinkNFSe';

    Competencia := INIRec.ReadDateTime(sSecao, 'Competencia', 0);
    DtEmissao := INIRec.ReadDateTime(sSecao, 'DtEmissao', 0);
    NumeroRps := INIRec.ReadInteger(sSecao, 'NumeroRps', 0);
    NumeroNFSe := INIRec.ReadString(sSecao, 'NumeroNFSe', '');
    SerieNFSe := INIRec.ReadString(sSecao, 'SerieNFSe', '');
    NumeroRps := INIRec.ReadInteger(sSecao, 'NumeroRps', 0);
    SerieRps := INIRec.ReadString(sSecao, 'SerieRps', '');
    TipoRps := INIRec.ReadString(sSecao, 'TipoRps', '');
    Pagina := INIRec.ReadInteger(sSecao, 'Pagina', 0);

    Result := True;
  finally
    INIRec.Free;
  end;
end;

end.

