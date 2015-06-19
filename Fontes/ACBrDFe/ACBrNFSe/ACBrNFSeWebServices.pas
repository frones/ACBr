{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{  de Serviço eletrônica - NFSe                                                }

{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrNFSeWebServices;

interface

uses
  Classes, SysUtils,
  ACBrDFe, ACBrDFeWebService,
  pnfsNFSe, pcnAuxiliar, pcnConversao, pnfsConversao,
  ACBrNFSeNotasFiscais, ACBrNFSeConfiguracoes,

  pnfsEnvLoteRpsResposta, pnfsConsSitLoteRpsResposta,
  pnfsConsLoteRpsResposta, pnfsConsNfseporRpsResposta,
  pnfsConsNfseResposta, pnfsCancNfseResposta,
  pnfsGerarNfseResposta, pnfsSubsNfseResposta;

type

  { TNFSeWebService }

  TNFSeWebService = class(TDFeWebService)
  private
  protected
    FPConfiguracoesNFSe: TConfiguracoesNFSe;
    FPStatus: TStatusACBrNFSe;
    FPLayout: TLayOutNFSe;
    FNameSpaceDad: String;
    FNameSpaceCab: String;
    FURI: String;
    FTagI: String;
    FTagF: String;
    FDadosSenha: String;
    FDadosEnvelope: String;

    procedure InicializarServico; override;
    procedure DefinirURL; override;
    function GerarVersaoDadosSoap: String; override;
    function GerarCabecalhoSoap: String; override;
    procedure FinalizarServico; override;
    procedure DefinirEnvelopeSoap; override;

  public
    constructor Create(AOwner: TACBrDFe); override;

    property Status: TStatusACBrNFSe read FPStatus;
    property Layout: TLayOutNFSe read FPLayout;
    property NameSpaceCab: String read FNameSpaceCab;
    property NameSpaceDad: String read FNameSpaceDad;
    property URI: String read FURI;
    property TagI: String read FTagI;
    property TagF: String read FTagF;
    property DadosSenha: String read FDadosSenha;
    property DadosEnvelope: String read FDadosEnvelope;
  end;

  { TNFSeGerarLoteRPS }

  TNFSeGerarLoteRPS = Class(TNFSeWebService)
  private
    FNotasFiscais: TNotasFiscais;
    FNumeroLote: String;
  protected
    procedure EnviarDados; override;
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    procedure FinalizarServico; override;

    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
      reintroduce; overload;
    destructor Destroy; override;

    property NumeroLote: String read FNumeroLote;
  end;

  { TNFSeEnviarLoteRPS }

  TNFSeEnviarLoteRPS = class(TNFSeWebService)
  private
    FNotasFiscais: TNotasFiscais;
    FNumeroLote: String;
    FDataRecebimento: TDateTime;
    FProtocolo: String;
    FNFSeRetorno: TretEnvLote;

    function GetLote: String;
    function GetProtocolo: String;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    procedure FinalizarServico; override;

    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
      reintroduce; overload;
    destructor Destroy; override;

    property NumeroLote: String read FNumeroLote;
    property DataRecebimento: TDateTime read FDataRecebimento;
    property Protocolo: String read FProtocolo;
    property NFSeRetorno: TretEnvLote read FNFSeRetorno write FNFSeRetorno;
  end;

(*
  { TNFSeRetRecepcao }

  TNFSeRetRecepcao = class(TNFSeWebService)
  private
    FRecibo: String;
    FProtocolo: String;
    FChaveNFSe: String;
    FNotasFiscais: TNotasFiscais;
    Fversao: String;
    FTpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FcUF: Integer;
    FxMotivo: String;
    FcMsg: Integer;
    FxMsg: String;

    FNFSeRetorno: TRetConsReciNFSe;

    function GetRecibo: String;
    function TratarRespostaFinal: Boolean;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    procedure FinalizarServico; override;

    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
      reintroduce; overload;
    destructor Destroy; override;

    function Executar: Boolean; override;

    property versao: String read Fversao;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;
    property verAplic: String read FverAplic;
    property cStat: Integer read FcStat;
    property cUF: Integer read FcUF;
    property xMotivo: String read FxMotivo;
    property cMsg: Integer read FcMsg;
    property xMsg: String read FxMsg;
    property Recibo: String read GetRecibo write FRecibo;
    property Protocolo: String read FProtocolo write FProtocolo;
    property ChaveNFSe: String read FChaveNFSe write FChaveNFSe;

    property NFSeRetorno: TRetConsReciNFSe read FNFSeRetorno;
  end;

  { TNFSeRecibo }

  TNFSeRecibo = class(TNFSeWebService)
  private
    FRecibo: String;
    Fversao: String;
    FTpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FcUF: Integer;
    FxMsg: String;
    FcMsg: Integer;

    FNFSeRetorno: TRetConsReciNFSe;
  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirURL; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;

    property versao: String read Fversao;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;
    property verAplic: String read FverAplic;
    property cStat: Integer read FcStat;
    property xMotivo: String read FxMotivo;
    property cUF: Integer read FcUF;
    property xMsg: String read FxMsg;
    property cMsg: Integer read FcMsg;
    property Recibo: String read FRecibo write FRecibo;

    property NFSeRetorno: TRetConsReciNFSe read FNFSeRetorno;
  end;

  { TNFSeConsulta }

  TNFSeConsulta = class(TNFSeWebService)
  private
    FNFSeChave: String;
    FProtocolo: String;
    FDhRecbto: TDateTime;
    FXMotivo: String;
    Fversao: String;
    FTpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FcUF: Integer;

    FprotNFSe: TProcNFSe;
    FretCancNFSe: TRetCancNFSe;
    FprocEventoNFSe: TRetEventoNFSeCollection;
  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;

    property NFSeChave: String read FNFSeChave write FNFSeChave;
    property Protocolo: String read FProtocolo write FProtocolo;
    property DhRecbto: TDateTime read FDhRecbto write FDhRecbto;
    property XMotivo: String read FXMotivo write FXMotivo;
    property versao: String read Fversao;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;
    property verAplic: String read FverAplic;
    property cStat: Integer read FcStat;
    property cUF: Integer read FcUF;

    property protNFSe: TProcNFSe read FprotNFSe;
    property retCancNFSe: TRetCancNFSe read FretCancNFSe;
    property procEventoNFSe: TRetEventoNFSeCollection read FprocEventoNFSe;
  end;
  *)

  { TNFSeEnvioWebService }

  TNFSeEnvioWebService = class(TNFSeWebService)
  private
    FXMLEnvio: String;
    FPURLEnvio: String;
    FVersao: String;
    FSoapActionEnvio: String;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgErro(E: Exception): String; override;
    function GerarVersaoDadosSoap: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;
    function Executar: Boolean; override;

    property XMLEnvio: String read FXMLEnvio write FXMLEnvio;
    property URLEnvio: String read FPURLEnvio write FPURLEnvio;
    property SoapActionEnvio: String read FSoapActionEnvio write FSoapActionEnvio;
  end;

  { TWebServices }

  TWebServices = class
  private
    FACBrNFSe: TACBrDFe;
    FGerarLoteRPS: TNFSeGerarLoteRPS;
    FEnviarLoteRPS: TNFSeEnviarLoteRPS;
    (*
    FRetorno: TNFSeRetRecepcao;
    FRecibo: TNFSeRecibo;
    FConsulta: TNFSeConsulta;
   *)
    FEnvioWebService: TNFSeEnvioWebService;

  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    function GeraLote(ALote: Integer): Boolean; overload;
    function GeraLote(ALote: String): Boolean; overload;

    function Envia(ALote: Integer): Boolean; overload;
    function Envia(ALote: String): Boolean; overload;

    property ACBrNFSe: TACBrDFe read FACBrNFSe write FACBrNFSe;
    property GerarLoteRPS: TNFSeGerarLoteRPS read FGerarLoteRPS write FGerarLoteRPS;
    property EnviarLoteRPS: TNFSeEnviarLoteRPS read FEnviarLoteRPS write FEnviarLoteRPS;
    (*
    property Retorno: TNFSeRetRecepcao read FRetorno write FRetorno;
    property Recibo: TNFSeRecibo read FRecibo write FRecibo;
    property Consulta: TNFSeConsulta read FConsulta write FConsulta;
    *)
    property EnvioWebService: TNFSeEnvioWebService read FEnvioWebService write FEnvioWebService;
  end;

implementation

uses
  StrUtils, Math,
  ACBrUtil, ACBrNFSe, pnfsNFSeG,
  pcnGerador, pcnLeitor;

{ TNFSeWebService }

constructor TNFSeWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPConfiguracoesNFSe := TConfiguracoesNFSe(FPConfiguracoes);
  FPLayout := LayNfseRecepcaoLote;
  FPStatus := stNFSeIdle;
end;

procedure TNFSeWebService.DefinirEnvelopeSoap;
var
  Texto: String;
begin

  {$IFDEF UNICODE}
   Texto := '<' + ENCODING_UTF8 + '>';    // Envelope já está sendo montado em UTF8
  {$ELSE}
   Texto := '';  // Isso forçará a conversão para UTF8, antes do envio
  {$ENDIF}

  Texto := FDadosEnvelope;
  // %CabMsg%   : Representa a Mensagem de Cabeçalho
  // %DadosMsg% : Representa a Mensagem de Dados
  Texto := stringReplace(Texto, '%CabMsg%', FPCabMsg, [rfReplaceAll]);
  Texto := stringReplace(Texto, '%DadosMsg%', FPDadosMsg, [rfReplaceAll]);

  FPEnvelopeSoap := Texto;
end;

procedure TNFSeWebService.InicializarServico;
begin
  { Sobrescrever apenas se necessário }
  inherited InicializarServico;

  TACBrNFSe(FPDFeOwner).SetStatus(FPStatus);
end;

procedure TNFSeWebService.DefinirURL;
var
  Versao: Double;
begin
  { sobrescrever apenas se necessário.
    Você também pode mudar apenas o valor de "FLayoutServico" na classe
    filha e chamar: Inherited;     }

  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';

  TACBrNFSe(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;


function TNFSeWebService.GerarVersaoDadosSoap: String;
begin
  { Sobrescrever apenas se necessário }

  if EstaVazio(FPVersaoServico) then
    FPVersaoServico := TACBrNFSe(FPDFeOwner).LerVersaoDeParams(FPLayout);

  Result := '<versaoDados>' + FPVersaoServico + '</versaoDados>';
end;

procedure TNFSeWebService.FinalizarServico;
begin
  { Sobrescrever apenas se necessário }

  TACBrNFSe(FPDFeOwner).SetStatus(stNFSeIdle);
end;

function TNFSeWebService.GerarCabecalhoSoap: String;
begin
 Result := FPCabMsg;
end;

{ TNFSeGerarLoteRPS }

constructor TNFSeGerarLoteRPS.Create(AOwner: TACBrDFe;
  ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;

  FPStatus := stNFSeRecepcao;
  FPLayout := LayNfseRecepcaoLote;
  FPArqEnv := 'lot-rps';
  FPArqResp := ''; // O lote é apenas gerado não existe o envio
end;

destructor TNFSeGerarLoteRPS.Destroy;
begin
  inherited Destroy;
end;

procedure TNFSeGerarLoteRPS.EnviarDados;
begin
  // O Gerar Lote RPS não ocorre o envio para o Web Service
end;

procedure TNFSeGerarLoteRPS.DefinirURL;
begin
  FPLayout := LayNfseRecepcaoLote;
  inherited DefinirURL;
end;

procedure TNFSeGerarLoteRPS.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'NFSeGerarLoteRPS';
  FPSoapAction := FPServico;
end;

procedure TNFSeGerarLoteRPS.DefinirDadosMsg;
var
  I: integer;
  URI,
  Separador,
  vNotas,
  NameSpace,
  ServicoEnviar,
  DefTipos,
  Cabecalho,
  Prefixo2,
  Prefixo3,
  Prefixo4: String;
begin
  vNotas := '';

  NameSpace := FPConfiguracoesNFSe.Geral.ConfigXML.NameSpace;
  DefTipos := FPConfiguracoesNFSe.Geral.ConfigSchemas.DefTipos;
  ServicoEnviar := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoEnviar;
  Cabecalho := FPConfiguracoesNFSe.Geral.ConfigSchemas.Cabecalho;
  Prefixo2 := FPConfiguracoesNFSe.Geral.ConfigGeral.Prefixo2;
  Prefixo3 := FPConfiguracoesNFSe.Geral.ConfigGeral.Prefixo3;
  Prefixo4 := FPConfiguracoesNFSe.Geral.ConfigGeral.Prefixo4;

  if RightStr(NameSpace, 1) = '/' then
    Separador := ''
  else
    Separador := '/';

  if Cabecalho <> '' then
  begin
    if Prefixo2 <> '' then
      FNameSpaceCab := ' xmlns:' + StringReplace(Prefixo2, ':', '', []) +
                       '="' + NameSpace + Separador + Cabecalho +'">'
    else
      FNameSpaceCab := ' xmlns="' + NameSpace + Separador + Cabecalho +'">';
  end
  else
    FNameSpaceCab := '>';

  if FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoEnviar <> '' then
  begin
    if (FPConfiguracoesNFSe.Geral.Provedor = proIssDSF) then
      FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + NameSpace + '" '
    else
      if (FPConfiguracoesNFSe.Geral.Provedor = proInfisc) then
        FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + NameSpace + '" '
      else begin
        if (RightStr(NameSpace, 1) = '/') then
        begin
          if Prefixo3 <> '' then
            FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + NameSpace + Separador + ServicoEnviar + '"'
          else
            FNameSpaceDad := 'xmlns="' + NameSpace + Separador + ServicoEnviar + '"';
        end
        else begin
          if Prefixo3 <> '' then
            FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + NameSpace + '"'
          else
            FNameSpaceDad := 'xmlns="' + NameSpace + '"';
        end;
      end;
  end
  else
    FNameSpaceDad := '';

  if (DefTipos = '') and (NameSpaceDad <> '') then
    FNameSpaceDad := FNameSpaceDad + '>';

  if DefTipos <> '' then
  begin
    if Prefixo4 <> '' then
      FNameSpaceDad := FNameSpaceDad + ' xmlns:' +
                       StringReplace(Prefixo4, ':', '', []) + '="' + NameSpace + Separador + DefTipos + '">'
    else
      FNameSpaceDad := FNameSpaceDad + ' xmlns="' + NameSpace + Separador + DefTipos + '">';
  end;

  if FNameSpaceDad = '' then
    FNameSpaceDad := '>'
  else
    FNameSpaceDad := ' ' + FNameSpaceDad;

  if FPConfiguracoesNFSe.Geral.ConfigAssinar.RPS then
  begin
    for I := 0 to FNotasFiscais.Count - 1 do
       vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                             '<' + Prefixo4 + 'InfRps' +
                                RetornarConteudoEntre(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].XMLAssinado,
                                     '<' + Prefixo4 + 'InfRps', '</Rps>') +
                          '</' + Prefixo4 + 'Rps>';
  end
  else begin
    for I := 0 to FNotasFiscais.Count - 1 do
       vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                             '<' + Prefixo4 + 'InfRps' +
                                RetornarConteudoEntre(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].XMLOriginal,
                                     '<' + Prefixo4 + 'InfRps', '</Rps>') +
                          '</' + Prefixo4 + 'Rps>';
  end;

  FPCabMsg := FPConfiguracoesNFSe.Geral.ConfigEnvelope.CabecalhoMsg;
  FURI := '';
//  FURI := FProvedorClass.GetURI(URI);
  FTagI := '<' + Prefixo3 + 'EnviarLoteEnvio' + FNameSpaceDad;
  FTagF := '</' + Prefixo3 + 'EnviarLoteEnvio>';
  FDadosSenha := '';
//  FDadosSenha := FProvedorClass.Gera_DadosSenha(FConfiguracoes.WebServices.UserWeb, FConfiguracoes.WebServices.SenhaWeb);

  FPDadosMsg := TNFSeG.Gera_DadosMsgEnviarLote(Prefixo3, Prefixo4,
                                               FPConfiguracoesNFSe.Geral.ConfigGeral.Identificador,
                                               NameSpace,
                                               FPConfiguracoesNFSe.Geral.ConfigXML.VersaoDados,
                                               FPConfiguracoesNFSe.Geral.ConfigXML.VersaoXML,
                                               TNFSeGerarLoteRps(Self).NumeroLote,
                                               OnlyNumber(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj),
                                               TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal,
                                               IntToStr(TNFSeGerarLoteRps(Self).FNotasFiscais.Count),
                                               vNotas,
                                               FTagI, FTagF, FPConfiguracoesNFSe.Geral.Provedor);

  FDadosEnvelope := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Recepcionar;

  // Lote tem mais de 500kb ? //
  if Length(FPDadosMsg) > (500 * 1024) then
    GerarException(ACBrStr('Tamanho do XML de Dados superior a 500 Kbytes. Tamanho atual: ' +
      IntToStr(trunc(Length(FPDadosMsg) / 1024)) + ' Kbytes'));
end;

function TNFSeGerarLoteRPS.TratarResposta: Boolean;
begin
  TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NomeArq :=
    FPConfiguracoes.Arquivos.PathSalvar +
    GerarPrefixoArquivo + '-' + FPArqEnv + '.xml';
end;

procedure TNFSeGerarLoteRPS.FinalizarServico;
begin
  inherited;
{a}
end;

function TNFSeGerarLoteRPS.GerarMsgLog: String;
begin
{a}
end;

function TNFSeGerarLoteRPS.GerarPrefixoArquivo: String;
begin
  Result := NumeroLote;
end;

{ TNFSeEnviarLoteRPS }

constructor TNFSeEnviarLoteRPS.Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;
//  FSincrono := False;

  FPStatus := stNFSeRecepcao;
  FPLayout := LayNfseRecepcaoLote;
  FPArqEnv := 'env-lot';
  FPArqResp := 'rec';

//  FNFSeRetornoSincrono := nil;
  FNFSeRetorno := nil;
end;

destructor TNFSeEnviarLoteRPS.Destroy;
begin
//  if Assigned(FNFSeRetornoSincrono) then
//    FNFSeRetornoSincrono.Free;

  if Assigned(FNFSeRetorno) then
    FNFSeRetorno.Free;

  inherited Destroy;
end;

function TNFSeEnviarLoteRPS.GetLote: String;
begin
  Result := Trim(FNumeroLote);
end;

function TNFSeEnviarLoteRPS.GetProtocolo: String;
begin
  Result := Trim(FProtocolo);
end;

procedure TNFSeEnviarLoteRPS.DefinirURL;
begin

  inherited DefinirURL;
end;

procedure TNFSeEnviarLoteRPS.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'NFSeEnviarLoteRPS2';
  FPSoapAction := FPServico;
end;

procedure TNFSeEnviarLoteRPS.DefinirDadosMsg;
var
  I: Integer;
  vNotas: String;
  indSinc: String;
begin
(*
  if (FPLayout = LayNFSeAutorizacao) or (FPConfiguracoesNFSe.Geral.ModeloDF = moNFCe) or
    (FPConfiguracoesNFSe.Geral.VersaoDF = ve310) then
    indSinc := '<indSinc>' + IfThen(FSincrono, '1', '0') + '</indSinc>'
  else
    indSinc := '';

  vNotas := '';
  for I := 0 to FNotasFiscais.Count - 1 do
    vNotas := vNotas + '<NFSe' + RetornarConteudoEntre(
      FNotasFiscais.Items[I].XMLAssinado, '<NFSe', '</NFSe>') + '</NFSe>';

  FPDadosMsg := '<enviNFSe xmlns="http://www.portalfiscal.inf.br/NFSe" versao="' +
    FPVersaoServico + '">' + '<idLote>' + FLote + '</idLote>' + indSinc +
    vNotas + '</enviNFSe>';

  // Lote tem mais de 500kb ? //
  if Length(FPDadosMsg) > (500 * 1024) then
    GerarException(ACBrStr('Tamanho do XML de Dados superior a 500 Kbytes. Tamanho atual: ' +
      IntToStr(trunc(Length(FPDadosMsg) / 1024)) + ' Kbytes'));
  FRecibo := '';
*)
end;

function TNFSeEnviarLoteRPS.TratarResposta: Boolean;
var
  I: Integer;
  chNFSe, NomeArquivo: String;
//  AProcNFSe: TProcNFSe;
begin
(*
  if FPLayout = LayNFSeAutorizacao then
  begin
    FPRetWS := SeparaDados(FPRetornoWS, 'NFSeAutorizacaoLoteResult');
    if FPRetWS = '' then
      FPRetWS := SeparaDados(FPRetornoWS, 'NFSeAutorizacaoResult');
  end
  else
    FPRetWS := SeparaDados(FPRetornoWS, 'NFSeEnviarLoteRPSLote2Result');

  if ((FPConfiguracoesNFSe.Geral.ModeloDF = moNFCe) or
    (FPConfiguracoesNFSe.Geral.VersaoDF = ve310)) and FSincrono then
  begin
    FNFSeRetornoSincrono := TRetConsSitNFSe.Create;

    if pos('retEnviNFSe', FPRetWS) > 0 then
      FNFSeRetornoSincrono.Leitor.Arquivo :=
        StringReplace(FPRetWS, 'retEnviNFSe', 'retConsSitNFSe',
        [rfReplaceAll, rfIgnoreCase])
    else if pos('retConsReciNFSe', FPRetWS) > 0 then
      FNFSeRetornoSincrono.Leitor.Arquivo :=
        StringReplace(FPRetWS, 'retConsReciNFSe', 'retConsSitNFSe',
        [rfReplaceAll, rfIgnoreCase])
    else
      FNFSeRetornoSincrono.Leitor.Arquivo := FPRetWS;

    FNFSeRetornoSincrono.LerXml;

    Fversao := FNFSeRetornoSincrono.versao;
    FTpAmb := FNFSeRetornoSincrono.TpAmb;
    FverAplic := FNFSeRetornoSincrono.verAplic;

    // Consta no Retorno da NFC-e
    FRecibo := FNFSeRetornoSincrono.nRec;
    FcUF := FNFSeRetornoSincrono.cUF;
    chNFSe := FNFSeRetornoSincrono.ProtNFSe.chNFSe;

    if (FNFSeRetornoSincrono.protNFSe.cStat > 0) then
      FcStat := FNFSeRetornoSincrono.protNFSe.cStat
    else
      FcStat := FNFSeRetornoSincrono.cStat;

    if (FNFSeRetornoSincrono.protNFSe.xMotivo <> '') then
    begin
      FPMsg := FNFSeRetornoSincrono.protNFSe.xMotivo;
      FxMotivo := FNFSeRetornoSincrono.protNFSe.xMotivo;
    end
    else
    begin
      FPMsg := FNFSeRetornoSincrono.xMotivo;
      FxMotivo := FNFSeRetornoSincrono.xMotivo;
    end;

    // Verificar se a NF-e foi autorizada com sucesso
    Result := (FNFSeRetornoSincrono.cStat = 104) and
      (TACBrNFSe(FPDFeOwner).CstatProcessado(FNFSeRetornoSincrono.protNFSe.cStat));

    NomeArquivo := PathWithDelim(FPConfiguracoesNFSe.Arquivos.PathSalvar) + chNFSe;

    if Result then
    begin
      for I := 0 to TACBrNFSe(FPDFeOwner).NotasFiscais.Count - 1 do
      begin
        if OnlyNumber(chNFSe) = TACBrNFSe(FPDFeOwner).NotasFiscais.Items[I].NumID then
        begin
          if (TACBrNFSe(FPDFeOwner).Configuracoes.Geral.ValidarDigest) and
            (TACBrNFSe(FPDFeOwner).NotasFiscais.Items[I].NFSe.signature.DigestValue <>
            FNFSeRetornoSincrono.protNFSe.digVal) and
            (FNFSeRetornoSincrono.protNFSe.digVal <> '') then
          begin
            raise EACBrNFSeException.Create('DigestValue do documento ' +
              TACBrNFSe(FPDFeOwner).NotasFiscais.Items[I].NumID + ' não coNFSere.');
          end;
          with TACBrNFSe(FPDFeOwner).NotasFiscais.Items[I] do
          begin
            NFSe.procNFSe.cStat := FNFSeRetornoSincrono.protNFSe.cStat;
            NFSe.procNFSe.tpAmb := FNFSeRetornoSincrono.tpAmb;
            NFSe.procNFSe.verAplic := FNFSeRetornoSincrono.verAplic;
            NFSe.procNFSe.chNFSe := FNFSeRetornoSincrono.ProtNFSe.chNFSe;
            NFSe.procNFSe.dhRecbto := FNFSeRetornoSincrono.protNFSe.dhRecbto;
            NFSe.procNFSe.nProt := FNFSeRetornoSincrono.ProtNFSe.nProt;
            NFSe.procNFSe.digVal := FNFSeRetornoSincrono.protNFSe.digVal;
            NFSe.procNFSe.xMotivo := FNFSeRetornoSincrono.protNFSe.xMotivo;
          end;

          if (FileExists(NomeArquivo + '-NFSe.xml')) or
            NaoEstaVazio(TACBrNFSe(FPDFeOwner).NotasFiscais.Items[I].NomeArq) then
          begin
            AProcNFSe := TProcNFSe.Create;
            try
              if NaoEstaVazio(TACBrNFSe(
                FPDFeOwner).NotasFiscais.Items[I].NomeArq) then
                AProcNFSe.PathNFSe := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[I].NomeArq
              else
                AProcNFSe.PathNFSe := NomeArquivo + '-NFSe.xml';

              AProcNFSe.PathRetConsSitNFSe := '';
              AProcNFSe.PathRetConsReciNFSe := '';
              AProcNFSe.tpAmb := FNFSeRetornoSincrono.protNFSe.tpAmb;
              AProcNFSe.verAplic := FNFSeRetornoSincrono.protNFSe.verAplic;
              AProcNFSe.chNFSe := FNFSeRetornoSincrono.protNFSe.chNFSe;
              AProcNFSe.dhRecbto := FNFSeRetornoSincrono.protNFSe.dhRecbto;
              AProcNFSe.nProt := FNFSeRetornoSincrono.protNFSe.nProt;
              AProcNFSe.digVal := FNFSeRetornoSincrono.protNFSe.digVal;
              AProcNFSe.cStat := FNFSeRetornoSincrono.protNFSe.cStat;
              AProcNFSe.xMotivo := FNFSeRetornoSincrono.protNFSe.xMotivo;

              AProcNFSe.Versao := FPVersaoServico;
              AProcNFSe.GerarXML;

              if NaoEstaVazio(AProcNFSe.Gerador.ArquivoFormatoXML) then
                AProcNFSe.Gerador.SalvarArquivo(AProcNFSe.PathNFSe);
            finally
              AProcNFSe.Free;
            end;
          end;

          if FPConfiguracoesNFSe.Arquivos.Salvar then
          begin
            if FPConfiguracoesNFSe.Arquivos.SalvarApenasNFSeProcessadas then
            begin
              if TACBrNFSe(FPDFeOwner).NotasFiscais.Items[I].Processada then
                TACBrNFSe(FPDFeOwner).NotasFiscais.Items[I].GravarXML;
            end
            else
              TACBrNFSe(FPDFeOwner).NotasFiscais.Items[I].GravarXML;
          end;

          Break;
        end;
      end;
    end;
  end
  else
  begin
    FNFSeRetorno := TretEnvNFSe.Create;

    FNFSeRetorno.Leitor.Arquivo := FPRetWS;
    FNFSeRetorno.LerXml;

    Fversao := FNFSeRetorno.versao;
    FTpAmb := FNFSeRetorno.TpAmb;
    FverAplic := FNFSeRetorno.verAplic;
    FcStat := FNFSeRetorno.cStat;
    FxMotivo := FNFSeRetorno.xMotivo;
    FdhRecbto := FNFSeRetorno.infRec.dhRecbto;
    FTMed := FNFSeRetorno.infRec.tMed;
    FcUF := FNFSeRetorno.cUF;
    FPMsg := FNFSeRetorno.xMotivo;
    FRecibo := FNFSeRetorno.infRec.nRec;

    Result := (FNFSeRetorno.CStat = 103);
  end;
*)
end;

procedure TNFSeEnviarLoteRPS.FinalizarServico;
begin
  inherited FinalizarServico;

//  if Assigned(FNFSeRetornoSincrono) then
//    FreeAndNil(FNFSeRetornoSincrono);

  if Assigned(FNFSeRetorno) then
    FreeAndNil(FNFSeRetorno);
end;

function TNFSeEnviarLoteRPS.GerarMsgLog: String;
begin
(*
  if Assigned(FNFSeRetorno) then
    Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                             'Ambiente: %s ' + LineBreak +
                             'Versão Aplicativo: %s ' + LineBreak +
                             'Status Código: %s ' + LineBreak +
                             'Status Descrição: %s ' + LineBreak +
                             'UF: %s ' + sLineBreak +
                             'Recibo: %s ' + LineBreak +
                             'Recebimento: %s ' + LineBreak +
                             'Tempo Médio: %s ' + LineBreak),
                     [FNFSeRetorno.versao,
                      TpAmbToStr(FNFSeRetorno.TpAmb),
                      FNFSeRetorno.verAplic,
                      IntToStr(FNFSeRetorno.cStat),
                      FNFSeRetorno.xMotivo,
                      CodigoParaUF(FNFSeRetorno.cUF),
                      FNFSeRetorno.infRec.nRec,
                      IfThen(FNFSeRetorno.InfRec.dhRecbto = 0, '',
                             FormatDateTimeBr(FNFSeRetorno.InfRec.dhRecbto)),
                      IntToStr(FNFSeRetorno.InfRec.TMed)])
  else
    Result := '';
*)
end;

function TNFSeEnviarLoteRPS.GerarPrefixoArquivo: String;
begin
(*
  if Assigned(FNFSeRetornoSincrono) then  // Esta procesando nome do Retorno Sincrono ?
  begin
    if FRecibo <> '' then
    begin
      Result := Recibo;
      FPArqResp := 'pro-rec';
    end
    else
    begin
      Result := Lote;
      FPArqResp := 'pro-lot';
    end;
  end
  else
    Result := Lote;
*)
end;

(*
{ TNFSeRetRecepcao }

constructor TNFSeRetRecepcao.Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;
  FNFSeRetorno := TRetConsReciNFSe.Create;

  FPStatus := stNFSeRetRecepcao;
  FPLayout := LayNFSeRetRecepcao;
  FPArqEnv := 'ped-rec';
  FPArqResp := 'pro-rec';
end;

destructor TNFSeRetRecepcao.Destroy;
begin
  FNFSeRetorno.Free;

  inherited Destroy;
end;

function TNFSeRetRecepcao.GetRecibo: String;
begin
  Result := Trim(FRecibo);
end;

function TNFSeRetRecepcao.TratarRespostaFinal: Boolean;
var
  I, J: Integer;
  AProcNFSe: TProcNFSe;
  AInfProt: TProtNFSeCollection;
begin
  Result := False;

  AInfProt := FNFSeRetorno.ProtNFSe;

  if (AInfProt.Count > 0) then
  begin
    FPMsg := FNFSeRetorno.ProtNFSe.Items[0].xMotivo;
    FxMotivo := FNFSeRetorno.ProtNFSe.Items[0].xMotivo;
  end;

  //Setando os retornos das notas fiscais;
  for I := 0 to AInfProt.Count - 1 do
  begin
    for J := 0 to FNotasFiscais.Count - 1 do
    begin
      if OnlyNumber(AInfProt.Items[I].chNFSe) = FNotasFiscais.Items[J].NumID then
      begin
        if (TACBrNFSe(FPDFeOwner).Configuracoes.Geral.ValidarDigest) and
          (FNotasFiscais.Items[J].NFSe.signature.DigestValue <>
          AInfProt.Items[I].digVal) and (AInfProt.Items[I].digVal <> '') then
        begin
          raise EACBrNFSeException.Create('DigestValue do documento ' +
            FNotasFiscais.Items[J].NumID + ' não coNFSere.');
        end;

        FNotasFiscais.Items[J].NFSe.procNFSe.tpAmb := AInfProt.Items[I].tpAmb;
        FNotasFiscais.Items[J].NFSe.procNFSe.verAplic := AInfProt.Items[I].verAplic;
        FNotasFiscais.Items[J].NFSe.procNFSe.chNFSe := AInfProt.Items[I].chNFSe;
        FNotasFiscais.Items[J].NFSe.procNFSe.dhRecbto := AInfProt.Items[I].dhRecbto;
        FNotasFiscais.Items[J].NFSe.procNFSe.nProt := AInfProt.Items[I].nProt;
        FNotasFiscais.Items[J].NFSe.procNFSe.digVal := AInfProt.Items[I].digVal;
        FNotasFiscais.Items[J].NFSe.procNFSe.cStat := AInfProt.Items[I].cStat;
        FNotasFiscais.Items[J].NFSe.procNFSe.xMotivo := AInfProt.Items[I].xMotivo;

        if FPConfiguracoesNFSe.Arquivos.Salvar or NaoEstaVazio(
          FNotasFiscais.Items[J].NomeArq) then
        begin
          if FileExists(PathWithDelim(FPConfiguracoesNFSe.Arquivos.PathSalvar) +
                        AInfProt.Items[I].chNFSe + '-NFSe.xml') and
             FileExists(PathWithDelim(FPConfiguracoesNFSe.Arquivos.PathSalvar) +
                        FNFSeRetorno.nRec + '-pro-rec.xml') then
          begin
            AProcNFSe := TProcNFSe.Create;
            try
              AProcNFSe.PathNFSe :=
                PathWithDelim(FPConfiguracoesNFSe.Arquivos.PathSalvar) +
                AInfProt.Items[I].chNFSe + '-NFSe.xml';
              AProcNFSe.PathRetConsReciNFSe :=
                PathWithDelim(FPConfiguracoesNFSe.Arquivos.PathSalvar) +
                FNFSeRetorno.nRec + '-pro-rec.xml';

              AProcNFSe.Versao := FPVersaoServico;
              AProcNFSe.GerarXML;

              if NaoEstaVazio(AProcNFSe.Gerador.ArquivoFormatoXML) then
              begin
                if NaoEstaVazio(FNotasFiscais.Items[J].NomeArq) then
                  AProcNFSe.Gerador.SalvarArquivo(FNotasFiscais.Items[J].NomeArq)
                else
                  AProcNFSe.Gerador.SalvarArquivo(
                    PathWithDelim(FPConfiguracoesNFSe.Arquivos.PathSalvar) +
                    AInfProt.Items[I].chNFSe + '-NFSe.xml');
              end;
            finally
              AProcNFSe.Free;
            end;
          end;
        end;

        if FPConfiguracoesNFSe.Arquivos.Salvar then
        begin
          if FPConfiguracoesNFSe.Arquivos.SalvarApenasNFSeProcessadas then
          begin
            if FNotasFiscais.Items[J].Processada then
              FNotasFiscais.Items[J].GravarXML;
          end
          else
            FNotasFiscais.Items[J].GravarXML;
        end;

        break;
      end;
    end;
  end;

  //Verificando se existe alguma nota confirmada
  for I := 0 to FNotasFiscais.Count - 1 do
  begin
    if FNotasFiscais.Items[I].Confirmada then
    begin
      Result := True;
      break;
    end;
  end;

  //Verificando se existe alguma nota nao confirmada
  for I := 0 to FNotasFiscais.Count - 1 do
  begin
    if not FNotasFiscais.Items[I].Confirmada then
    begin
      FPMsg := ACBrStr('Nota(s) não confirmadas:') + LineBreak;
      break;
    end;
  end;

  //Montando a mensagem de retorno para as notas nao confirmadas
  for I := 0 to FNotasFiscais.Count - 1 do
  begin
    if not FNotasFiscais.Items[I].Confirmada then
      FPMsg := FPMsg + IntToStr(FNotasFiscais.Items[I].NFSe.Ide.nNF) +
        '->' + FNotasFiscais.Items[I].Msg + LineBreak;
  end;

  if AInfProt.Count > 0 then
  begin
    FChaveNFSe := AInfProt.Items[0].chNFSe;
    FProtocolo := AInfProt.Items[0].nProt;
    FcStat := AInfProt.Items[0].cStat;
  end;
end;

function TNFSeRetRecepcao.Executar: Boolean;
var
  IntervaloTentativas, Tentativas: Integer;
begin
  Result := False;

  TACBrNFSe(FPDFeOwner).SetStatus(stNFSeRetRecepcao);
  try
    Sleep(FPConfiguracoesNFSe.WebServices.AguardarConsultaRet);

    Tentativas := 0;
    IntervaloTentativas := max(FPConfiguracoesNFSe.WebServices.IntervaloTentativas, 1000);

    while (inherited Executar) and
      (Tentativas < FPConfiguracoesNFSe.WebServices.Tentativas) do
    begin
      Inc(Tentativas);
      sleep(IntervaloTentativas);
    end;
  finally
    TACBrNFSe(FPDFeOwner).SetStatus(stIdle);
  end;

  if FNFSeRetorno.CStat = 104 then  // Lote processado ?
    Result := TratarRespostaFinal;
end;

procedure TNFSeRetRecepcao.DefinirURL;
begin
  if TACBrNFSe(FPDFeOwner).EhAutorizacao then
    FPLayout := LayNFSeRetAutorizacao
  else
    FPLayout := LayNFSeRetRecepcao;

  inherited DefinirURL;
end;

procedure TNFSeRetRecepcao.DefinirServicoEAction;
begin
  if FPLayout = LayNFSeRetAutorizacao then
    FPServico := GetUrlWsd + 'NFSeRetAutorizacao'
  else
    FPServico := GetUrlWsd + 'NFSeRetRecepcao2';

  FPSoapAction := FPServico;
end;

procedure TNFSeRetRecepcao.DefinirDadosMsg;
var
  ConsReciNFSe: TConsReciNFSe;
begin
  ConsReciNFSe := TConsReciNFSe.Create;
  try
    ConsReciNFSe.tpAmb := FPConfiguracoesNFSe.WebServices.Ambiente;
    ConsReciNFSe.nRec := FRecibo;
    ConsReciNFSe.Versao := FPVersaoServico;
    ConsReciNFSe.GerarXML;

    FPDadosMsg := ConsReciNFSe.Gerador.ArquivoFormatoXML;
  finally
    ConsReciNFSe.Free;
  end;
end;

function TNFSeRetRecepcao.TratarResposta: Boolean;
begin
  if FPLayout = LayNFSeRetAutorizacao then
  begin
    FPRetWS := SeparaDados(FPRetornoWS, 'NFSeRetAutorizacaoResult');
    if FPRetWS = '' then
      FPRetWS := SeparaDados(FPRetornoWS, 'NFSeRetAutorizacaoLoteResult');
  end
  else
    FPRetWS := SeparaDados(FPRetornoWS, 'NFSeRetRecepcao2Result');

  // Limpando variaveis internas
  FNFSeRetorno.Free;
  FNFSeRetorno := TRetConsReciNFSe.Create;

  FNFSeRetorno.Leitor.Arquivo := FPRetWS;
  FNFSeRetorno.LerXML;

  Fversao := FNFSeRetorno.versao;
  FTpAmb := FNFSeRetorno.TpAmb;
  FverAplic := FNFSeRetorno.verAplic;
  FcStat := FNFSeRetorno.cStat;
  FcUF := FNFSeRetorno.cUF;
  FPMsg := FNFSeRetorno.xMotivo;
  FxMotivo := FNFSeRetorno.xMotivo;
  FcMsg := FNFSeRetorno.cMsg;
  FxMsg := FNFSeRetorno.xMsg;

  Result := (FNFSeRetorno.CStat = 105); // Lote em Processamento
end;

procedure TNFSeRetRecepcao.FinalizarServico;
begin
  // Sobrescrito, para não liberar para stIdle... não ainda...;
end;

function TNFSeRetRecepcao.GerarMsgLog: String;
begin
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Recibo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + LineBreak +
                           'cMsg: %s ' + LineBreak +
                           'xMsg: %s ' + LineBreak),
                   [FNFSeRetorno.versao, TpAmbToStr(FNFSeRetorno.tpAmb),
                    FNFSeRetorno.verAplic, FNFSeRetorno.nRec,
                    IntToStr(FNFSeRetorno.cStat), FNFSeRetorno.xMotivo,
                    CodigoParaUF(FNFSeRetorno.cUF), IntToStr(FNFSeRetorno.cMsg),
                    FNFSeRetorno.xMsg]);
end;

function TNFSeRetRecepcao.GerarPrefixoArquivo: String;
begin
  Result := Recibo;
end;

{ TNFSeRecibo }

constructor TNFSeRecibo.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FNFSeRetorno := TRetConsReciNFSe.Create;

  FPStatus := stNFSeRecibo;
  FPLayout := LayNFSeRetRecepcao;
  FPArqEnv := 'ped-rec';
  FPArqResp := 'pro-rec';
end;

destructor TNFSeRecibo.Destroy;
begin
  FNFSeRetorno.Free;

  inherited Destroy;
end;

procedure TNFSeRecibo.DefinirServicoEAction;
begin
  if FPLayout = LayNFSeRetAutorizacao then
    FPServico := GetUrlWsd + 'NFSeRetAutorizacao'
  else
    FPServico := GetUrlWsd + 'NFSeRetRecepcao2';

  FPSoapAction := FPServico;
end;

procedure TNFSeRecibo.DefinirURL;
begin
  if TACBrNFSe(FPDFeOwner).EhAutorizacao then
    FPLayout := LayNFSeRetAutorizacao
  else
    FPLayout := LayNFSeRetRecepcao;

  inherited DefinirURL;
end;

procedure TNFSeRecibo.DefinirDadosMsg;
var
  ConsReciNFSe: TConsReciNFSe;
begin
  ConsReciNFSe := TConsReciNFSe.Create;
  try
    ConsReciNFSe.tpAmb := FPConfiguracoesNFSe.WebServices.Ambiente;
    ConsReciNFSe.nRec := FRecibo;
    ConsReciNFSe.Versao := FPVersaoServico;
    ConsReciNFSe.GerarXML;

    FPDadosMsg := ConsReciNFSe.Gerador.ArquivoFormatoXML;
  finally
    ConsReciNFSe.Free;
  end;
end;

function TNFSeRecibo.TratarResposta: Boolean;
begin
  if FPLayout = LayNFSeRetAutorizacao then
  begin
    FPRetWS := SeparaDados(FPRetornoWS, 'NFSeRetAutorizacaoResult');
    if FPRetWS = '' then
      FPRetWS := SeparaDados(FPRetornoWS, 'NFSeRetAutorizacaoLoteResult');
  end
  else
    FPRetWS := SeparaDados(FPRetornoWS, 'NFSeRetRecepcao2Result');

  // Limpando variaveis internas
  FNFSeRetorno.Free;
  FNFSeRetorno := TRetConsReciNFSe.Create;

  FNFSeRetorno.Leitor.Arquivo := FPRetWS;
  FNFSeRetorno.LerXML;

  Fversao := FNFSeRetorno.versao;
  FTpAmb := FNFSeRetorno.TpAmb;
  FverAplic := FNFSeRetorno.verAplic;
  FcStat := FNFSeRetorno.cStat;
  FxMotivo := FNFSeRetorno.xMotivo;
  FcUF := FNFSeRetorno.cUF;
  FxMsg := FNFSeRetorno.xMsg;
  FcMsg := FNFSeRetorno.cMsg;
  FPMsg := FxMotivo;

  Result := (FNFSeRetorno.CStat = 104);
end;

function TNFSeRecibo.GerarMsgLog: String;
begin
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Recibo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + LineBreak),
                   [FNFSeRetorno.versao, TpAmbToStr(FNFSeRetorno.TpAmb),
                   FNFSeRetorno.verAplic, FNFSeRetorno.nRec,
                   IntToStr(FNFSeRetorno.cStat),
                   FNFSeRetorno.ProtNFSe.Items[0].xMotivo,
                   CodigoParaUF(FNFSeRetorno.cUF)]);
end;

{ TNFSeConsulta }

constructor TNFSeConsulta.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FprotNFSe := TProcNFSe.Create;
  FretCancNFSe := TRetCancNFSe.Create;
  FprocEventoNFSe := TRetEventoNFSeCollection.Create(AOwner);

  FPStatus := stNFSeConsulta;
  FPLayout := LayNFSeConsulta;
  FPArqEnv := 'ped-sit';
  FPArqResp := 'sit';
end;

destructor TNFSeConsulta.Destroy;
begin
  FprotNFSe.Free;
  FretCancNFSe.Free;
  if Assigned(FprocEventoNFSe) then
    FprocEventoNFSe.Free;

  inherited Destroy;
end;

procedure TNFSeConsulta.DefinirServicoEAction;
begin
  if (FPConfiguracoesNFSe.Geral.ModeloDF = moNFSe) and
     (FPConfiguracoesNFSe.Geral.VersaoDF = ve310) and
     (FPConfiguracoesNFSe.WebServices.UFCodigo in [29, 41]) then // 29 = BA, 41 = PR
    FPServico := GetUrlWsd + 'NFSeConsulta'
  else
    FPServico := GetUrlWsd + 'NFSeConsulta2';

  FPSoapAction := FPServico;
end;

procedure TNFSeConsulta.DefinirDadosMsg;
var
  ConsSitNFSe: TConsSitNFSe;
  OK: Boolean;
begin
  OK := False;
  ConsSitNFSe := TConsSitNFSe.Create;
  try
    ConsSitNFSe.TpAmb := FPConfiguracoesNFSe.WebServices.Ambiente;
    ConsSitNFSe.chNFSe := FNFSeChave;

    FPConfiguracoesNFSe.Geral.ModeloDF :=
      StrToModeloDF(OK, ExtrairModeloChaveAcesso(ConsSitNFSe.chNFSe));

    ConsSitNFSe.Versao := FPVersaoServico;
    ConsSitNFSe.GerarXML;

    FPDadosMsg := ConsSitNFSe.Gerador.ArquivoFormatoXML;
  finally
    ConsSitNFSe.Free;
  end;
end;

function TNFSeConsulta.TratarResposta: Boolean;
var
  NFSeRetorno: TRetConsSitNFSe;
  NFCancelada, Atualiza: Boolean;
  aEventos, aMsg, NomeArquivo: String;
  AProcNFSe: TProcNFSe;
  I, J: Integer;
begin
  NFSeRetorno := TRetConsSitNFSe.Create;

  try
    FPRetWS := SeparaDados(FPRetornoWS, 'NFSeConsultaNF2Result');
    if FPRetWS = '' then
      FPRetWS := SeparaDados(FPRetornoWS, 'NFSeConsultaNFResult');

    NFSeRetorno.Leitor.Arquivo := FPRetWS;
    NFSeRetorno.LerXML;

    NFCancelada := False;
    aEventos := '';

    // <retConsSitNFSe> - Retorno da consulta da situação da NF-e
    // Este é o status oficial da NF-e
    Fversao := NFSeRetorno.versao;
    FTpAmb := NFSeRetorno.tpAmb;
    FverAplic := NFSeRetorno.verAplic;
    FcStat := NFSeRetorno.cStat;
    FXMotivo := NFSeRetorno.xMotivo;
    FcUF := NFSeRetorno.cUF;
    FNFSeChave := NFSeRetorno.chNFSe;
    FPMsg := FXMotivo;



    // Verifica se a nota fiscal está cancelada pelo método antigo. Se estiver,
    // então NFCancelada será True e já atribui Protocolo, Data e Mensagem
    if NFSeRetorno.retCancNFSe.cStat > 0 then
    begin
      FRetCancNFSe.versao := NFSeRetorno.retCancNFSe.versao;
      FretCancNFSe.tpAmb := NFSeRetorno.retCancNFSe.tpAmb;
      FretCancNFSe.verAplic := NFSeRetorno.retCancNFSe.verAplic;
      FretCancNFSe.cStat := NFSeRetorno.retCancNFSe.cStat;
      FretCancNFSe.xMotivo := NFSeRetorno.retCancNFSe.xMotivo;
      FretCancNFSe.cUF := NFSeRetorno.retCancNFSe.cUF;
      FretCancNFSe.chNFSe := NFSeRetorno.retCancNFSe.chNFSe;
      FretCancNFSe.dhRecbto := NFSeRetorno.retCancNFSe.dhRecbto;
      FretCancNFSe.nProt := NFSeRetorno.retCancNFSe.nProt;

      NFCancelada := True;
      FProtocolo := NFSeRetorno.retCancNFSe.nProt;
      FDhRecbto := NFSeRetorno.retCancNFSe.dhRecbto;
      FPMsg := NFSeRetorno.xMotivo;
    end;

    // <protNFSe> - Retorno dos dados do ENVIO da NF-e
    // Considerá-los apenas se não existir nenhum evento de cancelamento (110111)
    FprotNFSe.PathNFSe := NFSeRetorno.protNFSe.PathNFSe;
    FprotNFSe.PathRetConsReciNFSe := NFSeRetorno.protNFSe.PathRetConsReciNFSe;
    FprotNFSe.PathRetConsSitNFSe := NFSeRetorno.protNFSe.PathRetConsSitNFSe;
    FprotNFSe.PathRetConsSitNFSe := NFSeRetorno.protNFSe.PathRetConsSitNFSe;
    FprotNFSe.tpAmb := NFSeRetorno.protNFSe.tpAmb;
    FprotNFSe.verAplic := NFSeRetorno.protNFSe.verAplic;
    FprotNFSe.chNFSe := NFSeRetorno.protNFSe.chNFSe;
    FprotNFSe.dhRecbto := NFSeRetorno.protNFSe.dhRecbto;
    FprotNFSe.nProt := NFSeRetorno.protNFSe.nProt;
    FprotNFSe.digVal := NFSeRetorno.protNFSe.digVal;
    FprotNFSe.cStat := NFSeRetorno.protNFSe.cStat;
    FprotNFSe.xMotivo := NFSeRetorno.protNFSe.xMotivo;

    if Assigned(NFSeRetorno.procEventoNFSe) and (NFSeRetorno.procEventoNFSe.Count > 0) then
    begin
      aEventos := '=====================================================' +
        LineBreak + '================== Eventos da NF-e ==================' +
        LineBreak + '=====================================================' +
        LineBreak + '' + LineBreak + 'Quantidade total de eventos: ' +
        IntToStr(NFSeRetorno.procEventoNFSe.Count);

      FprocEventoNFSe.Clear;
      for I := 0 to NFSeRetorno.procEventoNFSe.Count - 1 do
      begin
        with FprocEventoNFSe.Add.RetEventoNFSe do
        begin
          idLote := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.idLote;
          tpAmb := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.tpAmb;
          verAplic := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.verAplic;
          cOrgao := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.cOrgao;
          cStat := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.cStat;
          xMotivo := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.xMotivo;
          XML := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.XML;

          INFSevento.ID := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.ID;
          INFSevento.tpAmb := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.tpAmb;
          INFSevento.CNPJ := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.CNPJ;
          INFSevento.chNFSe := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.chNFSe;
          INFSevento.dhEvento := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.dhEvento;
          INFSevento.TpEvento := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.TpEvento;
          INFSevento.nSeqEvento := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.nSeqEvento;
          INFSevento.VersaoEvento := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.VersaoEvento;
          INFSevento.DetEvento.xCorrecao := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.DetEvento.xCorrecao;
          INFSevento.DetEvento.xCondUso := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.DetEvento.xCondUso;
          INFSevento.DetEvento.nProt := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.DetEvento.nProt;
          INFSevento.DetEvento.xJust := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.DetEvento.xJust;

          retEvento.Clear;
          for J := 0 to NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Count-1 do
          begin
            with retEvento.Add.RetINFSevento do
            begin
              Id := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.Id;
              tpAmb := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.tpAmb;
              verAplic := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.verAplic;
              cOrgao := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.cOrgao;
              cStat := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.cStat;
              xMotivo := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.xMotivo;
              chNFSe := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.chNFSe;
              tpEvento := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.tpEvento;
              xEvento := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.xEvento;
              nSeqEvento := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.nSeqEvento;
              CNPJDest := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.CNPJDest;
              emailDest := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.emailDest;
              dhRegEvento := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.dhRegEvento;
              nProt := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.nProt;
              XML := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.XML;
            end;
          end;
        end;

        with NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe do
        begin
          aEventos := aEventos + LineBreak + LineBreak +
            Format(ACBrStr('Número de sequência: %s ' + LineBreak +
                           'Código do evento: %s ' + LineBreak +
                           'Descrição do evento: %s ' + LineBreak +
                           'Status do evento: %s ' + LineBreak +
                           'Descrição do status: %s ' + LineBreak +
                           'Protocolo: %s ' + LineBreak +
                           'Data/Hora do registro: %s '),
                   [IntToStr(INFSevento.nSeqEvento),
                    TpEventoToStr(INFSevento.TpEvento),
                    INFSevento.DescEvento,
                    IntToStr(retEvento.Items[J].RetINFSevento.cStat),
                    retEvento.Items[J].RetINFSevento.xMotivo,
                    retEvento.Items[J].RetINFSevento.nProt,
                    FormatDateTimeBr(retEvento.Items[J].RetINFSevento.dhRegEvento)]);

          if retEvento.Items[J].RetINFSevento.tpEvento = teCancelamento then
          begin
            NFCancelada := True;
            FProtocolo := retEvento.Items[J].RetINFSevento.nProt;
            FDhRecbto := retEvento.Items[J].RetINFSevento.dhRegEvento;
            FPMsg := retEvento.Items[J].RetINFSevento.xMotivo;
          end;
        end;
      end;
    end;

    if not NFCancelada and (NaoEstaVazio(NFSeRetorno.protNFSe.nProt))  then
    begin
      FProtocolo := NFSeRetorno.protNFSe.nProt;
      FDhRecbto := NFSeRetorno.protNFSe.dhRecbto;
      FPMsg := NFSeRetorno.protNFSe.xMotivo;
    end;

    //TODO: Verificar porque monta "aMsg", pois ela não está sendo usada em lugar nenhum
    aMsg := GerarMsgLog;
    if aEventos <> '' then
      aMsg := aMsg + sLineBreak + aEventos;

    Result := (NFSeRetorno.CStat in [100, 101, 110, 150, 151, 155]);

    NomeArquivo := PathWithDelim(FPConfiguracoesNFSe.Arquivos.PathSalvar) + FNFSeChave;

    for i := 0 to TACBrNFSe(FPDFeOwner).NotasFiscais.Count - 1 do
    begin
      with TACBrNFSe(FPDFeOwner).NotasFiscais.Items[i] do
      begin
        if (OnlyNumber(FNFSeChave) = NumID) then
        begin
          Atualiza := True;
          if (NFSeRetorno.CStat in [101, 151, 155]) then
            Atualiza := False;

          // Atualiza o Status da NFSe interna //
          NFSe.procNFSe.cStat := NFSeRetorno.cStat;

          if Atualiza then
          begin
            if (FPConfiguracoesNFSe.Geral.ValidarDigest) and
              (NFSeRetorno.protNFSe.digVal <> '') and
              (NFSe.signature.DigestValue <> NFSeRetorno.protNFSe.digVal) then
            begin
              raise EACBrNFSeException.Create('DigestValue do documento ' +
                NumID + ' não coNFSere.');
            end;

            NFSe.procNFSe.tpAmb := NFSeRetorno.tpAmb;
            NFSe.procNFSe.verAplic := NFSeRetorno.verAplic;
            NFSe.procNFSe.chNFSe := NFSeRetorno.chNFSe;
            NFSe.procNFSe.dhRecbto := FDhRecbto;
            NFSe.procNFSe.nProt := FProtocolo;
            NFSe.procNFSe.digVal := NFSeRetorno.protNFSe.digVal;
            NFSe.procNFSe.cStat := NFSeRetorno.cStat;
            NFSe.procNFSe.xMotivo := NFSeRetorno.xMotivo;

            if FileExists(NomeArquivo + '-NFSe.xml') or NaoEstaVazio(NomeArq) then
            begin
              AProcNFSe := TProcNFSe.Create;
              try
                if NaoEstaVazio(NomeArq) then
                  AProcNFSe.PathNFSe := NomeArq
                else
                  AProcNFSe.PathNFSe := NomeArquivo + '-NFSe.xml';

                AProcNFSe.PathRetConsSitNFSe := NomeArquivo + '-sit.xml';

                if FPConfiguracoesNFSe.Geral.VersaoDF >= ve310 then
                  AProcNFSe.Versao :=
                    TACBrNFSe(FPDFeOwner).LerVersaoDeParams(LayNFSeAutorizacao)
                else
                  AProcNFSe.Versao :=
                    TACBrNFSe(FPDFeOwner).LerVersaoDeParams(LayNFSeEnviarLoteRPS);

                AProcNFSe.GerarXML;

                if NaoEstaVazio(AProcNFSe.Gerador.ArquivoFormatoXML) then
                  AProcNFSe.Gerador.SalvarArquivo(AProcNFSe.PathNFSe);
              finally
                AProcNFSe.Free;
              end;
            end;

            if FPConfiguracoesNFSe.Arquivos.Salvar then
            begin
              if FPConfiguracoesNFSe.Arquivos.SalvarApenasNFSeProcessadas then
              begin
                if Processada then
                  GravarXML();
              end
              else
                GravarXML();
            end;
          end;

          break;
        end;
      end;
    end;

    if (TACBrNFSe(FPDFeOwner).NotasFiscais.Count <= 0) then
    begin
      if FPConfiguracoesNFSe.Arquivos.Salvar then
      begin
        if FileExists(NomeArquivo + '-NFSe.xml') then
        begin
          AProcNFSe := TProcNFSe.Create;
          try
            AProcNFSe.PathNFSe := NomeArquivo + '-NFSe.xml';
            AProcNFSe.PathRetConsSitNFSe := NomeArquivo + '-sit.xml';

            if FPConfiguracoesNFSe.Geral.VersaoDF >= ve310 then
              AProcNFSe.Versao :=
                TACBrNFSe(FPDFeOwner).LerVersaoDeParams(LayNFSeAutorizacao)
            else
              AProcNFSe.Versao := TACBrNFSe(FPDFeOwner).LerVersaoDeParams(LayNFSeEnviarLoteRPS);

            AProcNFSe.GerarXML;

            if NaoEstaVazio(AProcNFSe.Gerador.ArquivoFormatoXML) then
              AProcNFSe.Gerador.SalvarArquivo(AProcNFSe.PathNFSe);
          finally
            AProcNFSe.Free;
          end;
        end;
      end;
    end;
  finally
    NFSeRetorno.Free;
  end;
end;

function TNFSeConsulta.GerarMsgLog: String;
begin
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Identificador: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + LineBreak +
                           'Chave Acesso: %s ' + LineBreak +
                           'Recebimento: %s ' + LineBreak +
                           'Protocolo: %s ' + LineBreak +
                           'Digest Value: %s ' + LineBreak),
                   [Fversao, FNFSeChave, TpAmbToStr(FTpAmb), FverAplic,
                    IntToStr(FcStat), FXMotivo, CodigoParaUF(FcUF), FNFSeChave,
                    FormatDateTimeBr(FDhRecbto), FProtocolo, FprotNFSe.digVal]);
end;

function TNFSeConsulta.GerarPrefixoArquivo: String;
begin
  Result := Trim(FNFSeChave);
end;
*)

{ TNFSeEnvioWebService }

constructor TNFSeEnvioWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPStatus := stNFSeEnvioWebService;
  FVersao := '';
end;

destructor TNFSeEnvioWebService.Destroy;
begin
  inherited Destroy;
end;

function TNFSeEnvioWebService.Executar: Boolean;
begin
  Result := inherited Executar;
end;

procedure TNFSeEnvioWebService.DefinirURL;
begin
  FPURL := FPURLEnvio;
end;

procedure TNFSeEnvioWebService.DefinirServicoEAction;
begin
  FPServico := FPSoapAction;
end;

procedure TNFSeEnvioWebService.DefinirDadosMsg;
var
  LeitorXML: TLeitor;
begin
  LeitorXML := TLeitor.Create;
  try
    LeitorXML.Arquivo := FXMLEnvio;
    LeitorXML.Grupo := FXMLEnvio;
    FVersao := LeitorXML.rAtributo('versao')
  finally
    LeitorXML.Free;
  end;

  FPDadosMsg := FXMLEnvio;
end;

function TNFSeEnvioWebService.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'soap:Body');
  Result := True;
end;

function TNFSeEnvioWebService.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService: '+FPServico + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

function TNFSeEnvioWebService.GerarVersaoDadosSoap: String;
begin
  Result := '<versaoDados>' + FVersao + '</versaoDados>';
end;

{ TWebServices }

constructor TWebServices.Create(AOwner: TACBrDFe);
begin
  FACBrNFSe := TACBrNFSe(AOwner);

  FGerarLoteRPS := TNFSeGerarLoteRPS.Create(FACBrNFSe, TACBrNFSe(FACBrNFSe).NotasFiscais);
  FEnviarLoteRPS := TNFSeEnviarLoteRPS.Create(FACBrNFSe, TACBrNFSe(FACBrNFSe).NotasFiscais);
(*
  FRetorno := TNFSeRetRecepcao.Create(FACBrNFSe, TACBrNFSe(FACBrNFSe).NotasFiscais);
  FRecibo := TNFSeRecibo.Create(FACBrNFSe);
  FConsulta := TNFSeConsulta.Create(FACBrNFSe);
*)
  FEnvioWebService := TNFSeEnvioWebService.Create(FACBrNFSe);
end;

destructor TWebServices.Destroy;
begin
  FGerarLoteRPS.Free;
  FEnviarLoteRPS.Free;
(*
  FRetorno.Free;
  FRecibo.Free;
  FConsulta.Free;
*)
  FEnvioWebService.Free;

  inherited Destroy;
end;

function TWebServices.GeraLote(ALote: Integer): Boolean;
begin
  Result := GeraLote(IntToStr(ALote));
end;

function TWebServices.GeraLote(ALote: String): Boolean;
begin
  FGerarLoteRPS.FNumeroLote := ALote;

  Result := GerarLoteRPS.Executar;

  if not (Result) then
    GerarLoteRPS.GerarException( GerarLoteRPS.Msg );
end;

function TWebServices.Envia(ALote: Integer): Boolean;
begin
  Result := Envia(IntToStr(ALote));
end;

function TWebServices.Envia(ALote: String): Boolean;
begin
  FEnviarLoteRPS.FNumeroLote := ALote;

  Result := EnviarLoteRPS.Executar;

  if not (Result) then
    EnviarLoteRPS.GerarException( EnviarLoteRPS.Msg );

  (*

  if not ASincrono then
  begin
    FRetorno.Recibo := FEnviarLoteRPS.Recibo;
    if not FRetorno.Executar then
      FRetorno.GerarException( FRetorno.Msg );
  end;

************************************************************

 Self.ConsSitLote.Cnpj               := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.Prestador.Cnpj;
 Self.ConsSitLote.InscricaoMunicipal := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal;
 Self.ConsSitLote.Protocolo          := Self.Enviar.Protocolo;
 Self.ConsSitLote.NumeroLote         := Self.Enviar.NumeroLote;

 if (TACBrNFSe( FACBrNFSe ).Configuracoes.WebServices.Provedor in [proISSDigital] ) then
 begin
   Self.ConsSitLote.Senha        := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.Prestador.Senha;
   Self.ConsSitLote.FraseSecreta := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.Prestador.FraseSecreta;
 end;

 Self.ConsLote.Protocolo := Self.Enviar.Protocolo;

 if (TACBrNFSe( FACBrNFSe ).Configuracoes.WebServices.Provedor in [proISSDigital, proTecnos] ) then
 begin
   Self.ConsLote.Senha        := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.Prestador.Senha;
   Self.ConsLote.FraseSecreta := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.Prestador.FraseSecreta;
 end;

 if (TACBrNFSe( FACBrNFSe ).Configuracoes.WebServices.ConsultaLoteAposEnvio) and (Result) then
 begin
   if not (TACBrNFSe( FACBrNFSe ).Configuracoes.WebServices.Provedor in [proDigifred, proProdata,
          proVitoria, proPVH, profintelISS, proSaatri, proSisPMJP, proCoplan, proISSDigital,
          proISSDSF, proFiorilli, proFreire, proTecnos, proDBSeller]) then //adicionei o provedor DBSeller - Rodrigo Custódio
    begin
     Result := Self.ConsSitLote.Executar;

     if not (Result)
      then begin
       if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
        then TACBrNFSe( FACBrNFSe ).OnGerarLog(Self.ConsSitLote.Msg);
       if Self.ConsSitLote.Msg <> ''
        then raise Exception.Create(Self.ConsSitLote.Msg)
        else raise Exception.Create('Erro Desconhecido ao Consultar a Situação do Lote!')
      end;
    end;

   if TACBrNFSe( FACBrNFSe ).Configuracoes.WebServices.Provedor = proInfisc then
     Result := True // Para Provedor Infisc já retorna a NFS-e ao Consultar Situação Lote
   else
     Result := Self.ConsLote.Executar;

   if not (Result)
    then begin
     if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
      then TACBrNFSe( FACBrNFSe ).OnGerarLog(Self.ConsLote.Msg);
     if Self.ConsLote.Msg <> ''
      then raise Exception.Create(Self.ConsLote.Msg)
      else raise Exception.Create('Erro Desconhecido ao Consultar o Lote!')
    end;
 end;
*)
end;

end.
