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
  ACBrDFe, ACBrDFeUtil, ACBrDFeConfiguracoes,
  ACBrDFeSSL, ACBrNFSeXConversao, ACBrNFSeXConfiguracoes,
  ACBrXmlBase, ACBrXmlDocument;

resourcestring
  ERR_NAO_IMP = 'Serviço não implementado para este provedor.';

type

  TACBrNFSeXWebservice = class
  private
    FPrefixo: string;

    function GetBaseUrl: string;

  protected
    HttpClient: TDFeSSLHttpClass;

    FPFaultNode: string;
    FPFaultCodeNode: string;
    FPFaultMsgNode: string;

    FPConfiguracoes: TConfiguracoes;
    FPDFeOwner: TACBrDFe;
    FPURL: String;
    FPMimeType: String;
    FPEnvio: string;
    FPRetorno: string;
    FUseOuterXml: Boolean;

    FPArqEnv: String;
    FPArqResp: String;
    FPMsgOrig: string;

    procedure FazerLog(const Msg: String; Exibir: Boolean = False); virtual;
    procedure GerarException(const Msg: String; E: Exception = nil); virtual;
    function GetSoapBody(const Response: string): string; virtual;

    function GerarPrefixoArquivo: String; virtual;
    procedure SalvarEnvio(ADadosSoap, ADadosMsg: string); virtual;
    procedure SalvarRetorno(ADadosSoap, ADadosMsg: string); virtual;

    function PrepararEnvio(const Message, SoapAction, SoapHeader: string;
                                  namespace: array of string): string; virtual; abstract;

    function TratarRetorno(const ARetorno: string; responseTag: array of string): string; virtual;
    procedure ChecarRetorno(const ADocument: TACBrXmlDocument); virtual;

    function Executar(SoapAction, Message, responseTag : string): string; overload;
    function Executar(SoapAction, Message, responseTag, namespace : string): string; overload;
    function Executar(SoapAction, Message, responseTag : string;
                                  namespace: array of string): string; overload;
    function Executar(SoapAction, Message: string;
                      responseTag, namespace: array of string): string; overload;
    function Executar(SoapAction, Message, SoapHeader: string;
                      responseTag, namespace: array of string): string; overload;

  public
    constructor Create(AOwner: TACBrDFe; AMetodo: TMetodo; AURl: string);

    function Recepcionar(ACabecalho, AMSG: String): string; virtual;
    function ConsultarLote(ACabecalho, AMSG: String): string; virtual;
    function ConsultarSituacao(ACabecalho, AMSG: String): string; virtual;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; virtual;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; virtual;
    function ConsultarNFSeUrl(ACabecalho, AMSG: String): string; virtual;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; virtual;
    function ConsultarNFSeServicoPrestado(ACabecalho, AMSG: String): string; virtual;
    function ConsultarNFSeServicoTomado(ACabecalho, AMSG: String): string; virtual;
    function Cancelar(ACabecalho, AMSG: String): string; virtual;
    function GerarNFSe(ACabecalho, AMSG: String): string; virtual;
    function RecepcionarSincrono(ACabecalho, AMSG: String): string; virtual;
    function SubstituirNFSe(ACabecalho, AMSG: String): string; virtual;
    function AbrirSessao(ACabecalho, AMSG: String): string; virtual;
    function FecharSessao(ACabecalho, AMSG: String): string; virtual;
    function TesteEnvio(ACabecalho, AMSG: String): string; virtual;

    property URL: String read FPURL;
    property BaseURL: String read GetBaseUrl;
    property MimeType: String read FPMimeType;
    property Envio: string read FPEnvio;
    property Retorno: string read FPRetorno;
    property Prefixo: string read FPrefixo write FPrefixo;

  end;

  TACBrNFSeXWebserviceSoap11 = class(TACBrNFSeXWebservice)
  protected
    function PrepararEnvio(const Message, SoapAction, SoapHeader: string;
                                  namespace: array of string): string; override;
  public
    constructor Create(AOwner: TACBrDFe; AMetodo: TMetodo; AURl: string);

  end;

  TACBrNFSeXWebserviceSoap12 = class(TACBrNFSeXWebservice)
  protected
    function PrepararEnvio(const Message, SoapAction, SoapHeader: string;
                                  namespace: array of string): string; override;
  public
    constructor Create(AOwner: TACBrDFe; AMetodo: TMetodo; AURl: string);

  end;

  TACBrNFSeXWebserviceNoSoap = class(TACBrNFSeXWebservice)
  protected
    function GetSoapBody(const Response: string): string; override;

    function PrepararEnvio(const Message, SoapAction, SoapHeader: string;
                                  namespace: array of string): string; override;
  public
    constructor Create(AOwner: TACBrDFe; AMetodo: TMetodo; AURl: string);

  end;

  TACBrNFSeXWebserviceRest = class(TACBrNFSeXWebserviceNoSoap)
  protected
    function PrepararEnvio(const Message, SoapAction, SoapHeader: string;
                           namespace: array of string): string; override;

  public
    constructor Create(AOwner: TACBrDFe; AMetodo: TMetodo; AURl: string);

  end;

  TACBrNFSeXWebserviceMulti = class(TACBrNFSeXWebserviceNoSoap)
  protected
    FPBound: string;

    function GerarPrefixoArquivo: String; override;
    function PrepararEnvio(const Message, SoapAction, SoapHeader: string;
                                  namespace: array of string): string; override;
  public
    constructor Create(AOwner: TACBrDFe; AMetodo: TMetodo; AURl: string);

  end;

  TInfConsultaNFSe = class
 private
   FNumeroIniNFSe: string;
   FNumeroFinNFSe: string;
   FNumeroLote: string;
   FDataInicial: TDateTime;
   FDataFinal: TDateTime;

   FCNPJPrestador: String;
   FIMPrestador: String;
   FCNPJTomador: String;
   FIMTomador: String;
   FCNPJInter: String;
   FIMInter: String;
   FRazaoInter: String;

   FPagina: Integer;
   FtpConsulta: TtpConsulta;
   FtpPeriodo: TtpPeriodo;
   FTipo: String;
   FCodTribMun: String;

 public
   constructor Create;

   function LerFromIni(const AIniString: String): Boolean;

   property NumeroIniNFSe: string   read FNumeroIniNFSe write FNumeroIniNFSe;
   property NumeroFinNFSe: string   read FNumeroFinNFSe write FNumeroFinNFSe;
   property NumeroLote: string      read FNumeroLote    write FNumeroLote;
   property DataInicial: TDateTime  read FDataInicial   write FDataInicial;
   property DataFinal: TDateTime    read FDataFinal     write FDataFinal;
   property CNPJPrestador: String   read FCNPJPrestador write FCNPJPrestador;
   property IMPrestador: String     read FIMPrestador   write FIMPrestador;
   property CNPJTomador: String     read FCNPJTomador   write FCNPJTomador;
   property IMTomador: String       read FIMTomador     write FIMTomador;
   property CNPJInter: String       read FCNPJInter     write FCNPJInter;
   property IMInter: String         read FIMInter       write FIMInter;
   property RazaoInter: String      read FRazaoInter    write FRazaoInter;
   property Pagina: Integer         read FPagina        write FPagina;
   property tpConsulta: TtpConsulta read FtpConsulta    write FtpConsulta;
   property tpPeriodo: TtpPeriodo   read FtpPeriodo     write FtpPeriodo;
   property Tipo: String            read FTipo          write FTipo;
   property CodTribMun: String      read FCodTribMun    write FCodTribMun;
 end;

  TInfCancelamento = class
  private
    FNumeroNFSe: string;
    FSerieNFSe: string;
    FChaveNFSe: string;
    FCodCancelamento: string;
    FMotCancelamento: string;
    FNumeroLote: string;
    FNumeroRps: Integer;
    FSerieRps: string;
    FValorNFSe: Double;
    FCodVerificacao: string;

  public
    constructor Create;

    function LerFromIni(const AIniString: String): Boolean;

    property NumeroNFSe: string      read FNumeroNFSe      write FNumeroNFSe;
    property SerieNFSe: string       read FSerieNFSe       write FSerieNFSe;
    property ChaveNFSe: string       read FChaveNFSe       write FChaveNFSe;
    property CodCancelamento: string read FCodCancelamento write FCodCancelamento;
    property MotCancelamento: string read FMotCancelamento write FMotCancelamento;
    property NumeroLote: string      read FNumeroLote      write FNumeroLote;
    property NumeroRps: Integer      read FNumeroRps       write FNumeroRps;
    property SerieRps: string        read FSerieRps        write FSerieRps;
    property ValorNFSe: Double       read FValorNFSe       write FValorNFSe;
    property CodVerificacao: string  read FCodVerificacao  write FCodVerificacao;

  end;

implementation

uses
  IniFiles, StrUtils, synautil, ACBrConsts, ACBrUtil, ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXParametros;

{ TACBrNFSeXWebservice }

constructor TACBrNFSeXWebservice.Create(AOwner: TACBrDFe; AMetodo: TMetodo; AURl: string);
begin
  FPDFeOwner := AOwner;
  if Assigned(AOwner) then
    FPConfiguracoes := AOwner.Configuracoes;

  FUseOuterXml := True;

  FPFaultNode := 'Fault';
  FPFaultCodeNode := 'faultcode';
  FPFaultMsgNode := 'faultstring';

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

    tmConsultarNFSeURL:
      begin
        FPArqEnv := 'con-url-nfse';
        FPArqResp := 'lista-nfse-url';
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
  end;

  FPURL := AURl;
end;

procedure TACBrNFSeXWebservice.FazerLog(const Msg: String; Exibir: Boolean);
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

procedure TACBrNFSeXWebservice.GerarException(const Msg: String; E: Exception);
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

function TACBrNFSeXWebservice.GerarPrefixoArquivo: String;
begin
  if FPrefixo = '' then
    Result := FormatDateTime('yyyymmddhhnnss', Now)
  else
    Result := TiraPontos(FPrefixo);
end;

procedure TACBrNFSeXWebservice.SalvarEnvio(ADadosSoap, ADadosMsg: string);
var
  Prefixo, ArqEnv: String;
  IsUTF8: Boolean;
begin
  { Sobrescrever apenas se necessário }

  if FPArqEnv = '' then Exit;

  Prefixo := GerarPrefixoArquivo;

  if FPConfiguracoes.Geral.Salvar then
  begin
    ArqEnv := Prefixo + '-' + FPArqEnv + '.xml';

    IsUTF8  := XmlEstaAssinado(ADadosMsg);
    FPDFeOwner.Gravar(ArqEnv, ADadosMsg, '', IsUTF8);
  end;

  if FPConfiguracoes.WebServices.Salvar then
  begin
    ArqEnv := Prefixo + '-' + FPArqEnv + '-soap.xml';

    IsUTF8  := XmlEstaAssinado(ADadosSoap);
    FPDFeOwner.Gravar(ArqEnv, ADadosSoap, '', IsUTF8);
  end;
end;

procedure TACBrNFSeXWebservice.SalvarRetorno(ADadosSoap, ADadosMsg: string);
var
  Prefixo, ArqEnv: String;
  IsUTF8: Boolean;
begin
  { Sobrescrever apenas se necessário }

  if FPArqResp = '' then Exit;

  Prefixo := GerarPrefixoArquivo;

  if FPDFeOwner.Configuracoes.Geral.Salvar then
  begin
    ArqEnv := Prefixo + '-' + FPArqResp + '.xml';

    IsUTF8  := XmlEstaAssinado(ADadosMsg);
    FPDFeOwner.Gravar(ArqEnv, ADadosMsg, '', IsUTF8);
  end;

  if FPDFeOwner.Configuracoes.WebServices.Salvar then
  begin
    ArqEnv := Prefixo + '-' + FPArqResp + '-soap.xml';

    IsUTF8  := XmlEstaAssinado(ADadosSoap);
    FPDFeOwner.Gravar(ArqEnv, ADadosSoap, '', IsUTF8);
  end;
end;

function TACBrNFSeXWebservice.GetSoapBody(const Response: string): string;
var
  aXml:string;
begin
  aXml := SeparaDados(Response, 'Body');

  if aXml = '' then
    aXml := '<a>' +
                '<ListaMensagemRetorno>' +
                 '<MensagemRetorno>' +
                  '<Codigo>' + '</Codigo>' +
                  '<Mensagem>' + Response + '</Mensagem>' +
                  '<Correcao>' + '</Correcao>' +
                 '</MensagemRetorno>' +
                '</ListaMensagemRetorno>' +
               '</a>';

  Result :=  aXml;
end;

procedure TACBrNFSeXWebservice.ChecarRetorno(const ADocument: TACBrXmlDocument);
Var
  ANode: TACBrXmlNode;
  aMsg: string;
begin
  if ADocument.Root.Name = FPFaultNode then
    ANode := ADocument.Root
  else
    ANode := ADocument.Root.Childrens.FindAnyNs(FPFaultNode);

  if (ANode <> nil) then
  begin
    aMsg := ProcessarConteudoXml(ANode.Childrens.FindAnyNs(FPFaultCodeNode), tcStr) +
       ' - ' +
       ProcessarConteudoXml(ANode.Childrens.FindAnyNs(FPFaultMsgNode), tcStr);

    if aMsg = ' - ' then
      aMsg := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('Code'), tcStr) +
       ' - ' +
       ProcessarConteudoXml(ANode.Childrens.FindAnyNs('Reason'), tcStr) +
       ' - ' +
       ProcessarConteudoXml(ANode.Childrens.FindAnyNs('Detail'), tcStr);

    raise EACBrDFeException.Create(aMsg);
  end;
end;

function TACBrNFSeXWebservice.TratarRetorno(const ARetorno: string; responseTag: array of string): string;
var
  Document: TACBrXmlDocument;
  ANode: TACBrXmlNode;
  I: Integer;
  xRetorno: string;
begin
  xRetorno := StrToXml(ARetorno);
  xRetorno := RemoverCDATA(xRetorno);
  xRetorno := RemoverDeclaracaoXML(xRetorno);
  xRetorno := ConverteXMLtoUTF8(xRetorno);
  xRetorno := RemoverDeclaracaoXML(xRetorno);

  if xRetorno = '' then
  begin
    Result := xRetorno;
    Exit;
  end;

  if (High(responseTag) = 0) and (responseTag[0] = '') then
  begin
    Result := xRetorno;
    Exit;
  end;

  Document := TACBrXmlDocument.Create;
  Document.LoadFromXml(xRetorno);

  try
    ChecarRetorno(Document);
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

function TACBrNFSeXWebservice.Executar(SoapAction, Message, responseTag : string): string;
begin
  Result := Executar(SoapAction, Message, '', [], []);
end;

function TACBrNFSeXWebservice.Executar(SoapAction, Message, responseTag, namespace : string): string;
begin
  Result := Executar(SoapAction, Message, '', [responseTag], [namespace]);
end;

function TACBrNFSeXWebservice.Executar(SoapAction, Message, responseTag : string;
  namespace: array of string): string;
begin
  Result := Executar(SoapAction, Message, '', [responseTag], namespace);
end;

function TACBrNFSeXWebservice.Executar(SoapAction, Message: string;
  responseTag, namespace: array of string): string;
begin
  Result := Executar(SoapAction, Message, '', responseTag, namespace);
end;

function TACBrNFSeXWebservice.Executar(SoapAction, Message, SoapHeader: string;
  responseTag, namespace: array of string): string;
var
  Tentar, Tratado, TemCertificadoConfigurado: Boolean;
  HTTPResultCode, InternalErrorCode: Integer;
  aRetorno: TACBrXmlDocument;
begin
  FPEnvio := PrepararEnvio(Message, SoapAction, SoapHeader, namespace);

  SalvarEnvio(FPEnvio, FPMsgOrig);

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

  Tentar := True;
  while Tentar do
  begin
    Tentar  := False;
    Tratado := False;
    Result := '';
    FPRetorno := '';
    HTTPResultCode := 0;
    InternalErrorCode := 0;

    try
      if Assigned(FPDFeOwner.OnTransmit) then  // Envio por Evento... Aplicação cuidará do envio
      begin
        FPDFeOwner.OnTransmit( FPEnvio, FPURL, SoapAction,
                               FPMimeType, FPRetorno, HTTPResultCode, InternalErrorCode);

        if (InternalErrorCode <> 0) then
          raise EACBrDFeException.Create('Erro ao Transmitir');
      end
      else   // Envio interno, por TDFeSSL
      begin
        try
          HttpClient.URL := FPURL;
          HttpClient.Method := 'POST';
          HttpClient.MimeType := MimeType;

          if TACBrNFSeX(FPDFeOwner).Provider.ConfigGeral.UseAuthorizationHeader then
            HttpClient.HeaderReq.AddHeader('Authorization', TConfiguracoesNFSe(FPConfiguracoes).Geral.Emitente.WSChaveAutoriz);

          WriteStrToStream(HttpClient.DataReq, AnsiString(FPEnvio));

          try
            HttpClient.Execute;

            HttpClient.DataResp.Position := 0;
            FPRetorno := string(ReadStrFromStream(HttpClient.DataResp, HttpClient.DataResp.Size));

            // Verifica se o ResultCode é: 200 OK; 201 Created; 202 Accepted
            // https://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
            if not (HttpClient.HTTPResultCode in [200..202]) then
            begin
              aRetorno := TACBrXmlDocument.Create;

              if FPRetorno <> '' then
              begin
                aRetorno.LoadFromXml(FPRetorno);

                try
                  ChecarRetorno(aRetorno);
                finally
                  aRetorno.Free;
                end;
              end
              else
                raise EACBrDFeException.Create('WebService retornou um XML vazio.');
            end;
          except
            on E:Exception do
            begin
              raise EACBrDFeException.CreateDef(Format(ACBrStr(cACBrDFeSSLEnviarException),
                                             [HttpClient.InternalErrorCode, HttpClient.HTTPResultCode, HttpClient.URL] )
                                             + sLineBreak + HttpClient.LastErrorDesc + sLineBreak + Result);
            end;
          end;
        finally
          HTTPResultCode := HttpClient.HTTPResultCode;
          InternalErrorCode := HttpClient.InternalErrorCode;
        end;
      end;

      Result := TratarRetorno(GetSoapBody(FPRetorno), responseTag);
      SalvarRetorno(FPRetorno, Result);
    except
      if Assigned(FPDFeOwner.OnTransmitError) then
        FPDFeOwner.OnTransmitError(HTTPResultCode, InternalErrorCode,
                                   FPURL, FPEnvio, SoapAction,
                                   Tentar, Tratado);

      if not (Tentar or Tratado) then raise;
    end;
  end;
end;

function TACBrNFSeXWebservice.Recepcionar(ACabecalho, AMSG: String): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.ConsultarLote(ACabecalho, AMSG: String): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.ConsultarSituacao(ACabecalho, AMSG: String): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.ConsultarNFSePorRps(ACabecalho, AMSG: String): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.ConsultarNFSe(ACabecalho, AMSG: String): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.ConsultarNFSeUrl(ACabecalho, AMSG: String): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.ConsultarNFSeServicoPrestado(ACabecalho, AMSG: String): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.ConsultarNFSeServicoTomado(ACabecalho, AMSG: String): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.Cancelar(ACabecalho, AMSG: String): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.GerarNFSe(ACabecalho, AMSG: String): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.RecepcionarSincrono(ACabecalho, AMSG: String): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.SubstituirNFSe(ACabecalho, AMSG: String): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.AbrirSessao(ACabecalho, AMSG: String): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.FecharSessao(ACabecalho, AMSG: String): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

function TACBrNFSeXWebservice.TesteEnvio(ACabecalho, AMSG: String): string;
begin
  Result := '';
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

{ TACBrNFSeXWebserviceSoap11 }

constructor TACBrNFSeXWebserviceSoap11.Create(AOwner: TACBrDFe; AMetodo: TMetodo;
  AURl: string);
begin
  inherited Create(AOwner, AMetodo, AURl);

  FPMimeType := 'text/xml; charset=utf-8';
end;

function TACBrNFSeXWebserviceSoap11.PrepararEnvio(const Message, SoapAction,
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

  HttpClient := FPDFeOwner.SSL.SSLHttpClass;

  HttpClient.Clear;
  HttpClient.HeaderReq.AddHeader('SOAPAction', SoapAction);
end;

{ TACBrNFSeXWebserviceSoap12 }

constructor TACBrNFSeXWebserviceSoap12.Create(AOwner: TACBrDFe; AMetodo: TMetodo;
  AURl: string);
begin
  inherited Create(AOwner, AMetodo, AURl);

  FPMimeType := 'application/soap+xml; charset=utf-8';
end;

function TACBrNFSeXWebserviceSoap12.PrepararEnvio(const Message, SoapAction,
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

  HttpClient := FPDFeOwner.SSL.SSLHttpClass;

  HttpClient.Clear;
end;

{ TACBrNFSeXWebserviceNoSoap }

constructor TACBrNFSeXWebserviceNoSoap.Create(AOwner: TACBrDFe;
  AMetodo: TMetodo; AURl: string);
begin
  inherited Create(AOwner, AMetodo, AURl);

  FPMimeType := 'application/xml; charset=utf-8';
end;

function TACBrNFSeXWebserviceNoSoap.GetSoapBody(const Response: string): string;
begin
  Result := Response;
end;

function TACBrNFSeXWebserviceNoSoap.PrepararEnvio(const Message, SoapAction,
  SoapHeader: string; namespace: array of string): string;
begin
  Result := Message;

  Result := string(NativeStringToUTF8(Result));

  HttpClient := FPDFeOwner.SSL.SSLHttpClass;

  HttpClient.Clear;
  HttpClient.HeaderReq.AddHeader('SOAPAction', SoapAction);
end;

{ TACBrNFSeXWebserviceRest }

constructor TACBrNFSeXWebserviceRest.Create(AOwner: TACBrDFe; AMetodo: TMetodo;
  AURl: string);
begin
  inherited Create(AOwner, AMetodo, AURl);

  FPMimeType := 'application/json';
end;

function TACBrNFSeXWebserviceRest.PrepararEnvio(const Message, SoapAction,
  SoapHeader: string; namespace: array of string): string;
var
  UsuarioWeb, SenhaWeb, Texto: String;
begin
  UsuarioWeb := Trim(TConfiguracoesNFSe(FPConfiguracoes).Geral.Emitente.WSUser);

  if UsuarioWeb = '' then
    GerarException(ACBrStr('O provedor ' + TConfiguracoesNFSe(FPConfiguracoes).Geral.xProvedor +
      ' necessita que a propriedade: Configuracoes.Geral.Emitente.WSUser seja informada.'));

  SenhaWeb := Trim(TConfiguracoesNFSe(FPConfiguracoes).Geral.Emitente.WSSenha);

  if SenhaWeb = '' then
    GerarException(ACBrStr('O provedor ' + TConfiguracoesNFSe(FPConfiguracoes).Geral.xProvedor +
      ' necessita que a propriedade: Configuracoes.Geral.Emitente.WSSenha seja informada.'));

  Texto := StringReplace(Message, '"', '''', [rfReplaceAll]);
  Texto := StringReplace(Texto, #10, '', [rfReplaceAll]);
  Texto := StringReplace(Texto, #13, '', [rfReplaceAll]);

  Result := Format('{"xml": "%s", "usuario": "%s", "senha": "%s"}', [Texto, UsuarioWeb, SenhaWeb]);

  HttpClient := FPDFeOwner.SSL.SSLHttpClass;

  HttpClient.Clear;
end;

{ TACBrNFSeXWebserviceMulti }

constructor TACBrNFSeXWebserviceMulti.Create(AOwner: TACBrDFe; AMetodo: TMetodo;
  AURl: string);
begin
  inherited Create(AOwner, AMetodo, AURl);

  FPBound := IntToHex(Random(MaxInt), 8) + '_Synapse_boundary';
  FPMimeType := 'multipart/form-data; boundary=' + AnsiQuotedStr(FPBound, '"');
end;

function TACBrNFSeXWebserviceMulti.GerarPrefixoArquivo: String;
begin
  Result := IntToHex(Random(MaxInt), 8);
end;

function TACBrNFSeXWebserviceMulti.PrepararEnvio(const Message, SoapAction,
  SoapHeader: string; namespace: array of string): string;
var
  UsuarioWeb, SenhaWeb: String;
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
            AnsiQuotedStr(GerarPrefixoArquivo + '-lot-rps.xml', '"') + sLineBreak +
            'Content-Type: text/xml' + sLineBreak + sLineBreak + Message + sLineBreak +
            '--' + FPBound + '--' + sLineBreak;

  HttpClient := FPDFeOwner.SSL.SSLHttpClass;

  HttpClient.Clear;
end;

{ TInfConsulta }

constructor TInfConsultaNFSe.Create;
begin
  tpConsulta    := tcPorNumero;
  tpPeriodo     := tpEmissao;
  NumeroIniNFSe := '';
  NumeroFinNFSe := '';
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
  CodTribMun    := '';
  Pagina        := 1;
  Tipo          := '';
end;

function TInfConsultaNFSe.LerFromIni(const AIniString: String): Boolean;
var
  sSecao: String;
  INIRec: TMemIniFile;
  Ok: Boolean;
begin
{$IFNDEF COMPILER23_UP}
  Result := False;
{$ENDIF}

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    sSecao := 'ConsultarNFSe';

    tpConsulta := StrTotpConsulta(Ok, INIRec.ReadString(sSecao, 'tpConsulta', '1'));
    tpPeriodo  := StrTotpPeriodo(Ok, INIRec.ReadString(sSecao, 'tpPeriodo', '1'));

    NumeroIniNFSe := INIRec.ReadString(sSecao, 'NumeroIniNFSe', '');
    NumeroFinNFSe := INIRec.ReadString(sSecao, 'NumeroFinNFSe', '');
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
    Tipo          := INIRec.ReadString(sSecao, 'Tipo', '');
    CodTribMun    := INIRec.ReadString(sSecao, 'CodTribMun', '');

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
  FCodCancelamento := '';
  FMotCancelamento := '';
  FNumeroLote := '';
  FNumeroRps := 0;
  FSerieRps := '';
  FValorNFSe := 0.0;
  FCodVerificacao := '';
end;

function TInfCancelamento.LerFromIni(const AIniString: String): Boolean;
var
  sSecao: String;
  INIRec: TMemIniFile;
begin
{$IFNDEF COMPILER23_UP}
  Result := False;
{$ENDIF}

  INIRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(AIniString, INIRec);

    sSecao := 'CancelarNFSe';

    NumeroNFSe      := INIRec.ReadString(sSecao, 'NumeroNFSe', '');
    SerieNFSe       := INIRec.ReadString(sSecao, 'SerieNFSe', '');
    ChaveNFSe       := INIRec.ReadString(sSecao, 'ChaveNFSe', '');
    CodCancelamento := INIRec.ReadString(sSecao, 'CodCancelamento', '');
    MotCancelamento := INIRec.ReadString(sSecao, 'MotCancelamento', '');
    NumeroLote      := INIRec.ReadString(sSecao, 'NumeroLote', '');
    NumeroRps       := INIRec.ReadInteger(sSecao, 'NumeroRps', 0);
    SerieRps        := INIRec.ReadString(sSecao, 'SerieRps', '');
    ValorNFSe       := StringToFloatDef(INIRec.ReadString(sSecao, 'ValorNFSe', ''), 0);
    CodVerificacao  := INIRec.ReadString(sSecao, 'CodVerificacao', '');

    Result := True;
  finally
    INIRec.Free;
  end;
end;

end.

