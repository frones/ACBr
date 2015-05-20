{******************************************************************************}
{ Projeto: Componente ACBrGNRE                                                 }
{  Biblioteca multiplataforma de componentes Delphi/Lazarus para emissão da    }
{  Guia Nacional de Recolhimento de Tributos Estaduais                         }
{  http://www.gnre.pe.gov.br/                                                  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 Claudemir Vitor Pereira                }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                       Juliomar Marchetti                     }
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

{******************************************************************************
|* Historico
|*
|* 09/12/2013 - Claudemir Vitor Pereira
|*  - Doação do componente para o Projeto ACBr
******************************************************************************}
{$I ACBr.inc}

unit ACBrGNREWebServices;

interface

uses
  {$IFDEF FPC}
    LResources, Controls, Graphics,
  {$ELSE}
    StrUtils,
  {$ENDIF}
    Classes, SysUtils,
  {$IF DEFINED(VisualCLX)}
     QDialog,
  {$ELSEIF DEFINED(FMX)}
     FMX.Dialogs,
  {$ELSE}
     Dialogs,
  {$IFEND}
  {$IFDEF ACBrGNREOpenSSL}
    HTTPSend,
  {$ELSE}
     {$IFDEF SoapHTTP}
     SoapHTTPClient, SOAPHTTPTrans, SOAPConst, JwaWinCrypt, WinInet, ACBrCAPICOM_TLB,
     {$ELSE}
        ACBrHTTPReqResp,
     {$ENDIF}
  {$ENDIF}
    pcnCabecalho,pcnGerador, pcnConversao,pcnAuxiliar,pgnreGNRE, pgnreConsConfigUF,
    pgnreConsResLoteGNRE, pgnreConversao, ACBrGNREGuias, ACBrGNREConfiguracoes,
    pgnreRetEnvLoteGNRE, pgnreRetConsResLoteGNRE, pgnreRetConsConfigUF;

type

  TWebServicesBase = Class
  private
    procedure DoGNRERecepcaoLote;
    procedure DoGNRERetRecepcaoLote;
    procedure DoGNREConsultarResultadoLote;
    procedure DoGNREConsultarConfigUF;

    {$IFDEF ACBrGNREOpenSSL}
      procedure ConfiguraHTTP( HTTP : THTTPSend; Action : AnsiString);
    {$ELSE}
     {$IFDEF SoapHTTP}
      procedure ConfiguraReqResp( ReqResp : THTTPReqResp);
      procedure OnBeforePost(const HTTPReqResp: THTTPReqResp; Data:Pointer);
     {$ELSE}
      procedure ConfiguraReqResp( ReqResp : TACBrHTTPReqResp);
     {$ENDIF}
    {$ENDIF}
  protected
    FCabMsg: WideString;
    FDadosMsg: AnsiString;
    FDadosSenha: AnsiString;
    FRetornoWS: AnsiString;
    FRetWS: AnsiString;
    FMsg: AnsiString;
    FURL: WideString;
    FConfiguracoes: TConfiguracoes;
    FACBrGNRE : TComponent;
    procedure LoadMsgEntrada;
    procedure LoadURL;
    function Confirma(AResultado: string): Boolean;
    procedure GerarException(Msg: AnsiString);    
  public
    function Executar: Boolean; virtual;
    constructor Create(AOwner : TComponent); virtual;
    property CabMsg: WideString read FCabMsg;
    property DadosMsg: AnsiString read FDadosMsg;
    property DadosSenha: AnsiString read FDadosSenha;
    property RetornoWS: AnsiString read FRetornoWS;
    property RetWS: AnsiString read FRetWS;
    property Msg: AnsiString read FMsg;
  end;

  TGNRERecepcaoLote = Class(TWebServicesBase)
  private
    Fnumero: string;
    FtempoEstimadoProc: Integer;
    Fcodigo: Integer;
    Fdescricao: string;
    FdataHoraRecibo: TDateTime;
    Fambiente: TpcnTipoAmbiente;
    FGuias: TGuias;
    FGNRERetorno: TTretLote_GNRE;
  public
    function Executar: Boolean; override;
    constructor Create(AOwner : TComponent; AGuias : TGuias); reintroduce;
    property ambiente: TpcnTipoAmbiente read Fambiente write Fambiente;
    property codigo: Integer read Fcodigo write Fcodigo;
    property descricao: string read Fdescricao write Fdescricao;
    property numero: string read Fnumero write Fnumero;
    property dataHoraRecibo: TDateTime read FdataHoraRecibo write FdataHoraRecibo;
    property tempoEstimadoProc: Integer read FtempoEstimadoProc write FtempoEstimadoProc;
    property GNRERetorno: TTretLote_GNRE read FGNRERetorno write FGNRERetorno;
  end;

  TGNRERetRecepcaoLote = Class(TWebServicesBase)
  private
    Fambiente: TpcnTipoAmbiente;
    FnumeroRecibo: string;
    Fcodigo: Integer;
    Fresultado: string;
    Fdescricao: string;
    Fprotocolo: string;
    FGuias: TGuias;
    FGNRERetorno: TTResultLote_GNRE;
  public
    function Executar: Boolean; override;
    constructor Create(AOwner : TComponent; AGuias : TGuias); reintroduce;
    destructor Destroy; override;    
    property ambiente: TpcnTipoAmbiente read Fambiente write Fambiente;
    property numeroRecibo: string read FnumeroRecibo write FnumeroRecibo;
    property codigo: Integer read Fcodigo write Fcodigo;
    property descricao: string read Fdescricao write Fdescricao;
    property protocolo: string read Fprotocolo write Fprotocolo;
    property resultado: string read Fresultado write Fresultado;
    property GNRERetorno: TTResultLote_GNRE read FGNRERetorno write FGNRERetorno;
  end;

  TGNREConsultarResultadoLote = Class(TWebServicesBase)
  private
    Fambiente: TpcnTipoAmbiente;  
    FnumeroRecibo: string;
    Fcodigo: Integer;
    Fresultado: string;
    Fdescricao: string;
    Fprotocolo: string;    
    FGuias: TGuias;
    FGNRERetorno: TTResultLote_GNRE;    
  public
    function Executar: Boolean; override;
    constructor Create(AOwner : TComponent; AGuias : TGuias); reintroduce;
    property ambiente: TpcnTipoAmbiente read Fambiente write Fambiente;
    property numeroRecibo: string read FnumeroRecibo write FnumeroRecibo;
    property codigo: Integer read Fcodigo write Fcodigo;
    property descricao: string read Fdescricao write Fdescricao;
    property protocolo: string read Fprotocolo write Fprotocolo;
    property resultado: string read Fresultado write Fresultado;
    property GNRERetorno: TTResultLote_GNRE read FGNRERetorno write FGNRERetorno;
  end;

  TGNREConsultarConfigUF = Class(TWebServicesBase)
  private
    Fcodigo: Integer;
    Fdescricao: string;
    Freceita: Integer;
    FexigeReceita: string;        
    FexigeDataVencimento: string;
    FexigeDataPagamento: string;
    FexigeContribuinteEmitente: string;
    FexigeUfFavorecida: string;
    FexigeConvenio: string;
    FUf: string;
    Fambiente: TpcnTipoAmbiente;
    FGNRERetorno: TTConfigUf;
  public
    function Executar: Boolean; override;
    constructor Create(AOwner : TComponent); reintroduce;
    destructor Destroy; override;
    property ambiente: TpcnTipoAmbiente read Fambiente write Fambiente;
    property Uf: string read FUf write FUf;
    property codigo: Integer read Fcodigo write Fcodigo;
    property descricao: string read Fdescricao write Fdescricao;
    property receita: Integer read Freceita write Freceita;
    property exigeReceita: string read FexigeReceita write FexigeReceita;
    property exigeUfFavorecida: string read FexigeUfFavorecida write FexigeUfFavorecida;
    property exigeContribuinteEmitente: string read FexigeContribuinteEmitente write FexigeContribuinteEmitente;
    property exigeDataVencimento: string read FexigeDataVencimento write FexigeDataVencimento;
    property exigeConvenio: string read FexigeConvenio write FexigeConvenio;
    property exigeDataPagamento: string read FexigeDataPagamento write FexigeDataPagamento;
    property GNRERetorno: TTConfigUf read FGNRERetorno write FGNRERetorno;
  end;

  TWebServices = Class(TWebServicesBase)
  private
    FACBrGNRE: TComponent;
    FEnviar: TGNRERecepcaoLote;
    FRetorno: TGNRERetRecepcaoLote;
    FConsResLote: TGNREConsultarResultadoLote;
    FConsConfigUF: TGNREConsultarConfigUF;
  public
    constructor Create(AFGNRE: TComponent); reintroduce;
    destructor Destroy; override;
    function Envia: Boolean;
    function ConsultaResultadoLote(ANumRecibo: String): Boolean;
  published
    property ACBrGNRE: TComponent read FACBrGNRE write FACBrGNRE;
    property Enviar: TGNRERecepcaoLote read FEnviar write FEnviar;
    property Retorno: TGNRERetRecepcaoLote read FRetorno write FRetorno;
    property ConsResLote: TGNREConsultarResultadoLote read FConsResLote write FConsResLote;
    property ConsConfigUF: TGNREConsultarConfigUF read FConsConfigUF write FConsConfigUF;
  end;

implementation

uses
 {$IFDEF ACBrGNREOpenSSL}
   ssl_openssl,
 {$ENDIF}
 Math, ACBrUtil, ACBrGNRE2, ACBrDFeUtil, ACBrGNREUtil, pgnreGNRERetorno;

{$IFNDEF ACBrGNREOpenSSL}
const
  INTERNET_OPTION_CLIENT_CERT_CONTEXT = 84;
{$ENDIF}

{ TWebServicesBase }
constructor TWebServicesBase.Create(AOwner: TComponent);
begin
 FConfiguracoes := TConfiguracoes( TACBrGNRE( AOwner ).Configuracoes );
 FACBrGNRE      := TACBrGNRE( AOwner );
end;

procedure TWebServicesBase.GerarException(Msg: AnsiString);
begin
  //FazerLog( 'ERRO: ' + Msg, False );
  raise Exception.Create( Msg );
end;

{$IFDEF ACBrGNREOpenSSL}
 procedure TWebServicesBase.ConfiguraHTTP( HTTP : THTTPSend; Action : AnsiString);
 begin
   if FileExists(FConfiguracoes.Certificados.Certificado) then
     HTTP.Sock.SSL.PFXfile := FConfiguracoes.Certificados.Certificado
   else
     HTTP.Sock.SSL.PFX     := FConfiguracoes.Certificados.Certificado;

   HTTP.Sock.SSL.KeyPassword := FConfiguracoes.Certificados.Senha;

   HTTP.ProxyHost := FConfiguracoes.WebServices.ProxyHost;
   HTTP.ProxyPort := FConfiguracoes.WebServices.ProxyPort;
   HTTP.ProxyUser := FConfiguracoes.WebServices.ProxyUser;
   HTTP.ProxyPass := FConfiguracoes.WebServices.ProxyPass;

   if (pos('SCERECEPCAORFB', UpperCase(FURL)) <= 0) and
      (pos('SCECONSULTARFB', UpperCase(FURL)) <= 0) then
      HTTP.MimeType := 'application/soap+xml; charset=utf-8'
   else
      HTTP.MimeType := 'text/xml; charset=utf-8';

   HTTP.UserAgent := '';
   HTTP.Protocol := '1.1';
   HTTP.AddPortNumberToHost := False;
   HTTP.Headers.Add(Action);
 end;
{$ELSE}
 {$IFDEF SoapHTTP}
  procedure TWebServicesBase.ConfiguraReqResp( ReqResp : THTTPReqResp );
  begin
    if FConfiguracoes.WebServices.ProxyHost <> '' then
     begin
       ReqResp.Proxy    := FConfiguracoes.WebServices.ProxyHost + ':' +
                           FConfiguracoes.WebServices.ProxyPort;
       ReqResp.UserName := FConfiguracoes.WebServices.ProxyUser;
       ReqResp.Password := FConfiguracoes.WebServices.ProxyPass;
     end;
    ReqResp.OnBeforePost := OnBeforePost;
  end;

  procedure TWebServicesBase.OnBeforePost(const HTTPReqResp: THTTPReqResp;
    Data: Pointer);
  var
    Cert: ICertificate2;
    CertContext: ICertContext;
    PCertContext: Pointer;
    ContentHeader: string;
  begin
    Cert := FConfiguracoes.Certificados.GetCertificado;
    CertContext :=  Cert as ICertContext;
    CertContext.Get_CertContext(Integer(PCertContext));

    if not InternetSetOption(Data, INTERNET_OPTION_CLIENT_CERT_CONTEXT,
                             PCertContext, SizeOf(CERT_CONTEXT)) then
      GerarException('OnBeforePost: ' + IntToStr(GetLastError));

    if trim(FConfiguracoes.WebServices.ProxyUser) <> '' then
      if not InternetSetOption(Data, INTERNET_OPTION_PROXY_USERNAME,
                               PChar(FConfiguracoes.WebServices.ProxyUser),
                               Length(FConfiguracoes.WebServices.ProxyUser)) then
        GerarException('OnBeforePost: ' + IntToStr(GetLastError));

    if trim(FConfiguracoes.WebServices.ProxyPass) <> '' then
      if not InternetSetOption(Data, INTERNET_OPTION_PROXY_PASSWORD,
                               PChar(FConfiguracoes.WebServices.ProxyPass),
                               Length(FConfiguracoes.WebServices.ProxyPass)) then
        GerarException('OnBeforePost: ' + IntToStr(GetLastError));

    if (pos('SCERECEPCAORFB', UpperCase(FURL)) <= 0) and
       (pos('SCECONSULTARFB', UpperCase(FURL)) <= 0) then
    begin
       ContentHeader := Format(ContentTypeTemplate, ['application/soap+xml; charset=utf-8']);
       HttpAddRequestHeaders(Data, PChar(ContentHeader),
                             Length(ContentHeader), HTTP_ADDREQ_FLAG_REPLACE);
    end;

    HTTPReqResp.CheckContentType;
  end;
 {$ELSE}
  procedure TWebServicesBase.ConfiguraReqResp( ReqResp : TACBrHTTPReqResp);
  begin
    if FConfiguracoes.WebServices.ProxyHost <> '' then
    begin
      ReqResp.ProxyHost := FConfiguracoes.WebServices.ProxyHost;
      ReqResp.ProxyPort := FConfiguracoes.WebServices.ProxyPort;
      ReqResp.ProxyUser := FConfiguracoes.WebServices.ProxyUser;
      ReqResp.ProxyPass := FConfiguracoes.WebServices.ProxyPass;
    end;

    ReqResp.SetCertificate(FConfiguracoes.Certificados.GetCertificado);

    if (pos('SCERECEPCAORFB', UpperCase(FURL)) <= 0) and
       (pos('SCECONSULTARFB', UpperCase(FURL)) <= 0) then
      ReqResp.MimeType := 'application/soap+xml'
    else
      ReqResp.MimeType := 'text/xml';
  end;
 {$ENDIF}
{$ENDIF}

function TWebServicesBase.Executar: Boolean;
begin
  Result := False;
  LoadMsgEntrada;
  LoadURL;
end;

procedure TWebServicesBase.LoadMsgEntrada;
begin
  if Self is TGNRERecepcaoLote
  then DoGNRERecepcaoLote
  else if Self is TGNRERetRecepcaoLote
  then DoGNRERetRecepcaoLote
  else if Self is TGNREConsultarResultadoLote
  then DoGNREConsultarResultadoLote
  else if Self is TGNREConsultarConfigUF
  then DoGNREConsultarConfigUF;
end;

procedure TWebServicesBase.LoadURL;
begin
  if Self is TGNRERecepcaoLote
  then FURL := GNREUtil.GetURL(FConfiguracoes.WebServices.AmbienteCodigo, LayGNRERecepcao)
  else if (Self is TGNREConsultarResultadoLote) or (Self is TGNRERetRecepcaoLote)
  then FURL := GNREUtil.GetURL(FConfiguracoes.WebServices.AmbienteCodigo, LayGNRERetRecepcao)
  else if Self is TGNREConsultarConfigUF
  then FURL := GNREUtil.GetURL(FConfiguracoes.WebServices.AmbienteCodigo, LayGNREConsultaConfigUF);
end;

procedure TWebServicesBase.DoGNRERecepcaoLote;
var i : Integer;
  vGuias : WideString;
begin
  vGuias := '';
  for i := 0 to TGNRERecepcaoLote(Self).FGuias.Count - 1 do
    vGuias := vGuias + TGNRERecepcaoLote(Self).FGuias.Items[i].XML;

  FDadosMsg := '<TLote_GNRE xmlns="http://www.gnre.pe.gov.br">'+
                '<guias>'+ vGuias + '</guias>' +'</TLote_GNRE>';

  if Length(FDadosMsg) > (300 * 1024) then
  begin
    if Assigned(TACBrGNRE(Self.FACBrGNRE).OnGerarLog) then
      TACBrGNRE(Self.FACBrGNRE).OnGerarLog('ERRO: Tamanho do XML de Dados superior a 300 Kbytes. Tamanho atual: '+FloatToStr(Int(Length(FDadosMsg)/300))+' Kbytes');
    raise Exception.Create('ERRO: Tamanho do XML de Dados superior a 300 Kbytes. Tamanho atual: '+FloatToStr(Int(Length(FDadosMsg)/300))+' Kbytes');
    Exit;
  end;
end;

procedure TWebServicesBase.DoGNREConsultarResultadoLote;
var ConsResLoteGNRE : TConsResLoteGNRE;
begin
  ConsResLoteGNRE               := TConsResLoteGNRE.Create;
  ConsResLoteGNRE.Ambiente      := TpcnTipoAmbiente(FConfiguracoes.WebServices.AmbienteCodigo-1);
  ConsResLoteGNRE.numeroRecibo  := TGNREConsultarResultadoLote(Self).FnumeroRecibo;

  ConsResLoteGNRE.GerarXML;

  FDadosMsg := ConsResLoteGNRE.Gerador.ArquivoFormatoXML;
  ConsResLoteGNRE.Free;
end;

procedure TWebServicesBase.DoGNREConsultarConfigUF;
var ConsConfigUF: TConsConfigUF;
begin
  ConsConfigUF           := TConsConfigUF.Create;
  ConsConfigUF.UF        := FConfiguracoes.WebServices.UF;
  ConsConfigUF.Ambiente  := TpcnTipoAmbiente(FConfiguracoes.WebServices.AmbienteCodigo-1);
  ConsConfigUF.Receita   := TGNREConsultarConfigUF(Self).receita;

  ConsConfigUF.GerarXML;

  FDadosMsg := ConsConfigUF.Gerador.ArquivoFormatoXML;

  ConsConfigUF.Free;
end;

procedure TWebServicesBase.DoGNRERetRecepcaoLote;
var ConsResLoteGNRE : TConsResLoteGNRE;
begin
  ConsResLoteGNRE               := TConsResLoteGNRE.Create;
	try
		ConsResLoteGNRE.ambiente      := TpcnTipoAmbiente(FConfiguracoes.WebServices.AmbienteCodigo-1);
		ConsResLoteGNRE.numeroRecibo  := TGNRERetRecepcaoLote(Self).FnumeroRecibo;

		ConsResLoteGNRE.GerarXML;

		FDadosMsg := ConsResLoteGNRE.Gerador.ArquivoFormatoXML;
	finally
		ConsResLoteGNRE.Free;
	end;
end;

function TWebServicesBase.Confirma(AResultado: string): Boolean;
var SL, SLAux: TStringList;
  i, GuiasOk: Integer;
  Cabec, RepresentacaoNumerica, SituacaoGuia: string;
begin
  SL := TStringList.Create;
  SLAux := TStringList.Create;
  SL.Text := AResultado;
  GuiasOk := 0;
  
  try
    Cabec := SL.Strings[0];
    for i := 0 to SL.Count - 1 do
    begin
      if SameText(Copy(SL.Strings[i], 1, 1), '1') then
      begin
        SituacaoGuia := Trim(Copy(SL.Strings[i], 6, 1));
        if SameText(SituacaoGuia, '0') then
        begin
          SLAux.Add(Cabec);
          SLAux.Add(SL.Strings[i]);
          Inc(GuiasOk);
          RepresentacaoNumerica := Copy(SL.Strings[i], 979, 48);

          if FConfiguracoes.Geral.Salvar then
            SLAux.SaveToFile(PathWithDelim(FConfiguracoes.Geral.PathSalvar)+RepresentacaoNumerica+'-gnre.txt');
        end;
      end;

      SLAux.Clear;
      SituacaoGuia := '';
      RepresentacaoNumerica := '';
    end;
  finally
    FreeAndNil(SL);
    FreeAndNil(SLAux);
    Result := GuiasOk > 0;
  end;
end;

{ TWebServices }

constructor TWebServices.Create(AFGNRE: TComponent);
begin
  inherited Create( AFGNRE );

  FACBrGNRE     := TACBrGNRE(AFGNRE);
  FEnviar       := TGNRERecepcaoLote.Create(AFGNRE, TACBrGNRE(AFGNRE).Guias);
  FRetorno      := TGNRERetRecepcaoLote.Create(AFGNRE, TACBrGNRE(AFGNRE).Guias);
  FConsResLote  := TGNREConsultarResultadoLote.Create(AFGNRE, TACBrGNRE(AFGNRE).Guias);
  FConsConfigUF := TGNREConsultarConfigUF.Create(AFGNRE);
end;

destructor TWebServices.Destroy;
begin
  FEnviar.Free;
  FRetorno.Free;
  FConsResLote.Free;
  FConsConfigUF.Free;
 inherited;
end;

function TWebServices.Envia: Boolean;
begin
  if not(Self.Enviar.Executar) then
  begin
    if Assigned(TACBrGNRE( FACBrGNRE ).OnGerarLog) then
      TACBrGNRE( FACBrGNRE ).OnGerarLog(Self.Enviar.Msg);
    raise Exception.Create(Self.Enviar.Msg);
  end;

  Self.Retorno.numeroRecibo := Self.Enviar.numero;

  if not(Self.Retorno.Executar) then
  begin
    if Assigned(TACBrGNRE( FACBrGNRE ).OnGerarLog) then
      TACBrGNRE( FACBrGNRE ).OnGerarLog(Self.Enviar.Msg);
    raise Exception.Create(Self.Retorno.Msg);
  end;

  Result := True;
end;

function TWebServices.ConsultaResultadoLote(ANumRecibo: String): Boolean;
begin
  Self.ConsResLote.numeroRecibo := ANumRecibo;

  if not (Self.ConsResLote.Executar) then
  begin
    if Assigned(TACBrGNRE( FACBrGNRE ).OnGerarLog)
      then TACBrGNRE( FACBrGNRE ).OnGerarLog(Self.ConsResLote.Msg);
    raise Exception.Create(Self.ConsResLote.Msg);
  end;

  Result := true;
end;

{ TGNRERecepcaoLote }

constructor TGNRERecepcaoLote.Create(AOwner : TComponent;
  AGuias: TGuias);
begin
  inherited Create(AOwner);
  FGuias := AGuias;
end;

function TGNRERecepcaoLote.Executar: Boolean;
var
 aMsg        : String;
 Texto       : String;
 Acao        : TStringList;
 Stream      : TMemoryStream;
 StrStream   : TStringStream;
 {$IFDEF ACBrGNREOpenSSL}
   HTTP    : THTTPSend;
 {$ELSE}
  {$IFDEF SoapHTTP}
    ReqResp: THTTPReqResp;
   {$ELSE}
    ReqResp: TACBrHTTPReqResp;
   {$ENDIF}
 {$ENDIF}
begin
  inherited Executar;

  if Assigned(GNRERetorno)
    then GNRERetorno.Free;

  Result := False;
  Acao      := TStringList.Create;
  Stream    := TMemoryStream.Create;

  Texto := '<?xml version="1.0" encoding="utf-8" standalone="yes"?>';
  Texto := Texto + '<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap12="http://www.w3.org/2003/05/soap-envelope" xmlns="http://www.gnre.pe.gov.br/webservice/GnreLoteRecepcao">';
  Texto := Texto +   '<soap12:Header>';
  Texto := Texto +     '<gnreCabecMsg>';
  Texto := Texto +       '<versaoDados>'+GNREEnviGNRE+'</versaoDados>';
  Texto := Texto +     '</gnreCabecMsg>';
  Texto := Texto +   '</soap12:Header>';
  Texto := Texto +   '<soap12:Body>';
  Texto := Texto +     '<gnreDadosMsg>';
  Texto := Texto + FDadosMsg;
  Texto := Texto +     '</gnreDadosMsg>';
  Texto := Texto +   '</soap12:Body>';
  Texto := Texto + '</soap12:Envelope>';

  Acao.Text := Texto;

 {$IFDEF ACBrGNREOpenSSL}
   Acao.SaveToStream(Stream);
   HTTP := THTTPSend.Create;
 {$ELSE}
   {$IFDEF SoapHTTP}
    ReqResp := THTTPReqResp.Create(nil);
    ReqResp.UseUTF8InHeader := True;
   {$ELSE}
    ReqResp := TACBrHTTPReqResp.Create;
   {$ENDIF}
   ConfiguraReqResp( ReqResp );
   ReqResp.URL := FURL;
   ReqResp.SoapAction := 'https://www.gnre.pe.gov.br/gnreWS/services/GnreLoteRecepcao';
 {$ENDIF}

  try
   TACBrGNRE( FACBrGNRE ).SetStatus( stGNRERecepcao );

   if FConfiguracoes.Geral.Salvar then
     FConfiguracoes.Geral.Save(FormatDateTime('yyyymmddhhnnss',Now)+'-gnre-env-lot.xml', FDadosMsg);

    try
    {$IFDEF ACBrGNREOpenSSL}
      HTTP.Document.LoadFromStream(Stream);
      HTTP.HTTPMethod('POST', FURL);

      StrStream := TStringStream.Create('');
			try
				StrStream.CopyFrom(HTTP.Document, 0);

				FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
				FRetWS     := SeparaDados( FRetornoWS, 'processarResponse');
			finally
				StrStream.Free;
			end;
    {$ELSE}
		  {$IFDEF SoapHTTP}
        Stream := TMemoryStream.Create;
        StrStream := TStringStream.Create('');
        try
          ReqResp.Execute(Acao.Text, Stream);  // Dispara exceptions no caso de erro
          StrStream.CopyFrom(Stream, 0);
          FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
        finally
          StrStream.Free;
          Stream.Free;
        end;
      {$ELSE}
        ReqResp.Data := Acao.Text;
        FRetornoWS := ReqResp.Execute;
      {$ENDIF}
      FRetWS     := SeparaDados( FRetornoWS, 'processarResponse');

    {$ENDIF}

      GNRERetorno                := TTretLote_GNRE.Create;
      GNRERetorno.Leitor.Arquivo := GNREUtil.RetirarPrefixos(FRetWS);
      GNRERetorno.LerXml;

      TACBrGNRE( FACBrGNRE ).SetStatus( stGNREIdle );

      aMsg := 'Ambiente : '+TpAmbToStr(GNRERetorno.Ambiente)+LineBreak+
                      'Status Código : '+IntToStr(GNRERetorno.codigo)+LineBreak+
                      'Status Descrição : '+GNRERetorno.descricao+LineBreak+
                      'Recebimento : '+DFeUtil.SeSenao(GNRERetorno.dataHoraRecibo = 0, '', DateTimeToStr(GNRERetorno.dataHoraRecibo))+LineBreak+
                      'Tempo Médio : '+IntToStr(GNRERetorno.tempoEstimadoProc)+LineBreak+
                      'Número Recibo: '+GNRERetorno.numero;

      if FConfiguracoes.WebServices.Visualizar then
        ShowMessage(aMsg);

      Fcodigo             := GNRERetorno.codigo;
      Fdescricao          := GNRERetorno.descricao;
      Fnumero             := GNRERetorno.numero;
      FdataHoraRecibo     := GNRERetorno.dataHoraRecibo;
      FtempoEstimadoProc  := GNRERetorno.tempoEstimadoProc;
      FMsg                := GNRERetorno.descricao;
  
      if Assigned(TACBrGNRE( FACBrGNRE ).OnGerarLog) then
        TACBrGNRE( FACBrGNRE ).OnGerarLog(aMsg);

      Result := (GNRERetorno.codigo = 100); //Lote recebido com Sucesso

      if FConfiguracoes.Geral.Salvar
       then FConfiguracoes.Geral.Save(FormatDateTime('yyyymmddhhnnss',Now)+'-gnre-rec.xml', FRetWS);
    except
      on E: Exception do
      begin
        if Assigned(TACBrGNRE( FACBrGNRE ).OnGerarLog) then
          TACBrGNRE( FACBrGNRE ).OnGerarLog(E.Message);
        raise Exception.Create(E.Message);
      end;
    end;
    
  finally
  {$IFDEF ACBrGNREOpenSSL}
    HTTP.Free;
  {$ELSE}
    ReqResp.Free;
  {$ENDIF}
    Acao.Free;
    Stream.Free;
    DFeUtil.ConfAmbiente;
    TACBrGNRE( FACBrGNRE ).SetStatus( stGNREIdle );
  end;
end;

{ TGNRERetRecepcaoLote }

constructor TGNRERetRecepcaoLote.Create(AOwner: TComponent;
  AGuias: TGuias);
begin
  inherited Create(AOwner);
  FGuias := AGuias;
end;

destructor TGNRERetRecepcaoLote.Destroy;
begin
  if assigned(FGNRERetorno) then
    FGNRERetorno.Free;
  inherited;
end;

function TGNRERetRecepcaoLote.Executar: Boolean;

  function Processando: Boolean;
  var
    aMsg  : string;
    Texto : String;
    Acao  : TStringList ;
    Stream: TMemoryStream;
    StrStream: TStringStream;
    {$IFDEF ACBrGNREOpenSSL}
       HTTP: THTTPSend;
    {$ELSE}
		 {$IFDEF SoapHTTP}
			ReqResp: THTTPReqResp;
		 {$ELSE}
			ReqResp: TACBrHTTPReqResp;
		 {$ENDIF}		
    {$ENDIF}
  begin
    inherited Executar;

    Result := False;

    Acao   := TStringList.Create;
    Stream := TMemoryStream.Create;

    Texto := '<?xml version="1.0" encoding="utf-8" standalone="yes"?>';
    Texto := Texto + '<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap12="http://www.w3.org/2003/05/soap-envelope" xmlns="http://www.gnre.pe.gov.br/webservice/GnreResultadoLote">';
    Texto := Texto +   '<soap12:Header>';
    Texto := Texto +     '<gnreCabecMsg>';
    Texto := Texto +       '<versaoDados>'+GNREConsResLote+'</versaoDados>';
    Texto := Texto +     '</gnreCabecMsg>';
    Texto := Texto +   '</soap12:Header>';
    Texto := Texto +   '<soap12:Body>';
    Texto := Texto +     '<gnreDadosMsg>';
    Texto := Texto + FDadosMsg;
    Texto := Texto +     '</gnreDadosMsg>';
    Texto := Texto +   '</soap12:Body>';
    Texto := Texto + '</soap12:Envelope>';

    Acao.Text := Texto;

    {$IFDEF ACBrGNREOpenSSL}
       Acao.SaveToStream(Stream);
       HTTP := THTTPSend.Create;
    {$ELSE}
			 {$IFDEF SoapHTTP}
				ReqResp := THTTPReqResp.Create(nil);
				ReqResp.UseUTF8InHeader := True;
			 {$ELSE}
				ReqResp := TACBrHTTPReqResp.Create;
			 {$ENDIF}       
       ConfiguraReqResp( ReqResp );
       ReqResp.URL := Trim(FURL);
       ReqResp.SoapAction := 'http://www.gnre.pe.gov.br/gnreWS/services/GnreResultadoLote';
    {$ENDIF}

    try
      TACBrGNRE( FACBrGNRE ).SetStatus( stGNREConsulta );
      if Assigned(FGNRERetorno) then
        FGNRERetorno.Free;

      if FConfiguracoes.Geral.Salvar then
        FConfiguracoes.Geral.Save(numeroRecibo + '-ped-rec.xml', FDadosMsg);

      try
        {$IFDEF ACBrGNREOpenSSL}
           HTTP.Document.LoadFromStream(Stream);
           ConfiguraHTTP(HTTP,'SOAPAction: "http://www.gnre.pe.gov.br/gnreWS/services/GnreResultadoLote"');
           HTTP.HTTPMethod('POST', FURL);
           StrStream := TStringStream.Create('');
					 try
						 StrStream.CopyFrom(HTTP.Document, 0);

						 FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
						 FRetWS := SeparaDados( FRetornoWS, 'gnreRespostaMsg');
					 finally
						 StrStream.Free;
					 end;
        {$ELSE}
					{$IFDEF SoapHTTP}
						Stream := TMemoryStream.Create;
						StrStream := TStringStream.Create('');
						try
							ReqResp.Execute(Acao.Text, Stream);  // Dispara exceptions no caso de erro
							StrStream.CopyFrom(Stream, 0);
							FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
						finally
							StrStream.Free;
							Stream.Free;
						end;
					{$ELSE}
						ReqResp.Data := Acao.Text;
						FRetornoWS := ReqResp.Execute;
					{$ENDIF}
          FRetWS := SeparaDados( FRetornoWS, 'gnreRespostaMsg');
        {$ENDIF}

      if FConfiguracoes.Geral.Salvar then
         FConfiguracoes.Geral.Save(numeroRecibo + '-pro-rec.xml', FRetWS);

        FGNRERetorno := TTResultLote_GNRE.Create;
        FGNRERetorno.Leitor.Arquivo := GNREUtil.RetirarPrefixos(FRetWS);
        FGNRERetorno.LerXml;

        TACBrGNRE( FACBrGNRE ).SetStatus( stGNREIdle );
        aMsg := 'Ambiente : '+TpAmbToStr(GNRERetorno.Ambiente)+LineBreak+
                'Protocolo do Lote : '+GNRERetorno.resInfoCabec.NumeroProtocoloLote+LineBreak+
                'Status Código : '+IntToStr(GNRERetorno.codigo)+LineBreak+
                'Status Descrição : '+UTF8Decode(GNRERetorno.descricao)+LineBreak;

        if FConfiguracoes.WebServices.Visualizar then
          ShowMessage(aMsg);

        if Assigned(TACBrGNRE( FACBrGNRE ).OnGerarLog) then
           TACBrGNRE( FACBrGNRE ).OnGerarLog(aMsg);

        Fambiente  := GNRERetorno.Ambiente;
        Fcodigo    := GNRERetorno.codigo;
        Fdescricao := GNRERetorno.descricao;
        Fresultado := GNRERetorno.resultado;
        FMsg       := GNRERetorno.descricao;

        Result := (GNRERetorno.codigo = 401); // 401 = Lote em Processamento.
      except on E: Exception do
        begin
          if Assigned(TACBrGNRE( FACBrGNRE ).OnGerarLog) then
            TACBrGNRE( FACBrGNRE ).OnGerarLog('WebService Consulta Resultado Lote:'+LineBreak+
                                            '- Inativo ou Inoperante tente novamente.'+LineBreak+
                                            '- '+E.Message);
          raise Exception.Create('WebService Consulta Resultado Lote:'+LineBreak+
                                '- Inativo ou Inoperante tente novamente.'+LineBreak+
                                '- '+E.Message);
        end;
      end;

    finally
      {$IFDEF ACBrGNREOpenSSL}
         HTTP.Free;
      {$ELSE}
        ReqResp.Free;
      {$ENDIF}
      Acao.Free;
      Stream.Free;
      DFeUtil.ConfAmbiente;
      TACBrGNRE( FACBrGNRE ).SetStatus( stGNREIdle );
    end;
  end;

var
  vCont: Integer;
begin
  inherited Executar;
  Result := False;
  
  TACBrGNRE( FACBrGNRE ).SetStatus( stGNRERetRecepcao );
  Sleep(TACBrGNRE( FACBrGNRE ).Configuracoes.WebServices.AguardarConsultaRet);
  vCont := 1000;
  while Processando do
  begin
    if TACBrGNRE( FACBrGNRE ).Configuracoes.WebServices.IntervaloTentativas > 0 then
       Sleep(TACBrGNRE( FACBrGNRE ).Configuracoes.WebServices.IntervaloTentativas)
    else
       Sleep(vCont);

    if vCont > (TACBrGNRE( FACBrGNRE ).Configuracoes.WebServices.Tentativas * 1000) then
      break;

    vCont := vCont + 1000;
  end;
  TACBrGNRE( FACBrGNRE ).SetStatus( stGNREIdle );

  if (GNRERetorno.codigo = 402) then
  begin
    Result := Confirma(resultado);
    Fprotocolo := GNRERetorno.resInfoCabec.NumeroProtocoloLote;
  end;
end;

{ TGNREConsultarSituacaoLote }

constructor TGNREConsultarResultadoLote.Create(AOwner: TComponent;
  AGuias : TGuias);
begin
  inherited Create(AOwner);
  FGuias := AGuias;
end;

function TGNREConsultarResultadoLote.Executar: Boolean;
var
  aMsg  : string;
  Texto : String;
  Acao  : TStringList ;
  Stream: TMemoryStream;
  StrStream: TStringStream;
  {$IFDEF ACBrGNREOpenSSL}
     HTTP: THTTPSend;
  {$ELSE}
		 {$IFDEF SoapHTTP}
			ReqResp: THTTPReqResp;
		 {$ELSE}
			ReqResp: TACBrHTTPReqResp;
		 {$ENDIF}		
  {$ENDIF}
begin
  inherited Executar;

  Result := False;

  Acao   := TStringList.Create;
  Stream := TMemoryStream.Create;

  Texto := '<?xml version="1.0" encoding="utf-8" standalone="yes"?>';
  Texto := Texto + '<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap12="http://www.w3.org/2003/05/soap-envelope" xmlns="http://www.gnre.pe.gov.br/webservice/GnreResultadoLote">';
  Texto := Texto +   '<soap12:Header>';
  Texto := Texto +     '<gnreCabecMsg>';
  Texto := Texto +       '<versaoDados>'+GNREConsResLote+'</versaoDados>';
  Texto := Texto +     '</gnreCabecMsg>';
  Texto := Texto +   '</soap12:Header>';
  Texto := Texto +   '<soap12:Body>';
  Texto := Texto +     '<gnreDadosMsg>';
  Texto := Texto + FDadosMsg;
  Texto := Texto +     '</gnreDadosMsg>';
  Texto := Texto +   '</soap12:Body>';
  Texto := Texto + '</soap12:Envelope>';

  Acao.Text := Texto;

  {$IFDEF ACBrGNREOpenSSL}
     Acao.SaveToStream(Stream);
     HTTP := THTTPSend.Create;
  {$ELSE}
		 {$IFDEF SoapHTTP}
			ReqResp := THTTPReqResp.Create(nil);
			ReqResp.UseUTF8InHeader := True;
		 {$ELSE}
			ReqResp := TACBrHTTPReqResp.Create;
		 {$ENDIF}       
     ConfiguraReqResp( ReqResp );
     ReqResp.URL := Trim(FURL);
     ReqResp.SoapAction := 'http://www.gnre.pe.gov.br/gnreWS/services/GnreResultadoLote';
  {$ENDIF}

  try
    TACBrGNRE( FACBrGNRE ).SetStatus( stGNREConsulta );
    if Assigned(FGNRERetorno) then
      FGNRERetorno.Free;

    if FConfiguracoes.Geral.Salvar then
      FConfiguracoes.Geral.Save(numeroRecibo + '-ped-res.xml', FDadosMsg);

    try
      {$IFDEF ACBrGNREOpenSSL}
         HTTP.Document.LoadFromStream(Stream);
         ConfiguraHTTP(HTTP,'SOAPAction: "http://www.gnre.pe.gov.br/gnreWS/services/GnreResultadoLote"');
         HTTP.HTTPMethod('POST', FURL);
         StrStream := TStringStream.Create('');
				 try
					 StrStream.CopyFrom(HTTP.Document, 0);

					 FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
					 FRetWS := SeparaDados( FRetornoWS, 'gnreRespostaMsg');
				 finally
					 StrStream.Free;
				 end;
      {$ELSE}
				{$IFDEF SoapHTTP}
					Stream := TMemoryStream.Create;
					StrStream := TStringStream.Create('');
					try
						ReqResp.Execute(Acao.Text, Stream);  // Dispara exceptions no caso de erro
						StrStream.CopyFrom(Stream, 0);
						FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
					finally
						StrStream.Free;
						Stream.Free;
					end;
				{$ELSE}
					ReqResp.Data := Acao.Text;
					FRetornoWS := ReqResp.Execute;
				{$ENDIF}
         FRetWS := SeparaDados( FRetornoWS, 'gnreRespostaMsg');
      {$ENDIF}

      if FConfiguracoes.Geral.Salvar then
         FConfiguracoes.Geral.Save(numeroRecibo + '-pro-res.xml', FRetWS);

      FGNRERetorno := TTResultLote_GNRE.Create;
      FGNRERetorno.Leitor.Arquivo := GNREUtil.RetirarPrefixos(FRetWS);
      FGNRERetorno.LerXml;

      TACBrGNRE( FACBrGNRE ).SetStatus( stGNREIdle );
      aMsg := 'Ambiente : '+TpAmbToStr(GNRERetorno.Ambiente)+LineBreak+
              'Status Código : '+IntToStr(GNRERetorno.codigo)+LineBreak+
              'Status Descrição : '+UTF8Decode(GNRERetorno.descricao)+LineBreak;

      if FConfiguracoes.WebServices.Visualizar then
        ShowMessage(aMsg);

      if Assigned(TACBrGNRE( FACBrGNRE ).OnGerarLog) then
         TACBrGNRE( FACBrGNRE ).OnGerarLog(aMsg);

      Fambiente  := GNRERetorno.Ambiente;
      Fcodigo    := GNRERetorno.codigo;
      Fdescricao := GNRERetorno.descricao;
      Fresultado := GNRERetorno.resultado;
      FMsg       := GNRERetorno.descricao;

      Result := (GNRERetorno.codigo = 402); // 402 = Lote processado com sucesso.

      if Result then
      begin
        Confirma(resultado);
        Fprotocolo := GNRERetorno.resInfoCabec.NumeroProtocoloLote;
      end;

      if FConfiguracoes.Geral.Salvar then
        FConfiguracoes.Geral.Save(numeroRecibo+'-res.xml', FRetWS);

    except on E: Exception do
      begin
        if Assigned(TACBrGNRE( FACBrGNRE ).OnGerarLog) then
          TACBrGNRE( FACBrGNRE ).OnGerarLog('WebService Consulta Resultado Lote:'+LineBreak+
                                          '- Inativo ou Inoperante tente novamente.'+LineBreak+
                                          '- '+E.Message);
        raise Exception.Create('WebService Consulta Resultado Lote:'+LineBreak+
                              '- Inativo ou Inoperante tente novamente.'+LineBreak+
                              '- '+E.Message);
      end;
    end;

  finally
    {$IFDEF ACBrGNREOpenSSL}
       HTTP.Free;
    {$ELSE}
      ReqResp.Free;
    {$ENDIF}
    Acao.Free;
    Stream.Free;
    DFeUtil.ConfAmbiente;
    TACBrGNRE( FACBrGNRE ).SetStatus( stGNREIdle );
  end;
end;

{ TGNREConsultarConfigUF }

constructor TGNREConsultarConfigUF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TGNREConsultarConfigUF.Destroy;
begin
  if assigned(FGNRERetorno) then
    FGNRERetorno.Free;
  inherited;
end;

function TGNREConsultarConfigUF.Executar: Boolean;
var
  aMsg  : string;
  Texto : String;
  Acao  : TStringList ;
  Stream: TMemoryStream;
  StrStream: TStringStream;

  {$IFDEF ACBrGNREOpenSSL}
     HTTP: THTTPSend;
  {$ELSE}
		 {$IFDEF SoapHTTP}
			ReqResp: THTTPReqResp;
		 {$ELSE}
			ReqResp: TACBrHTTPReqResp;
		 {$ENDIF}		
  {$ENDIF}
begin
  if Assigned(FGNRERetorno) then
    FGNRERetorno.Free;

  inherited Executar;

  Result := False;

  Acao   := TStringList.Create;
  Stream := TMemoryStream.Create;

  Texto := '<?xml version="1.0" encoding="utf-8" standalone="yes"?>';
  Texto := Texto + '<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap12="http://www.w3.org/2003/05/soap-envelope" xmlns="http://www.gnre.pe.gov.br/webservice/GnreConfigUF">';
  Texto := Texto +   '<soap12:Header>';
  Texto := Texto +     '<gnreCabecMsg>';
  Texto := Texto +       '<versaoDados>'+GNREConsConfigUF+'</versaoDados>';
  Texto := Texto +     '</gnreCabecMsg>';
  Texto := Texto +   '</soap12:Header>';
  Texto := Texto +   '<soap12:Body>';
  Texto := Texto +     '<gnreDadosMsg>';
  Texto := Texto + FDadosMsg;
  Texto := Texto +     '</gnreDadosMsg>';
  Texto := Texto +   '</soap12:Body>';
  Texto := Texto + '</soap12:Envelope>';

  Acao.Text := Texto;

  {$IFDEF ACBrGNREOpenSSL}
     Acao.SaveToStream(Stream);
     HTTP := THTTPSend.Create;
  {$ELSE}
		 {$IFDEF SoapHTTP}
			ReqResp := THTTPReqResp.Create(nil);
			ReqResp.UseUTF8InHeader := True;
		 {$ELSE}
			ReqResp := TACBrHTTPReqResp.Create;
		 {$ENDIF}       
     ConfiguraReqResp( ReqResp );
     ReqResp.URL := Trim(FURL);
     ReqResp.SoapAction := 'http://www.gnre.pe.gov.br/gnreWS/services/GnreConfigUF';
  {$ENDIF}

  try
    TACBrGNRE( FACBrGNRE ).SetStatus( stGNREConsultaConfigUF );

    if FConfiguracoes.Geral.Salvar then
      FConfiguracoes.Geral.Save(FormatDateTime('yyyymmddhhnnss',Now)+'-ped-cfg.xml', FDadosMsg);

    try
      {$IFDEF ACBrGNREOpenSSL}
         HTTP.Document.LoadFromStream(Stream);
         ConfiguraHTTP(HTTP,'SOAPAction: "http://www.gnre.pe.gov.br/gnreWS/services/GnreConfigUF"');
         HTTP.HTTPMethod('POST', FURL);
         StrStream := TStringStream.Create('');
				 try
					 StrStream.CopyFrom(HTTP.Document, 0);

					 FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
					 FRetWS := SeparaDados( FRetornoWS, 'gnreRespostaMsg');
				 finally
					 StrStream.Free;
				 end;
      {$ELSE}
				{$IFDEF SoapHTTP}
					Stream := TMemoryStream.Create;
					StrStream := TStringStream.Create('');
					try
						ReqResp.Execute(Acao.Text, Stream);  // Dispara exceptions no caso de erro
						StrStream.CopyFrom(Stream, 0);
						FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
					finally
						StrStream.Free;
						Stream.Free;
					end;
				{$ELSE}
					ReqResp.Data := Acao.Text;
					FRetornoWS := ReqResp.Execute;
				{$ENDIF}
         FRetWS := SeparaDados( FRetornoWS, 'gnreRespostaMsg');
      {$ENDIF}
      FGNRERetorno := TTConfigUf.Create;
      FGNRERetorno.Leitor.Arquivo := GNREUtil.RetirarPrefixos(FRetWS);
      FGNRERetorno.LerXml;

      TACBrGNRE( FACBrGNRE ).SetStatus( stGNREIdle );
      aMsg := 'Ambiente : '+TpAmbToStr(GNRERetorno.Ambiente)+LineBreak+
              'Status Código : '+IntToStr(GNRERetorno.codigo)+LineBreak+
              'Status Descrição : '+UTF8Decode(GNRERetorno.descricao)+LineBreak+
              'UF : '+GNRERetorno.Uf;

      if FConfiguracoes.WebServices.Visualizar then
        ShowMessage(aMsg);

      if Assigned(TACBrGNRE( FACBrGNRE ).OnGerarLog) then
         TACBrGNRE( FACBrGNRE ).OnGerarLog(aMsg);

      Fambiente                   := GNRERetorno.Ambiente;
      Fcodigo                     := GNRERetorno.codigo;
      Fdescricao                  := UTF8Decode(GNRERetorno.descricao);
      FexigeReceita               := GNRERetorno.exigeReceita;
      FexigeDataVencimento        := GNRERetorno.exigeDataVencimento;
      FexigeDataPagamento         := GNRERetorno.exigeDataPagamento;
      FexigeContribuinteEmitente  := GNRERetorno.exigeContribuinteEmitente;
      FexigeUfFavorecida          := GNRERetorno.exigeUfFavorecida;
      FexigeConvenio              := GNRERetorno.exigeConvenio;
      FUf                         := GNRERetorno.Uf;
      FMsg                        := UTF8Decode(GNRERetorno.descricao);

      Result := (GNRERetorno.codigo = 450); // 450 = Consulta da configuração da UF realizada com sucesso.

      if FConfiguracoes.Geral.Salvar then
        FConfiguracoes.Geral.Save(FormatDateTime('yyyymmddhhnnss',Now)+'-cfg.xml', FRetWS);

    except on E: Exception do
      begin
       if Assigned(TACBrGNRE( FACBrGNRE ).OnGerarLog) then
          TACBrGNRE( FACBrGNRE ).OnGerarLog('WebService Consulta Configuração UF:'+LineBreak+
                                          '- Inativo ou Inoperante tente novamente.'+LineBreak+
                                          '- '+E.Message);
       raise Exception.Create('WebService Consulta Configuração UF:'+LineBreak+
                              '- Inativo ou Inoperante tente novamente.'+LineBreak+
                              '- '+E.Message);
      end;
    end;

  finally
    {$IFDEF ACBrGNREOpenSSL}
       HTTP.Free;
    {$ELSE}
      ReqResp.Free;
    {$ENDIF}
    Acao.Free;
    Stream.Free;
    DFeUtil.ConfAmbiente;
    TACBrGNRE( FACBrGNRE ).SetStatus( stGNREIdle );
  end;
end;

end.
