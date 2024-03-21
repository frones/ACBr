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

unit ACBrANeWebServices;

interface

uses
  Classes, SysUtils, synacode,
  ACBrDFe, ACBrDFeWebService, ACBrDFeUtil,
  ACBrANeDocumentos, ACBrANeConfiguracoes,
  pcnConversao, pcaConversao, pcaANe, pcaRetEnvANe;

const
  CURL_WSDL = '';

type

  { TANeWebService }

  TANeWebService = class(TDFeWebService)
  private
  protected
    FPStatus: TStatusACBrANe;
    FPLayout: TLayOutANe;
    FPConfiguracoesANe: TConfiguracoesANe;

    function ExtrairModeloChaveAcesso(AChaveANe: String): String;

  protected
    procedure InicializarServico; override;
    procedure DefinirEnvelopeSoap; override;
    procedure DefinirURL; override;
    function GerarVersaoDadosSoap: String; override;
    procedure EnviarDados; override;
    procedure FinalizarServico; override;

  public
    constructor Create(AOwner: TACBrDFe); override;
    procedure Clear; override;

    property Status: TStatusACBrANe read FPStatus;
    property Layout: TLayOutANe read FPLayout;
  end;

  { TANeAverbar }

  TANeAverbar = class(TANeWebService)
  private
    FDocumentos: TDocumentos;
    FANeRetorno: TRetEnvANe;

    FdhAverbacao: TDateTime;
    FProtocolo: String;
    FNumeroAverbacao: String;
    FArquivo: String;

    FErroCodigo: String;
    FErroDescricao: String;
    FErroValorEsperado: String;
    FErroValorInformado: String;

    FInfoCodigo: String;
    FInfoDescricao: String;

  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarMsgErro(E: Exception): String; override;
  public
    constructor Create(AOwner: TACBrDFe; ADocumentos: TDocumentos);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    // Documento Averbado
    property dhAverbacao: TDateTime  read FdhAverbacao     write FdhAverbacao;
    property Protocolo: String       read FProtocolo       write FProtocolo;
    property NumeroAverbacao: String read FNumeroAverbacao write FNumeroAverbacao;
    property Arquivo: String         read FArquivo         write FArquivo;

    // Documento Rejeitado
    property ErroCodigo: String         read FErroCodigo         write FErroCodigo;
    property ErroDescricao: String      read FErroDescricao      write FErroDescricao;
    property ErroValorEsperado: String  read FErroValorEsperado  write FErroValorEsperado;
    property ErroValorInformado: String read FErroValorInformado write FErroValorInformado;

    // Documento não Rejeitado
    property InfoCodigo: String    read FInfoCodigo    write FInfoCodigo;
    property InfoDescricao: String read FInfoDescricao write FInfoDescricao;

    property ANeRetorno: TRetEnvANe read FANeRetorno;
  end;

  { TANeEnvioWebService }

  TANeEnvioWebService = class(TANeWebService)
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
    procedure Clear; override;

    property XMLEnvio: String read FXMLEnvio write FXMLEnvio;
    property URLEnvio: String read FPURLEnvio write FPURLEnvio;
    property SoapActionEnvio: String read FSoapActionEnvio write FSoapActionEnvio;
  end;

  { TWebServices }

  TWebServices = class
  private
    FACBrANe: TACBrDFe;
    FANeAverbar: TANeAverbar;
    FEnvioWebService: TANeEnvioWebService;
  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    function Envia: Boolean;

    property ACBrANe: TACBrDFe read FACBrANe write FACBrANe;
    property ANeAverbar: TANeAverbar read FANeAverbar write FANeAverbar;
    property EnvioWebService: TANeEnvioWebService read FEnvioWebService write FEnvioWebService;
  end;

implementation

uses
  StrUtils, Math,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  ACBrUtil.DateTime,
  ACBrANe,
  ACBrDFeConsts,
  pcnGerador, pcnLeitor;

{ TANeWebService }

constructor TANeWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPConfiguracoesANe := TConfiguracoesANe(FPConfiguracoes);
  FPLayout := LayANeAverbacao;
  FPStatus := stANeIdle;

  FPSoapVersion := 'soapenv';
  FPHeaderElement := '';
  FPBodyElement := '';
  FPSoapEnvelopeAtributtes := 'xmlns:urn="urn:ATMWebSvr" ' +
    'xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"';
end;

procedure TANeWebService.Clear;
begin
  inherited Clear;

  FPStatus := stANeIdle;
  FPMimeType := 'text/xml';
  FPDFeOwner.SSL.UseCertificateHTTP := True;
end;

function TANeWebService.ExtrairModeloChaveAcesso(AChaveANe: String): String;
begin
  AChaveANe := OnlyNumber(AChaveANe);
  if ValidarChave('ANe' + AChaveANe) then
    Result := copy(AChaveANe, 21, 2)
  else
    Result := '';
end;

procedure TANeWebService.InicializarServico;
begin
  { Sobrescrever apenas se necessário }
  inherited InicializarServico;

  TACBrANe(FPDFeOwner).SetStatus(FPStatus);
end;

procedure TANeWebService.DefinirEnvelopeSoap;
var
  Texto: String;
begin
  {$IFDEF FPC}
   Texto := '<' + ENCODING_UTF8 + '>';    // Envelope já está sendo montado em UTF8
  {$ELSE}
   Texto := '';  // Isso forçará a conversão para UTF8, antes do envio
  {$ENDIF}

  FPDadosMsg := RemoverDeclaracaoXML(FPDadosMsg);

  Texto := Texto + '<' + FPSoapVersion + ':Envelope ' + FPSoapEnvelopeAtributtes + '>';

  case FPConfiguracoesANe.Geral.Seguradora of
    tsELT:
      begin
        Texto := Texto + '<' + FPSoapVersion + ':Header>';
        Texto := Texto + FPCabMsg;
        Texto := Texto + '</' + FPSoapVersion + ':Header>';
      end;
  else
    Texto := Texto + '<' + FPSoapVersion + ':Header/>';
  end;

  Texto := Texto + '<' + FPSoapVersion + ':Body>';
  Texto := Texto + '<' + FPBodyElement + '>';
  Texto := Texto + FPDadosMsg;
  Texto := Texto + '</' + FPBodyElement + '>';
  Texto := Texto + '</' + FPSoapVersion + ':Body>';
  Texto := Texto + '</' + FPSoapVersion + ':Envelope>';

  FPEnvelopeSoap := Texto;
end;

procedure TANeWebService.DefinirURL;
var
  Versao: Double;
begin
  { sobrescrever apenas se necessário.
    Você também pode mudar apenas o valor de "FLayoutServico" na classe
    filha e chamar: Inherited;     }

  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';

  TACBrANe(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;

function TANeWebService.GerarVersaoDadosSoap: String;
begin
  { Sobrescrever apenas se necessário }

  if EstaVazio(FPVersaoServico) then
    FPVersaoServico := TACBrANe(FPDFeOwner).LerVersaoDeParams(FPLayout);

  Result := '<versaoDados>' + FPVersaoServico + '</versaoDados>';
end;

procedure TANeWebService.FinalizarServico;
begin
  { Sobrescrever apenas se necessário }

  TACBrANe(FPDFeOwner).SetStatus(stANeIdle);
end;

procedure TANeWebService.EnviarDados;
Var
  Tentar, Tratado: Boolean;
begin
  { sobrescrever apenas se necessário.
    Você também pode mudar apenas o valor de "FLayoutServico" na classe
    filha e chamar: Inherited;     }

  FPRetWS := '';
  FPRetornoWS := '';
  Tentar := True;

  FPEnvelopeSoap := UTF8ToNativeString(FPEnvelopeSoap);

  while Tentar do
  begin
    Tentar := False;
    Tratado := False;

    // Tem Certificado carregado ?
    if (FPConfiguracoes.Certificados.NumeroSerie <> '') then
      if FPConfiguracoes.Certificados.VerificarValidade then
        if (FPDFeOwner.SSL.CertDataVenc < Now) then
          GerarException(ACBrStr('Data de Validade do Certificado já expirou: ' +
                                 FormatDateBr(FPDFeOwner.SSL.CertDataVenc)));

    try
      FPRetornoWS := FPDFeOwner.SSL.Enviar(FPEnvelopeSoap, FPURL, FPSoapAction, FPMimeType);
    except
      if Assigned(FPDFeOwner.OnTransmitError) then
        FPDFeOwner.OnTransmitError(FPDFeOwner.SSL.HTTPResultCode,
                                   FPDFeOwner.SSL.InternalErrorCode,
                                   FPURL, FPEnvelopeSoap, FPSoapAction,
                                   Tentar, Tratado);

      if not (Tentar or Tratado) then
        raise;
    end;
  end;
end;

{ TANeAverbar }

constructor TANeAverbar.Create(AOwner: TACBrDFe;
  ADocumentos: TDocumentos);
begin
  inherited Create(AOwner);

  FDocumentos := ADocumentos;
end;

destructor TANeAverbar.Destroy;
begin
  FANeRetorno.Free;

  inherited Destroy;
end;

procedure TANeAverbar.Clear;
begin
  inherited Clear;

  FPStatus := stANeAverbacao;
  FPLayout := LayANeAverbacao;
  FPArqEnv := 'ped-ANe';
  FPArqResp := 'res-ANe';

  FdhAverbacao := 0;
  FProtocolo := '';
  FNumeroAverbacao := '';
  FArquivo := '';

  FErroCodigo := '';
  FErroDescricao := '';
  FErroValorEsperado := '';
  FErroValorInformado := '';

  FInfoCodigo := '';
  FInfoDescricao := '';

  if Assigned(FANeRetorno) then
    FANeRetorno.Free;

  FANeRetorno := TretEnvANe.Create;

  FANeRetorno.Numero := '';
  FANeRetorno.Serie := '';
  FANeRetorno.Filial := '';
  FANeRetorno.CNPJCli := '';
  FANeRetorno.TpDoc := 0;
  FANeRetorno.Averbado.dhAverbacao := 0;
  FANeRetorno.Averbado.Protocolo := '';
  FANeRetorno.Declarado.dhChancela := 0;
  FANeRetorno.Declarado.Protocolo := '';

end;

procedure TANeAverbar.DefinirServicoEAction;
var
  sTipoDoc: String;
begin
  case FPConfiguracoesANe.Geral.TipoDoc of
   tdCTe: sTipoDoc := 'averbaCTe';
   tdNFe: sTipoDoc := 'averbaNFe';
   tdMDFe: sTipoDoc := 'declaraMDFe';
  end;

  case FPConfiguracoesANe.Geral.Seguradora of
    tsELT:
      begin
        FPSoapEnvelopeAtributtes := 'xmlns:tem="http://tempuri.org/" ' +
                      'xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"';
        FPServico := CURL_WSDL + '*';
        FPSoapAction := 'http://tempuri.org/IELTAverbaService/FileUpload';
        FPBodyElement := 'tem:RemoteFileInfo';
      end;
  else
    begin
      FPSoapEnvelopeAtributtes := 'xmlns:urn="urn:ATMWebSvr" ' +
                    'xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"';
      FPServico := CURL_WSDL + sTipoDoc;
      FPSoapAction := 'urn:ATMWebSvr#' + sTipoDoc;
      FPBodyElement := 'urn:' + sTipoDoc + 'Request';
    end;
  end;
end;

procedure TANeAverbar.DefinirDadosMsg;

function RemoverGrupoInfSuplementares(const XML, Grupo: string): string;
var
  IniGrupo, FimGrupo: Integer;
begin
  IniGrupo := Pos('<' + Grupo + '>', XML);
  if IniGrupo > 0 then
  begin
    FimGrupo := Pos('</' + Grupo + '>', XML) + Length(Grupo) + 3;

    Result := Copy(XML, 1, IniGrupo -1) + Copy(XML, FimGrupo, Length(XML));
  end
  else
    Result := XML;
end;

begin
  case FPConfiguracoesANe.Geral.Seguradora of
    tsELT:
    begin
      FPCabMsg   := RemoverDeclaracaoXML(FDocumentos.Items[0].XMLAssinado);
      FPDadosMsg := '<tem:FileByteStream>' +
                      EncodeBase64( FDocumentos.Items[0].ANe.xmlDFe ) +
                    '</tem:FileByteStream>';
    end
  else
    begin
      FPCabMsg   := '';

      with FDocumentos.Items[0] do
      begin
        case FPConfiguracoesANe.Geral.TipoDoc of
          tdCTe: FPDadosMsg := RemoverGrupoInfSuplementares(XMLAssinado, 'infCTeSupl');

          tdMDFe: FPDadosMsg := RemoverGrupoInfSuplementares(XMLAssinado, 'infMDFeSupl');

          tdNFe,
          tdAddBackMail: FPDadosMsg := XMLAssinado;
        end;
      end;
    end;
  end;
end;

function TANeAverbar.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDadosArray(['SOAP-ENV:Body',
                               's:Body'],FPRetornoWS );

//  FPRetWS := SeparaDados(FPRetornoWS, 'SOAP-ENV:Body');

  //A função UTF8ToNativeString deve ser removida quando for refatorado para usar ACBrXmlDocument
  ANeRetorno.Leitor.Arquivo := UTF8ToNativeString(ParseText(FPRetWS));
  ANeRetorno.Seguradora := FPConfiguracoesANe.Geral.Seguradora;
  ANeRetorno.LerXml;

  case FPConfiguracoesANe.Geral.Seguradora of
    tsELT:
      begin
        FdhAverbacao   := ANeRetorno.DataHora;
        FProtocolo     := ANeRetorno.Protocolo;

        FNumeroAverbacao := ANeRetorno.CTE;

        FErroCodigo    := IntToStr(ANeRetorno.Codigo);
        FErroDescricao := ANeRetorno.Resultado;
        FArquivo       := ANeRetorno.fname;

        FPMsg := ANeRetorno.status;

        Result := (FErroCodigo <> '') or (FProtocolo <> '');
      end;
  else
    begin
      if( FPConfiguracoesANe.Geral.TipoDoc = tdMDFe )then
      begin
        FdhAverbacao := ANeRetorno.Declarado.dhChancela;
        FProtocolo   := ANeRetorno.Declarado.Protocolo;
      end
      else
      begin
        FdhAverbacao := ANeRetorno.Averbado.dhAverbacao;
        FProtocolo   := ANeRetorno.Averbado.Protocolo;
      end;

      if ANeRetorno.Averbado.DadosSeguro.Count > 0 then
        FNumeroAverbacao := ANeRetorno.Averbado.DadosSeguro.Items[0].NumeroAverbacao;

      if ANeRetorno.Erros.Erro.Count > 0 then
      begin
        FErroCodigo         := ANeRetorno.Erros.Erro.Items[0].Codigo;
        FErroDescricao      := ANeRetorno.Erros.Erro.Items[0].Descricao;
        FErroValorEsperado  := ANeRetorno.Erros.Erro.Items[0].ValorEsperado;
        FErroValorInformado := ANeRetorno.Erros.Erro.Items[0].ValorInformado;

        FPMsg := FErroDescricao;
      end;

      if ANeRetorno.Infos.Info.Count > 0 then
      begin
        FInfoCodigo    := ANeRetorno.Infos.Info.Items[0].Codigo;
        FInfoDescricao := ANeRetorno.Infos.Info.Items[0].Descricao;

        FPMsg := FInfoDescricao;
      end;

      Result := (FErroCodigo <> '') or (FInfoCodigo <> '') or (FProtocolo <> '');
    end;
  end;
end;

function TANeAverbar.GerarMsgLog: String;
var
  xMsg: String;
begin
  xMsg := '';

  case FPConfiguracoesANe.Geral.Seguradora of
    tsELT:
      begin
        xMsg := Format(ACBrStr('Averbação:' + LineBreak +
                               ' Data     : %s ' + LineBreak +
                               ' Protocolo: %s ' + LineBreak +
                               ' Numero   : %s ' + LineBreak +
                               ' Arquivo  : %s ' + LineBreak +
                               ' Status   : %s ' + LineBreak + LineBreak +
                               'Resultado:' + LineBreak +
                               ' Código         : %s' + LineBreak +
                               ' Descrição      : %s' + LineBreak),
                     [IfThen(FdhAverbacao = 0, '', FormatDateTimeBr(FdhAverbacao)),
                      FProtocolo,
                      FNumeroAverbacao,
                      FArquivo,
                      FPMsg,
                      FErroCodigo,
                      FErroDescricao]);
      end;
  else
    begin
      if FErroCodigo <> '' then
      begin
        xMsg := Format(ACBrStr('Averbação:' + LineBreak +
                               ' Data     : %s ' + LineBreak +
                               ' Protocolo: %s ' + LineBreak +
                               ' Numero   : %s ' + LineBreak + LineBreak +
                               'Erro:' + LineBreak +
                               ' Código         : %s' + LineBreak +
                               ' Descrição      : %s' + LineBreak +
                               ' Valor Esperado : %s' + LineBreak +
                               ' Valor Informado: %s' + LineBreak),
                     [IfThen(FdhAverbacao = 0, '', FormatDateTimeBr(FdhAverbacao)),
                      FProtocolo,
                      FNumeroAverbacao,
                      FErroCodigo,
                      FErroDescricao,
                      FErroValorEsperado,
                      FErroValorInformado]);
      end;

      if FInfoCodigo <> '' then
      begin
        xMsg := Format(ACBrStr('Averbação:' + LineBreak +
                               ' Data     : %s ' + LineBreak +
                               ' Protocolo: %s ' + LineBreak +
                               ' Numero   : %s ' + LineBreak + LineBreak +
                               'Informações:' + LineBreak +
                               ' Código   : %s' + LineBreak +
                               ' Descrição: %s' + LineBreak),
                       [IfThen(FdhAverbacao = 0, '', FormatDateTimeBr(FdhAverbacao)),
                        FProtocolo,
                        FNumeroAverbacao,
                        FInfoCodigo,
                        FInfoDescricao]);
      end;

      if (FProtocolo <> '') and (FErroCodigo = '') and (FInfoCodigo = '') then
      begin
        xMsg := Format(ACBrStr('Averbação:' + LineBreak +
                               ' Data     : %s ' + LineBreak +
                               ' Protocolo: %s ' + LineBreak +
                               ' Numero   : %s ' + LineBreak + LineBreak),
                         [IfThen(FdhAverbacao = 0, '', FormatDateTimeBr(FdhAverbacao)),
                          FProtocolo,
                          FNumeroAverbacao]);

        if ANeRetorno.Averbado.DadosSeguro.Count > 0 then
        begin
          xMsg := xMsg + Format(ACBrStr('Dados do Seguro:' + LineBreak +
                                        ' Numero Averbação: %s' + LineBreak +
                                        ' CNPJ Seguradora: %s' + LineBreak +
                                        ' Nome Seguradora: %s' + LineBreak +
                                        ' Numero Apolice : %s' + LineBreak +
                                        ' Tipo Movimento : %s' + LineBreak +
                                        ' Valor Averbado : %s' + LineBreak +
                                        ' Ramo Averbado  : %s' + LineBreak),
                         [ANeRetorno.Averbado.DadosSeguro.Items[0].NumeroAverbacao,
                          ANeRetorno.Averbado.DadosSeguro.Items[0].CNPJSeguradora,
                          ANeRetorno.Averbado.DadosSeguro.Items[0].NomeSeguradora,
                          ANeRetorno.Averbado.DadosSeguro.Items[0].NumApolice,
                          ANeRetorno.Averbado.DadosSeguro.Items[0].TpMov,
                          FloatToStr(ANeRetorno.Averbado.DadosSeguro.Items[0].ValorAverbado),
                          ANeRetorno.Averbado.DadosSeguro.Items[0].RamoAverbado]);
        end;
      end;
    end;
  end;

  Result := xMsg;
end;

function TANeAverbar.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService Averbar Documento:' + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

{ TANeEnvioWebService }

constructor TANeEnvioWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPStatus := stANeEnvioWebService;
end;

destructor TANeEnvioWebService.Destroy;
begin
  inherited Destroy;
end;

function TANeEnvioWebService.Executar: Boolean;
begin
  Result := inherited Executar;
end;

procedure TANeEnvioWebService.Clear;
begin
  inherited Clear;

  FVersao := '';
end;

procedure TANeEnvioWebService.DefinirURL;
begin
  FPURL := FPURLEnvio;
end;

procedure TANeEnvioWebService.DefinirServicoEAction;
begin
  FPServico := FPSoapAction;
end;

procedure TANeEnvioWebService.DefinirDadosMsg;
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

function TANeEnvioWebService.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'soap:Body');
  Result := True;
end;

function TANeEnvioWebService.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService: '+FPServico + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

function TANeEnvioWebService.GerarVersaoDadosSoap: String;
begin
  Result := '<versaoDados>' + FVersao + '</versaoDados>';
end;

{ TWebServices }

constructor TWebServices.Create(AOwner: TACBrDFe);
begin
  FACBrANe := TACBrANe(AOwner);

  FANeAverbar := TANeAverbar.Create(FACBrANe, TACBrANe(FACBrANe).Documentos);

  FEnvioWebService := TANeEnvioWebService.Create(FACBrANe);
end;

destructor TWebServices.Destroy;
begin
  FANeAverbar.Free;
  FEnvioWebService.Free;

  inherited Destroy;
end;

function TWebServices.Envia: Boolean;
begin
  if not ANeAverbar.Executar then
    ANeAverbar.GerarException( ANeAverbar.Msg );

  Result := True;
end;

end.
