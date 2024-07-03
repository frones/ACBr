{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit Fiorilli.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml, ACBrNFSeXProviderABRASFv2,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceFiorilli200 = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetDadosUsuario: string;
  public
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function RecepcionarSincrono(const ACabecalho, AMSG: String): string; override;
    function GerarNFSe(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoPrestado(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoTomado(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;

    property DadosUsuario: string read GetDadosUsuario;
  end;

  TACBrNFSeProviderFiorilli200 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure PrepararEmitir(Response: TNFSeEmiteResponse); override;
  end;

implementation

uses
  ACBrDFeException, ACBrUtil.Strings, ACBrUtil.XMLHTML,
  ACBrNFSeX, ACBrNFSeXConfiguracoes,
  ACBrNFSeXNotasFiscais, Fiorilli.GravarXml, Fiorilli.LerXml;

{ TACBrNFSeProviderFiorilli200 }

procedure TACBrNFSeProviderFiorilli200.Configuracao;
var
  NaoAssinar: Boolean;
begin
  inherited Configuracao;

  ConfigGeral.QuebradeLinha := '\s\n';
  ConfigGeral.ConsultaPorFaixaPreencherNumNfseFinal := true;

  ConfigGeral.Autenticacao.RequerLogin := True;

  NaoAssinar := ConfigGeral.Params.ParamTemValor('Assinar', 'NaoAssinar');

  if (ConfigAssinar.Assinaturas = taConfigProvedor) and not NaoAssinar then
  begin
    with ConfigAssinar do
    begin
      Rps := True;
      LoteRps := True;
      CancelarNFSe := True;
      RpsGerarNFSe := True;
      RpsSubstituirNFSe := True;
      SubstituirNFSe := True;
    end;
  end;
end;

function TACBrNFSeProviderFiorilli200.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Fiorilli200.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderFiorilli200.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Fiorilli200.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderFiorilli200.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceFiorilli200.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderFiorilli200.PrepararEmitir(
  Response: TNFSeEmiteResponse);
begin
  // O provedor Fiorilli exige que o numero do lote seja numerico e que não
  // não tenha zeros a esquerda.
  Response.NumeroLote := IntToStr(StrToIntDef(Trim(Response.NumeroLote), 0));

  inherited PrepararEmitir(Response);
end;

{ TACBrNFSeXWebserviceFiorilli200 }

function TACBrNFSeXWebserviceFiorilli200.GetDadosUsuario: string;
begin
  with TACBrNFSeX(FPDFeOwner).Configuracoes.Geral do
  begin
    Result := '<username>' + Emitente.WSUser + '</username>' +
              '<password>' + Emitente.WSSenha + '</password>';
  end;
end;

function TACBrNFSeXWebserviceFiorilli200.Recepcionar(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:recepcionarLoteRps>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</ws:recepcionarLoteRps>';

  Result := Executar('recepcionarLoteRps', Request,
                     ['EnviarLoteRpsResposta'],
                     ['xmlns:ws="http://ws.issweb.fiorilli.com.br/"']);
end;

function TACBrNFSeXWebserviceFiorilli200.RecepcionarSincrono(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:recepcionarLoteRpsSincrono>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</ws:recepcionarLoteRpsSincrono>';

  Result := Executar('recepcionarLoteRpsSincrono', Request,
                     ['EnviarLoteRpsSincronoResposta'],
                     ['xmlns:ws="http://ws.issweb.fiorilli.com.br/"']);
end;

function TACBrNFSeXWebserviceFiorilli200.GerarNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:gerarNfse>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</ws:gerarNfse>';

  Result := Executar('gerarNfse', Request,
                     ['GerarNfseResposta'],
                     ['xmlns:ws="http://ws.issweb.fiorilli.com.br/"']);
end;

function TACBrNFSeXWebserviceFiorilli200.ConsultarLote(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:consultarLoteRps>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</ws:consultarLoteRps>';

  Result := Executar('consultarLoteRps', Request,
                     ['ConsultarLoteRpsResposta'],
                     ['xmlns:ws="http://ws.issweb.fiorilli.com.br/"']);
end;

function TACBrNFSeXWebserviceFiorilli200.ConsultarNFSePorFaixa(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:consultarNfsePorFaixa>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</ws:consultarNfsePorFaixa>';

  Result := Executar('consultarNfsePorFaixa', Request,
                     ['ConsultarNfseFaixaResposta'],
                     ['xmlns:ws="http://ws.issweb.fiorilli.com.br/"']);
end;

function TACBrNFSeXWebserviceFiorilli200.ConsultarNFSePorRps(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:consultarNfsePorRps>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</ws:consultarNfsePorRps>';

  Result := Executar('consultarNfsePorRps', Request,
                     ['ConsultarNfseRpsResposta'],
                     ['xmlns:ws="http://ws.issweb.fiorilli.com.br/"']);
end;

function TACBrNFSeXWebserviceFiorilli200.ConsultarNFSeServicoPrestado(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:consultarNfseServicoPrestado>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</ws:consultarNfseServicoPrestado>';

  Result := Executar('consultarNfseServicoPrestado', Request,
                     ['ConsultarNfseServicoPrestadoResposta'],
                     ['xmlns:ws="http://ws.issweb.fiorilli.com.br/"']);
end;

function TACBrNFSeXWebserviceFiorilli200.ConsultarNFSeServicoTomado(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:consultarNfseServicoTomado>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</ws:consultarNfseServicoTomado>';

  Result := Executar('consultarNfseServicoTomado', Request,
                     ['ConsultarNfseServicoTomadoResposta'],
                     ['xmlns:ws="http://ws.issweb.fiorilli.com.br/"']);
end;

function TACBrNFSeXWebserviceFiorilli200.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:cancelarNfse>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</ws:cancelarNfse>';

  Result := Executar('cancelarNfse', Request,
                     ['CancelarNfseResposta'],
                     ['xmlns:ws="http://ws.issweb.fiorilli.com.br/"']);
end;

function TACBrNFSeXWebserviceFiorilli200.SubstituirNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:substituirNfse>';
  Request := Request + AMSG;
  Request := Request + DadosUsuario;
  Request := Request + '</ws:substituirNfse>';

  Result := Executar('substituirNfse', Request,
                     ['SubstituirNfseResposta'],
                     ['xmlns:ws="http://ws.issweb.fiorilli.com.br/"']);
end;

function TACBrNFSeXWebserviceFiorilli200.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := ConverteANSIparaUTF8(aXml);
  Result := RemoverDeclaracaoXML(Result);

  Result := inherited TratarXmlRetornado(Result);

  Result := StringReplace(Result, '&#xd;', '\s\n', [rfReplaceAll]);
  Result := StringReplace(Result, ''#$A'', '\s\n', [rfReplaceAll]);
  Result := ParseText(Result);
  Result := RemoverPrefixosDesnecessarios(Result);
  Result := RemoverCaracteresDesnecessarios(Result);
  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
end;

end.
