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

unit EloTech.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceEloTech = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetRequerente: string;
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function RecepcionarSincrono(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoPrestado(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoTomado(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(ACabecalho, AMSG: String): string; override;

    property Requerente: string read GetRequerente;
  end;

  TACBrNFSeProviderEloTech = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

implementation

uses
  ACBrUtil, ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes,
  ACBrNFSeXNotasFiscais, EloTech.GravarXml, EloTech.LerXml;

{ TACBrNFSeProviderEloTech }

procedure TACBrNFSeProviderEloTech.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.UseCertificateHTTP := False;

  with ConfigWebServices do
  begin
    VersaoDados := '2.03';
    VersaoAtrib := '2.03';
  end;

  SetXmlNameSpace('http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd');

  with ConfigMsgDados do
  begin
    with LoteRps do
    begin
//      GerarID := False;
    end;

    with ConsultarLote do
    begin
//      NrOcorrNumLote   := 1;
//      NrOcorrProtocolo := -1;
    end;

    with ConsultarNFSeRps do
    begin
//      GerarGrupoRequerente := True;
//      GerarGrupoPrestador  := False;
    end;

    with ConsultarNFSe do
    begin
//      GerarGrupoRequerente := True;
//      GerarGrupoPrestador  := False;
    end;

    with CancelarNFSe do
    begin
//      GerarIDInfPedCanc  := False;
//      NrOcorrChaveAcesso := -1;
    end;

    with SubstituirNFSe do
    begin
//      GerarIDSubstituicaoNFSe := False;
//      GerarGrupoCPFCNPJ       := True;
    end;
  end;

  SetNomeXSD('nfse_v2_03.xsd');
//  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProviderEloTech.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_EloTech.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderEloTech.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_EloTech.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderEloTech.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceEloTech.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceEloTech.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceEloTech.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebserviceEloTech.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmConsultarNFSeServicoPrestado:
          Result := TACBrNFSeXWebserviceEloTech.Create(FAOwner, AMetodo, ConsultarNFSeServicoPrestado);
        tmConsultarNFSeServicoTomado:
          Result := TACBrNFSeXWebserviceEloTech.Create(FAOwner, AMetodo, ConsultarNFSeServicoTomado);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceEloTech.Create(FAOwner, AMetodo, CancelarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceEloTech.Create(FAOwner, AMetodo, RecepcionarSincrono);
        tmSubstituirNFSe:
          Result := TACBrNFSeXWebserviceEloTech.Create(FAOwner, AMetodo, SubstituirNFSe);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end
  else
  begin
    with ConfigWebServices.Producao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceEloTech.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceEloTech.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceEloTech.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebserviceEloTech.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmConsultarNFSeServicoPrestado:
          Result := TACBrNFSeXWebserviceEloTech.Create(FAOwner, AMetodo, ConsultarNFSeServicoPrestado);
        tmConsultarNFSeServicoTomado:
          Result := TACBrNFSeXWebserviceEloTech.Create(FAOwner, AMetodo, ConsultarNFSeServicoTomado);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceEloTech.Create(FAOwner, AMetodo, CancelarNFSe);
        tmRecepcionarSincrono:
          Result := TACBrNFSeXWebserviceEloTech.Create(FAOwner, AMetodo, RecepcionarSincrono);
        tmSubstituirNFSe:
          Result := TACBrNFSeXWebserviceEloTech.Create(FAOwner, AMetodo, SubstituirNFSe);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end;
end;

{ TACBrNFSeXWebserviceEloTech }

function TACBrNFSeXWebserviceEloTech.GetRequerente: string;
var
  xRequerente: string;
  Homologacao: Boolean;
begin
  Homologacao := (FPConfiguracoes.WebServices.AmbienteCodigo = 2);

  with TACBrNFSeX(FPDFeOwner).Configuracoes.Geral do
  begin
    xRequerente := '<IdentificacaoRequerente>' +
                     '<CpfCnpj>' +
                       '<Cnpj>' + Emitente.CNPJ + '</Cnpf>' +
                     '</CpfCnpj>' +
                     '<InscricaoMunicipal>' +
                        Emitente.InscMun +
                     '</InscricaoMunicipal>' +
                     '<Senha>' + Emitente.WSSenha + '</Senha>' +
                     '<Homologa>' +
                        LowerCase(booltostr(Homologacao, True)) +
                     '</Homologa>';
  end;
end;

function TACBrNFSeXWebserviceEloTech.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
  i: Integer;
begin
  i := Pos('<LoteRps', AMSG);

  if i > 0 then
  begin
    Request := Copy(AMSG, 1, i -2) + Requerente + Copy(AMSG, i -1, Length(AMSG));
  end;

  FPMsgOrig := AMSG;

  Result := Executar('', Request,
                     ['return', 'outputXML', 'EnviarLoteRpsResposta'],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
  i: Integer;
begin
  i := Pos('<LoteRps', AMSG);

  if i > 0 then
  begin
    Request := Copy(AMSG, 1, i -2) + Requerente + Copy(AMSG, i -1, Length(AMSG));
  end;

  FPMsgOrig := AMSG;

  Result := Executar('', Request,
                     ['return', 'outputXML', 'EnviarLoteRpsSincronoResposta'],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
  i: Integer;
begin
  i := Pos('<NumeroLote', AMSG);

  if i > 0 then
  begin
    Request := Copy(AMSG, 1, i -2) + Requerente + Copy(AMSG, i -1, Length(AMSG));
  end;

  FPMsgOrig := AMSG;

  Result := Executar('', Request,
                     ['return', 'outputXML', 'ConsultarLoteRpsResposta'],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
  i: Integer;
begin
  i := Pos('<Prestador', AMSG);

  // Remove o grupo <Prestador> e coloca no lugar o <IdentificacaoRequerente>
  if i > 0 then
  begin
    Request := Copy(AMSG, 1, i -2) + Requerente;

    i := Pos('</Prestador', AMSG);

    Request := Request + Copy(AMSG, i +11, Length(AMSG));
  end;

  FPMsgOrig := AMSG;

  Result := Executar('', Request,
                     ['return', 'outputXML', 'ConsultarNfsePorFaixaResposta'],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
  i: Integer;
begin
  i := Pos('Prestador', AMSG);

  // Remove o grupo <Prestador> e coloca no lugar o <IdentificacaoRequerente>
  if i > 0 then
  begin
    Request := Copy(AMSG, 1, i -2) + Requerente + '</ConsultarNfseRpsEnvio>';
  end;

  FPMsgOrig := AMSG;

  Result := Executar('', Request,
                     ['return', 'outputXML', 'ConsultarNfseRpsResposta'],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
  i: Integer;
begin
  i := Pos('<Prestador', AMSG);

  // Remove o grupo <Prestador> e coloca no lugar o <IdentificacaoRequerente>
  if i > 0 then
  begin
    Request := Copy(AMSG, 1, i -2) + Requerente;

    i := Pos('</Prestador', AMSG);

    Request := Request + Copy(AMSG, i +11, Length(AMSG));
  end;

  FPMsgOrig := AMSG;

  Result := Executar('', Request,
                     ['return', 'outputXML', 'ConsultarNfseServicoPrestadoResposta'],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
  i: Integer;
begin
  i := Pos('<Prestador', AMSG);

  // Remove o grupo <Prestador> e coloca no lugar o <IdentificacaoRequerente>
  if i > 0 then
  begin
    Request := Copy(AMSG, 1, i -2) + Requerente;

    i := Pos('</Prestador', AMSG);

    Request := Request + Copy(AMSG, i +11, Length(AMSG));
  end;

  FPMsgOrig := AMSG;

  Result := Executar('', Request,
                     ['return', 'outputXML', 'ConsultarNfseServicoTomadoResposta'],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
  i: Integer;
begin
  i := Pos('Pedido', AMSG);

  if i > 0 then
  begin
    Request := Copy(AMSG, 1, i -2) + Requerente + Copy(AMSG, i -1, Length(AMSG));
  end;

  FPMsgOrig := AMSG;

  Result := Executar('', Request,
                     ['return', 'outputXML', 'CancelarNfseResposta'],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
  i: Integer;
begin
  i := Pos('Pedido', AMSG);

  if i > 0 then
  begin
    Request := Copy(AMSG, 1, i -2) + Requerente + Copy(AMSG, i -1, Length(AMSG));
  end;

  FPMsgOrig := AMSG;

  Result := Executar('', Request,
                     ['return', 'outputXML', 'SubstituirNfseResposta'],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

end.
