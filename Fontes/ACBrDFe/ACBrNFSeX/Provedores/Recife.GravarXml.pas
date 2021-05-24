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

unit Recife.GravarXml;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ENDIF}
  SysUtils, Classes, StrUtils,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml_ABRASFv1, ACBrNFSeXConversao;

type
  { TNFSeW_Recife }

  TNFSeW_Recife = class(TNFSeW_ABRASFv1)
  protected
    procedure Configuracao; override;

  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     Recife
//==============================================================================

{ TNFSeW_Recife }

procedure TNFSeW_Recife.Configuracao;
begin
  // Executa a Configuração Padrão
  inherited Configuracao;

  // Altera a Configuração Padrão para gerar o XML do RPS
  NrOcorrValorPis := 1;
  NrOcorrValorCofins := 1;
  NrOcorrValorInss := 1;
  NrOcorrValorIr := 1;
  NrOcorrValorCsll := 1;
  NrOcorrValorIss := 1;
  NrOcorrAliquota := 0;
  NrOcorrValorTotalRecebido := 1;
end;
{
procedure TNFSeW_Recife.ConfigurarEnvelopes;
begin
  with ConfigEnvelope do
  begin
    with Recepcionar do
    begin
      IncluirEncodingDados := True;
      ElementoBody := 'RecepcionarLoteRpsRequest';
      AtributoElementoBody := ' xmlns="http://nfse.recife.pe.gov.br/"';
      ElementoCabecalho := '';
      ElementoDados := 'inputXML';
    end;

    with ConsultarSituacao do
    begin
      IncluirEncodingDados := True;
      ElementoBody := 'ConsultarSituacaoLoteRpsRequest';
      AtributoElementoBody := ' xmlns="http://nfse.recife.pe.gov.br/"';
      ElementoCabecalho := '';
      ElementoDados := 'inputXML';
    end;

    with ConsultarLote do
    begin
      IncluirEncodingDados := True;
      ElementoBody := 'ConsultarLoteRpsRequest';
      AtributoElementoBody := ' xmlns="http://nfse.recife.pe.gov.br/"';
      ElementoCabecalho := '';
      ElementoDados := 'inputXML';
    end;

    with ConsultarNFSePorRps do
    begin
      IncluirEncodingDados := True;
      ElementoBody := 'ConsultarNfsePorRpsRequest';
      AtributoElementoBody := ' xmlns="http://nfse.recife.pe.gov.br/"';
      ElementoCabecalho := '';
      ElementoDados := 'inputXML';
    end;

    with ConsultarNFSe do
    begin
      IncluirEncodingDados := True;
      ElementoBody := 'ConsultarNfseRequest';
      AtributoElementoBody := ' xmlns="http://nfse.recife.pe.gov.br/"';
      ElementoCabecalho := '';
      ElementoDados := 'inputXML';
    end;

    with Cancelar do
    begin
      IncluirEncodingDados := True;
      ElementoBody := 'CancelarNfseRequest';
      AtributoElementoBody := ' xmlns="http://nfse.recife.pe.gov.br/"';
      ElementoCabecalho := '';
      ElementoDados := 'inputXML';
    end;

    with GerarNFSe do
    begin
      IncluirEncodingDados := True;
      ElementoBody := 'GerarNfseRequest';
      AtributoElementoBody := ' xmlns="http://nfse.recife.pe.gov.br/"';
      ElementoCabecalho := '';
      ElementoDados := 'inputXML';
      ServicoImplementado := True;
    end;
  end;
end;

procedure TNFSeW_Recife.ConfigurarSoapAction;
var
  URL: string;
begin
  URL := 'http://nfse.recife.pe.gov.br/';

  with ConfigSoapAction do
  begin
    Recepcionar         := URL + 'RecepcionarLoteRps';
    ConsultarSituacao   := URL + 'ConsultarSituacaoLoteRps';
    ConsultarLote       := URL + 'ConsultarLoteRps';
    ConsultarNFSePorRps := URL + 'ConsultarNfsePorRps';
    ConsultarNFSe       := URL + 'ConsultarNfse';
    Cancelar            := URL + 'CancelarNfse';
    GerarNFSe           := URL + 'GerarNfse';
  end;
end;
}
end.
