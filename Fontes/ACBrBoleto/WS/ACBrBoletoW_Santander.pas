{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
{ Colaboradores nesse arquivo:  Renato Rubinho                                 }
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

unit ACBrBoletoW_Santander;

interface

uses
  Classes, SysUtils, DateUtils, Types, strutils,
  ACBrDFeConsts,
  pcnConversao, pcnGerador, synacode,
  ACBrBoletoWS,
  ACBrValidador,
  ACBrBoleto,
  ACBrBoletoConversao,
  ACBrutil.XMLHTML,
  ACBrBoletoPcnConsts,
  ACBrBoletoWS.SOAP;
  
type
  { TBoletoW_Santander }

  TBoletoW_Santander  = class(TBoletoWSSOAP)
  private

  protected
    procedure DefinirEnvelopeSoap; override;
    procedure DefinirRootElement; override;

    procedure GerarHeader; override;
    procedure GerarDados; override;

    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;

    procedure GerarTicket;

    function AmbienteWS: String;
  public
    constructor Create(ABoletoWS: TBoletoWS); override;

    function GerarRemessa: String; override;
    function Enviar: Boolean; override;
  end;

const
  C_TICKET_URL                  = 'https://ymbdlb.santander.com.br:443/dl-ticket-services/TicketEndpointService';

  C_COBRANCAV3_URL              = 'https://ymbcash.santander.com.br:443/ymbsrv/CobrancaV3EndpointService';

  C_BASE_SOAP_ATTRIBUTTES       = 'xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ';

  C_TICKET_SOAP_ATTRIBUTTES     = 'xmlns:impl="http://impl.webservice.dl.app.bsbr.altec.com/" '
                                   + C_BASE_SOAP_ATTRIBUTTES;

  C_COBRANCAV3_SOAP_ATTRIBUTTES = 'xmlns:impl="http://impl.webservice.v3.ymb.app.bsbr.altec.com/" '
                                   + C_BASE_SOAP_ATTRIBUTTES;

implementation

{ TBoletoW_Santander }

function TBoletoW_Santander.AmbienteWS: String;
begin
  Result := IfThen(ATitulo.ACBrBoleto.Configuracoes.WebService.Ambiente = taProducao,'P','T');
end;

constructor TBoletoW_Santander.Create(ABoletoWS: TBoletoWS);
begin
  inherited Create(ABoletoWS);
end;

procedure TBoletoW_Santander.DefinirEnvelopeSoap;
var Texto: String;
begin
  {$IFDEF FPC}
   Texto := '<' + ENCODING_UTF8 + '>';    // Envelope já está sendo montado em UTF8
  {$ELSE}
   Texto := '';  // Isso forçará a conversão para UTF8, antes do envio
  {$ENDIF}

  FPDadosMsg := RemoverDeclaracaoXML(FPDadosMsg);

  Texto := Texto + '<' + FPSoapVersion + ':Envelope ' + FPSoapEnvelopeAtributtes + '>';
  Texto := Texto + '<' + FPSoapVersion + ':Header/>';
  Texto := Texto + '<' + FPSoapVersion + ':Body>';
  Texto := Texto + FPDadosMsg;
  Texto := Texto + '</' + FPSoapVersion + ':Body>';
  Texto := Texto + '</' + FPSoapVersion + ':Envelope>';

  FPEnvelopeSoap := Texto;
  FPMimeType    := 'text/xml; charset=utf-8';
end;

procedure TBoletoW_Santander.DefinirURL;
begin
  FPURL := '';
  DefinirServicoEAction;
  FPVersaoServico := Boleto.Configuracoes.WebService.VersaoDF;
end;

procedure TBoletoW_Santander.DefinirServicoEAction;
var
  Acao: String;
begin
  FPURL := C_COBRANCAV3_URL;
  FPSoapEnvelopeAtributtes := C_COBRANCAV3_SOAP_ATTRIBUTTES;
  FPSoapAction  := 'INCLUSAO';

  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui:   Acao := TipoOperacaoToStr( tpInclui );
    tpTicket:
      begin
        Acao  := TipoOperacaoToStr( tpTicket );
        FPURL := C_TICKET_URL;
        FPSoapEnvelopeAtributtes := C_TICKET_SOAP_ATTRIBUTTES;
        FPSoapAction  := 'TICKET';
      end;
  end;

  FPServico := FPURL;
  FPSoapAction := Acao;
end;

procedure TBoletoW_Santander.DefinirRootElement;
begin
  FPRootElement := '';
  FPCloseRootElement := '';
end;

function TBoletoW_Santander.Enviar: Boolean;
begin
  Result := inherited Enviar;
end;

procedure TBoletoW_Santander.GerarDados;

  procedure CampoGrupo(key: string; value: variant; IDKey, IDValue: string; const min, max: smallint; Descricao: string; Tipo: TpcnTipoCampo = tcStr);
  begin
    Gerador.wGrupo('entry');
      Gerador.wCampo(tcStr, IDKey, 'key', 1, 256, 1, key, '');

      if key = 'PAGADOR.NUM-DOC' then
        Gerador.wCampoCNPJCPF(IDValue, IDValue, value)
      else
        Gerador.wCampo(Tipo, IDValue, 'value', min, max, 1, value, Descricao);

    Gerador.wGrupo('/entry');
  end;

begin
  if Assigned(ATitulo) then
  begin
    case Boleto.Configuracoes.WebService.Operacao of
      tpInclui:
        begin
          Gerador.wGrupo('impl:registraTitulo');
          Gerador.wGrupo('dto');

          Gerador.wCampo(tcStr, '#01', 'dtNsu', 08, 08, 1, FormatDateTime('ddmmyyyy',ATitulo.DataDocumento), '');
          Gerador.wCampo(tcStr, '#02', 'estacao', 01, 256, 1, ATitulo.ACBrBoleto.Cedente.CedenteWS.ClientID, '');
          Gerador.wCampo(tcStr, '#03', 'nsu', 01, 256, 1, ATitulo.NumeroDocumento + IntToStr(ATitulo.Parcela), '');

          GerarTicket;

          Gerador.wCampo(tcStr, '#05', 'tpAmbiente', 01, 01, 1, AmbienteWS, '');
          Gerador.wGrupo('/dto');
          Gerador.wGrupo('/impl:registraTitulo');
        end;
    end;
  end;
end;

procedure TBoletoW_Santander.GerarHeader;
begin

end;

function TBoletoW_Santander.GerarRemessa: String;
begin
  Result := inherited GerarRemessa;
end;

procedure TBoletoW_Santander.GerarTicket;
begin
  Gerador.wCampo(tcStr, '#04', 'ticket', 01, 256, 1, ATitulo.Mensagem.Text, 'Dados Ticket');
end;

end.

