{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{ Colaboradores nesse arquivo:  Victor Hugo Gonzales - Panda                   }
{                               Igless, Odeon                                  }
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

unit ACBrBoletoRet_Credisis;

interface

uses
  Classes, SysUtils, ACBrBoleto, ACBrBoletoWS, ACBrBoletoRetorno, pcnConversao;

type

{ TRetornoEnvio_Credisis }

  TRetornoEnvio_Credisis = class(TRetornoEnvioSOAP)
  private

  public
    constructor Create(ABoletoWS: TACBrBoleto); override;
    destructor  Destroy; Override;
    function LerRetorno: Boolean;override;
    function RetornoEnvio: Boolean; override;

  end;

  const
  C_URL_Retorno = 'SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ns1="urn:CredisisBoletoInterface-CredisisWebService"';

implementation

uses
  ACBrBoletoConversao, ACBrUtil.XMLHTML;

{ TRetornoEnvio_Credisis }

constructor TRetornoEnvio_Credisis.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);
end;

destructor TRetornoEnvio_Credisis.Destroy;
begin
  inherited Destroy;
end;

function TRetornoEnvio_Credisis.LerRetorno: Boolean;
var
    RetornoCredisis: TRetEnvio;
    lXML: String;
begin
    Result := True;

    lXML:= StringReplace(Leitor.Arquivo, 'ns1:', '', [rfReplaceAll]) ;
    lXML:= StringReplace(lXML, C_URL_Retorno, '', [rfReplaceAll]) ;
    Leitor.Arquivo := lXML;
    Leitor.Grupo   := Leitor.Arquivo;

    RetornoCredisis:= ACBrBoleto.CriarRetornoWebNaLista;
    try
      if leitor.rExtrai(1, 'gerarBoletosResponse') <> '' then
      begin
          RetornoCredisis.OriRetorno                         := Leitor.rCampo(tcStr, 'idWeb');
          RetornoCredisis.DadosRet.ControleNegocial.NSU      := Leitor.rCampo(tcStr, 'numeroSequencial');
          RetornoCredisis.DadosRet.IDBoleto.NossoNum         := Leitor.rCampo(tcStr, 'nossonumero');
          RetornoCredisis.DadosRet.IDBoleto.LinhaDig         := Leitor.rCampo(tcStr, 'linhaDigitavel');
          RetornoCredisis.DadosRet.IDBoleto.CodBarras        := Leitor.rCampo(tcStr, 'codigoBarras');
          RetornoCredisis.DadosRet.TituloRet.DataLimitePagto := Leitor.rCampo(tcDat, 'dataLimitePagamento');
          RetornoCredisis.CodRetorno                         := Leitor.rCampo(tcStr, 'code');
          RetornoCredisis.DadosRet.Excecao                   := Leitor.rCampo(tcStr, 'message');
      end else
      begin
        if (leitor.rExtrai(1, 'buscarBoletosResponse') <> '') and (leitor.rExtrai(2, 'item') <> '') then
        begin
          RetornoCredisis.OriRetorno                         := Leitor.rCampo(tcStr, 'idWeb');
          RetornoCredisis.DadosRet.TituloRet.Sacado.CNPJCPF  := Leitor.rCampo(tcStr, 'cpfCnpj');
          RetornoCredisis.DadosRet.IDBoleto.NossoNum         := Leitor.rCampo(tcStr, 'nossonumero');
          RetornoCredisis.DadosRet.TituloRet.NumeroDocumento := Leitor.rCampo(tcStr, 'documento');
          RetornoCredisis.DadosRet.TituloRet.Parcela         := Leitor.rCampo(tcInt, 'parcela');
        end;
      end;
    except
      Result := False;
    end;

  end;

function TRetornoEnvio_Credisis.RetornoEnvio: Boolean;
var
  lRetornoWS: String;
begin

  lRetornoWS := RetWS;
  RetWS := SeparaDados(lRetornoWS, 'SOAP-ENV:Body');

  Result:=inherited RetornoEnvio;

end;

end.

