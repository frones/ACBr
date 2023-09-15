{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  José M S Junior                                }
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

unit ACBrBoletoRet_BancoBrasil;

interface

uses
  Classes,
  SysUtils,
  ACBrBoleto,
  ACBrBoletoWS,
  ACBrBoletoRetorno,
  ACBrUtil,
  DateUtils,
  pcnConversao,
  ACBrBoletoWS.SOAP;

type

{ TRetornoEnvio_BancoBrasil }

  TRetornoEnvio_BancoBrasil = class(TRetornoEnvioSOAP)
  private

  public
    constructor Create(ABoletoWS: TACBrBoleto); override;
    destructor  Destroy; Override;
    function LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;override;
    function RetornoEnvio(const AIndex: Integer): Boolean; override;

  end;

  const
  C_URL_Retorno = ' xmlns:ns0="http://www.tibco.com/schemas/bws_registro_cbr/Recursos/XSD/Schema.xsd"';

implementation

uses
  ACBrBoletoConversao, ACBrUtil.XMLHTML;

{ TRetornoEnvio_BancoBrasil }

constructor TRetornoEnvio_BancoBrasil.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);
end;

destructor TRetornoEnvio_BancoBrasil.Destroy;
begin
  inherited Destroy;
end;

function TRetornoEnvio_BancoBrasil.LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;
var
    //RetornoBB: TACBrBoletoRetornoWS;
    lXML: String;
    TipoOperacao : TOperacao;
begin
    Result := True;

    lXML:= StringReplace(Leitor.Arquivo, 'ns0:', '', [rfReplaceAll]) ;
    lXML:= StringReplace(lXML, C_URL_Retorno, '', [rfReplaceAll]) ;
    Leitor.Arquivo := lXML;
    Leitor.Grupo := Leitor.Arquivo;

    //RetornoBB:= ACBrBoleto.CriarRetornoWebNaLista;
    try
      with ARetornoWS do
      begin
        if leitor.rExtrai(1, 'resposta') <> '' then
        begin
          TipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;
          ARetornoWS.HTTPResultCode  := HTTPResultCode;
          ARetornoWS.JSONEnvio       := EnvWs;
          ARetornoWS.Header.Operacao := TipoOperacao;
          CodRetorno := Leitor.rCampo(tcStr, 'codigoRetornoPrograma');
          OriRetorno := Leitor.rCampo(tcStr, 'nomeProgramaErro');
          MsgRetorno := Leitor.rCampo(tcStr, 'textoMensagemErro');

          with DadosRet do
          begin
            Excecao := Leitor.rCampo(tcStr, 'numeroPosicaoErroPrograma');

            with ControleNegocial do
            begin
              OriRetorno := Leitor.rCampo(tcStr, 'codigoCliente');
              CodRetorno := Leitor.rCampo(tcStr, 'numeroContratoCobranca');
              NSU        := Leitor.rCampo(tcStr, '');

            end;

            DadosRet.IDBoleto.CodBarras := Leitor.rCampo(tcStr, 'codigoBarraNumerico');
            DadosRet.IDBoleto.LinhaDig  := Leitor.rCampo(tcStr, 'linhaDigitavel');
            DadosRet.IDBoleto.NossoNum  := Leitor.rCampo(tcStr, 'textoNumeroTITULOCobrancaBb');

          end;
        end;
      end;
    except
      Result := False;
    end;

  end;

function TRetornoEnvio_BancoBrasil.RetornoEnvio(const AIndex: Integer): Boolean;
var
  lRetornoWS: String;
begin

  lRetornoWS := RetWS;
  RetWS := SeparaDados(lRetornoWS, 'SOAP-ENV:Body');

  Result:=inherited RetornoEnvio(AIndex);

end;

end.

