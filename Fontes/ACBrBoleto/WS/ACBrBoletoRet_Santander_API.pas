{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
{ Colaboradores nesse arquivo:  Jéter Rabelo Ferreira                          }
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

unit ACBrBoletoRet_Santander_API;

interface

uses
  Classes,
  SysUtils,
  ACBrBoleto,
  ACBrBoletoWS,
  ACBrBoletoRetorno,
  Jsons,
  DateUtils,
  ACBrBoletoWS.Rest,
  pcnConversao;

type
  { TRetornoEnvio_Santander }

  TRetornoEnvio_Santander_API = class(TRetornoEnvioREST)
  private

  public
    constructor Create(ABoletoWS: TACBrBoleto); override;
    destructor  Destroy; Override;
    function LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean; override;
    function RetornoEnvio(const AIndex: Integer): Boolean; override;
  end;


implementation

uses
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrBoletoConversao;

{ TRetornoEnvio_Santander_API }

constructor TRetornoEnvio_Santander_API.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);
end;

destructor TRetornoEnvio_Santander_API.Destroy;
begin
  inherited Destroy;
end;

function TRetornoEnvio_Santander_API.LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;
var
  AJson: TJson;
  AJSonRejeicao: TJsonObject;
  ARejeicao: TACBrBoletoRejeicao;
  AJSonResp: TJsonArray;
  settlementData: TJsonObject;
  I: Integer;
  TipoOperacao : TOperacao;
begin
  Result := True;
  TipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;
  ARetornoWs.JSONEnvio      := EnvWs;
  ARetornoWS.HTTPResultCode := HTTPResultCode;
  ARetornoWS.Header.Operacao := TipoOperacao;
  if RetWS <> '' then
  begin
    try
      AJSon := TJson.Create;

      try

        AJSon.Parse(RetWS);
        ARetornoWS.JSON := AJson.Stringify;


         case TipoOperacao of
           tpInclui:
            begin
              case HTTPResultCode of
                  200,201 :
                  begin
                      ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF               := AJson.Values['payer'].AsObject.Values['documentNumber'].AsString;
                      ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado            := AJson.Values['payer'].AsObject.Values['name'].AsString;
                      ARetornoWS.DadosRet.TituloRet.Sacado.Logradouro            := AJson.Values['payer'].AsObject.Values['address'].AsString;
                      ARetornoWS.DadosRet.TituloRet.Sacado.Bairro                := AJson.Values['payer'].AsObject.Values['neighborhood'].AsString;
                      ARetornoWS.DadosRet.TituloRet.Sacado.Cidade                := AJson.Values['payer'].AsObject.Values['city'].AsString;
                      ARetornoWS.DadosRet.TituloRet.Sacado.UF                    := AJson.Values['payer'].AsObject.Values['state'].AsString;
                      ARetornoWS.DadosRet.TituloRet.Sacado.Cep                   := AJson.Values['payer'].AsObject.Values['zipCode'].AsString;

                      ARetornoWS.DadosRet.TituloRet.SacadoAvalista.CNPJCPF       := AJson.Values['beneficiary'].AsObject.Values['documentNumber'].AsString;
                      ARetornoWS.DadosRet.TituloRet.SacadoAvalista.NomeAvalista  := AJson.Values['beneficiary'].AsObject.Values['name'].AsString;


                      ARetornoWS.DadosRet.IDBoleto.CodBarras                     := AJson.Values['barCode'].AsString;
                      ARetornoWS.DadosRet.IDBoleto.LinhaDig                      := AJson.Values['digitableLine'].AsString;
                      ARetornoWS.DadosRet.IDBoleto.NossoNum                      := AJson.Values['bankNumber'].AsString;

                      ARetornoWS.DadosRet.TituloRet.Vencimento                   := StringToDateTimeDef(AJson.Values['dueDate'].AsString, 0, 'yyyy-mm-dd');
                      ARetornoWS.DadosRet.TituloRet.NossoNumero                  := AJson.Values['bankNumber'].AsString;
                      ARetornoWS.DadosRet.TituloRet.SeuNumero                    := AJson.Values['clientNumber'].AsString;
                      ARetornoWS.DadosRet.TituloRet.CodBarras                    := AJson.Values['barCode'].AsString;
                      ARetornoWS.DadosRet.TituloRet.LinhaDig                     := AJson.Values['digitableLine'].AsString;
                      ARetornoWS.DadosRet.TituloRet.DataProcessamento            := StringToDateTimeDef(AJson.Values['entryDate'].AsString, 0, 'yyyy-mm-dd');
                      ARetornoWS.DadosRet.TituloRet.DataDocumento                :=  StringToDateTimeDef(AJson.Values['issueDate'].AsString, 0, 'yyyy-mm-dd');
                      ARetornoWS.DadosRet.TituloRet.ValorDocumento               := StrToFloatDef( AJson.Values['nominalValue'].AsString, 0);
                      ARetornoWS.DadosRet.TituloRet.EMV                          := AJson.Values['qrCodePix'].AsString;
                      ARetornoWS.DadosRet.TituloRet.UrlPix                       := AJson.Values['qrCodeUrl'].AsString;
                      ARetornoWS.DadosRet.TituloRet.TxId                         := AJson.Values['txId'].AsString;


                  end;
                   400 :
                  begin
                     AJSonResp := AJson.Values['_errors'].AsArray;
                     if AJSonResp.Count > 0  then
                      begin
                        ARetornoWS.CodRetorno := AJson.Values['_errorCode'].AsString;
                        ARetornoWS.MsgRetorno := AJson.Values['_message'].AsString;

                        for I := 0 to Pred(AJSonResp.Count) do
                        begin
                          AJSonRejeicao      := AJSonResp[I].AsObject;
                          ARejeicao          := ARetornoWS.CriarRejeicaoLista;
                          ARejeicao.Codigo   := AJson.Values['_errorCode'].AsString;
                          ARejeicao.Campo    := AJSonRejeicao.Values['_code'].AsString;
                          ARejeicao.Mensagem := AJSonRejeicao.Values['_message'].AsString;
                          ARejeicao.Valor    := AJSonRejeicao.Values['_field'].AsString;
                        end;

                      end;
                  end;

              end;
            end;
            tpBaixa,tpAltera:
            begin
              case HTTPResultCode of

                   400 :
                  begin
                     AJSonResp := AJson.Values['_errors'].AsArray;
                     if AJSonResp.Count > 0  then
                      begin
                        ARetornoWS.CodRetorno := AJson.Values['_errorCode'].AsString;
                        ARetornoWS.MsgRetorno := AJson.Values['_message'].AsString;

                        for I := 0 to Pred(AJSonResp.Count) do
                        begin
                          AJSonRejeicao      := AJSonResp[I].AsObject;
                          ARejeicao          := ARetornoWS.CriarRejeicaoLista;
                          ARejeicao.Codigo   := AJson.Values['_errorCode'].AsString;
                          ARejeicao.Campo    := AJSonRejeicao.Values['_code'].AsString;
                          ARejeicao.Mensagem := AJSonRejeicao.Values['_message'].AsString;
                          ARejeicao.Valor    := AJSonRejeicao.Values['_field'].AsString;
                        end;

                      end;
                  end;

              end;
            end;
            tpConsultaDetalhe :
            begin
               case HTTPResultCode of
                  200 :
                  begin

                    ARetornoWS.DadosRet.TituloRet.NossoNumero          := AJson.Values['bankNumber'].AsString;
                    ARetornoWS.DadosRet.TituloRet.SeuNumero            := AJson.Values['clientNumber'].AsString;
                    ARetornoWS.DadosRet.TituloRet.Vencimento           := StringToDateTimeDef(AJson.Values['dueDate'].AsString, 0, 'yyyy-mm-dd');
                    ARetornoWS.DadosRet.TituloRet.DataDocumento        := StringToDateTimeDef(AJson.Values['issueDate'].AsString, 0, 'yyyy-mm-dd');
                    ARetornoWS.DadosRet.TituloRet.ValorDocumento       := AJson.Values['nominalValue'].AsNumber;
                    ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := AJson.Values['status'].AsString;



                      case ACBrBoleto.Configuracoes.WebService.Filtro.indicadorSituacao of
                      isbBaixado: //settlement: Pesquisa para informações de baixas/liquidações do boleto
                        begin
                           //settlementData
                            AJSonResp := AJson.Values['settlementData'].AsArray;
                           if AJSonResp.Count > 0  then
                            begin

                              for I := 0 to Pred(AJSonResp.Count) do
                              begin
                                settlementData      := AJSonResp[I].AsObject;
                                ARetornoWS.DadosRet.TituloRet.DataBaixa              :=  StringToDateTimeDef(settlementData.Values['settlementDate'].AsString, 0, 'yyyy-mm-dd');
                                ARetornoWS.DadosRet.TituloRet.ValorMoraJuros         :=  settlementData.Values['interestValue'].AsNumber;
                                ARetornoWS.DadosRet.TituloRet.ValorDesconto          :=  settlementData.Values['discountValue'].AsNumber;
                                ARetornoWS.DadosRet.TituloRet.ValorPago              :=  settlementData.Values['settlementValue'].AsNumber;
                                ARetornoWS.DadosRet.TituloRet.ValorIOF               :=  settlementData.Values['settlementIofValue'].AsNumber;
                                ARetornoWS.DadosRet.TituloRet.ValorOutrasDespesas    :=  settlementData.Values['otherValues'].AsNumber;
                                ARetornoWS.DadosRet.TituloRet.ValorAbatimento        :=  settlementData.Values['deductionValue'].AsNumber;
                                ARetornoWS.DadosRet.TituloRet.DataCredito            :=  StringToDateTimeDef(settlementData.Values['settlementCreditDate'].AsString, 0, 'yyyy-mm-dd');
                                ARetornoWS.DadosRet.TituloRet.ValorRecebido          :=  settlementData.Values['settlementCreditedValue'].AsNumber;
                                ARetornoWS.DadosRet.TituloRet.ValorDespesaCobranca   :=  settlementData.Values['settlementDutyValue'].AsNumber;

                              end;

                            end;

                        end;
                      isbAberto:// bankslip: Pesquisa para dados completos do boleto
                        begin

                               //bankSlipData
                             ARetornoWS.DadosRet.TituloRet.DataProcessamento     := StringToDateTimeDef(AJson.Values['bankSlipData'].AsObject.Values['processingDate'].AsString, 0, 'yyyy-mm-dd');
                             ARetornoWS.DadosRet.TituloRet.DataRegistro          := StringToDateTimeDef(AJson.Values['bankSlipData'].AsObject.Values['entryDate'].AsString, 0, 'yyyy-mm-dd');
                             ARetornoWS.DadosRet.TituloRet.DiasDeProtesto        := AJson.Values['bankSlipData'].AsObject.Values['protestQuantityDays'].AsInteger;
                             ARetornoWS.DadosRet.TituloRet.QtdeParcelas          := AJson.Values['bankSlipData'].AsObject.Values['parcelsQuantity'].AsInteger;
                             ARetornoWS.DadosRet.TituloRet.QtdePagamentoParcial  := AJson.Values['bankSlipData'].AsObject.Values['paidParcelsQuantity'].AsInteger;
                             ARetornoWS.DadosRet.TituloRet.ValorRecebido         := AJson.Values['bankSlipData'].AsObject.Values['amountReceived'].AsNumber;
                             ARetornoWS.DadosRet.TituloRet.ValorMoraJuros        := AJson.Values['bankSlipData'].AsObject.Values['interestPercentage'].AsNumber;
                             ARetornoWS.DadosRet.TituloRet.LinhaDig              := AJson.Values['bankSlipData'].AsObject.Values['digitableLine'].AsString;
                             ARetornoWS.DadosRet.TituloRet.CodBarras             := AJson.Values['bankSlipData'].AsObject.Values['barCode'].AsString;

                                // payerData
                             ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF    := AJson.Values['payerData'].AsObject.Values['payerDocumentNumber'].AsString;
                             ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado := AJson.Values['payerData'].AsObject.Values['payerName'].AsString;
                             ARetornoWS.DadosRet.TituloRet.Sacado.Logradouro := AJson.Values['payerData'].AsObject.Values['payerAddress'].AsString;
                             ARetornoWS.DadosRet.TituloRet.Sacado.Bairro     := AJson.Values['payerData'].AsObject.Values['payerNeighborhood'].AsString;
                             ARetornoWS.DadosRet.TituloRet.Sacado.Cidade     := AJson.Values['payerData'].AsObject.Values['payerCounty'].AsString;
                             ARetornoWS.DadosRet.TituloRet.Sacado.UF         := AJson.Values['payerData'].AsObject.Values['payerStateAbbreviation'].AsString;
                             ARetornoWS.DadosRet.TituloRet.Sacado.Cep        := AJson.Values['payerData'].AsObject.Values['payerZipCode'].AsString;



                        end;
                      end;
					  
                  end;
                  400 :
                  begin
                     AJSonResp := AJson.Values['_errors'].AsArray;
                     if AJSonResp.Count > 0  then
                      begin
                        ARetornoWS.CodRetorno := AJson.Values['_errorCode'].AsString;
                        ARetornoWS.MsgRetorno := AJson.Values['_message'].AsString;

                        for I := 0 to Pred(AJSonResp.Count) do
                        begin
                          AJSonRejeicao      := AJSonResp[I].AsObject;
                          ARejeicao          := ARetornoWS.CriarRejeicaoLista;
                          ARejeicao.Codigo   := AJson.Values['_errorCode'].AsString;
                          ARejeicao.Campo    := AJSonRejeicao.Values['_code'].AsString;
                          ARejeicao.Mensagem := AJSonRejeicao.Values['_message'].AsString;
                          ARejeicao.Valor    := AJSonRejeicao.Values['_field'].AsString;
                        end;

                      end;
                  end;

               end;

            end;


         end;


      finally
       AJson.free;
      end;

    Except
      Result := False;
    end;


  end
  else
  begin
    case TipoOperacao of
      tpInclui,
      tpBaixa,
      tpAltera,
      tpConsulta,
      tpConsultaDetalhe:
        begin
          case HTTPResultCode of
              401 :
              begin
                ARejeicao            := ARetornoWS.CriarRejeicaoLista;
                ARejeicao.Codigo     := '401';
                ARejeicao.Mensagem   := 'Não autorizado/Autenticado';
                ARejeicao.Ocorrencia := '401 - Não autorizado/Autenticado';
              end;
              403 :
              begin
                ARejeicao            := ARetornoWS.CriarRejeicaoLista;
                ARejeicao.Codigo     := '403';
                ARejeicao.Mensagem   := 'Não Autorizado';
                ARejeicao.Ocorrencia := '403 - Não Autorizado';
              end;
              404 :
              begin
                ARejeicao            := ARetornoWS.CriarRejeicaoLista;
                ARejeicao.Codigo     := '404';
                ARejeicao.Mensagem   := 'Informação não encontrada';
                ARejeicao.Ocorrencia := '404 - Informação não encontrada';
              end;
              406 :
              begin
                ARejeicao            := ARetornoWS.CriarRejeicaoLista;
                ARejeicao.Codigo     := '406';
                ARejeicao.Mensagem   := 'O recurso de destino não possui uma representação atual que seria aceitável';
                ARejeicao.Ocorrencia := '406 - O recurso de destino não possui uma representação atual que seria aceitável';
              end;
              500 :
              begin
                ARejeicao            := ARetornoWS.CriarRejeicaoLista;
                ARejeicao.Codigo     := '500';
                ARejeicao.Mensagem   := 'Erro de Servidor, Aplicação está fora';
                ARejeicao.Ocorrencia := '500 - Erro de Servidor, Aplicação está fora';
              end;
              501 :
              begin
                ARejeicao            := ARetornoWS.CriarRejeicaoLista;
                ARejeicao.Codigo     := '501';
                ARejeicao.Mensagem   := 'Erro de Servidor, Aplicação está fora';
                ARejeicao.Ocorrencia := '501 - O servidor não oferece suporte à funcionalidade necessária para atender à solicitação';
              end;
          end;
        end;
    end;


  end;
end;
function TRetornoEnvio_Santander_API.RetornoEnvio(const AIndex: Integer): Boolean;
begin
  Result:=inherited RetornoEnvio(AIndex);
end;

end.

