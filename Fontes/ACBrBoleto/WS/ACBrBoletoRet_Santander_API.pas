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
  ACBrJSON,
  DateUtils,
  ACBrBoletoWS.Rest;

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
  ACBrBoletoConversao,
  StrUtils, ACBrUtil.Base;

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
  LJsonArray, LJsonErrorArray: TACBrJSONArray;
  LJsonObject : TACBrJSONObject;
  LRejeicao: TACBrBoletoRejeicao;
  I: Integer;
  TipoOperacao : TOperacao;
  LTotalObjetos, nIndiceOBJ : integer;
  LExisteBankSlip : boolean;

begin
  Result := True;
  LExisteBankSlip := false;

  TipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;

  ARetornoWs.JSONEnvio       := EnvWs;
  ARetornoWS.HTTPResultCode  := HTTPResultCode;
  ARetornoWS.Header.Operacao := TipoOperacao;

  if RetWS <> '' then
  begin
    LJsonArray := TACBrJSONArray.Parse(RetWS);
    try
      try
        ARetornoWS.JSON := LJsonArray.ToJSON;
        if HTTPResultCode < 300 then
        begin
          case TipoOperacao of
            tpInclui:
              begin
                case HTTPResultCode of
                  200,
                  201 :
                  begin
                    LJSONObject := LJsonArray.ItemAsJSONObject[0];

                    ARetornoWS.DadosRet.IDBoleto.CodBarras                     := LJSONObject.AsString['barCode'];
                    ARetornoWS.DadosRet.IDBoleto.LinhaDig                      := LJSONObject.AsString['digitableLine'];
                    ARetornoWS.DadosRet.IDBoleto.NossoNum                      := LJSONObject.AsString['bankNumber'];

                    ARetornoWS.DadosRet.TituloRet.Vencimento                   := StringToDateTimeDef(LJSONObject.AsString['dueDate'], 0, 'yyyy-mm-dd');
                    ARetornoWS.DadosRet.TituloRet.NossoNumero                  := LJSONObject.AsString['bankNumber'];
                    ARetornoWS.DadosRet.TituloRet.SeuNumero                    := LJSONObject.AsString['clientNumber'];
                    ARetornoWS.DadosRet.TituloRet.CodBarras                    := LJSONObject.AsString['barCode'];
                    ARetornoWS.DadosRet.TituloRet.LinhaDig                     := LJSONObject.AsString['digitableLine'];
                    ARetornoWS.DadosRet.TituloRet.DataProcessamento            := StringToDateTimeDef(LJSONObject.AsString['entryDate'], 0, 'yyyy-mm-dd');
                    ARetornoWS.DadosRet.TituloRet.DataDocumento                :=  StringToDateTimeDef(LJSONObject.AsString['issueDate'], 0, 'yyyy-mm-dd');
                    ARetornoWS.DadosRet.TituloRet.ValorDocumento               := StrToFloatDef( LJSONObject.AsString['nominalValue'], 0);
                    ARetornoWS.DadosRet.TituloRet.EMV                          := LJSONObject.AsString['qrCodePix'];
                    ARetornoWS.DadosRet.TituloRet.UrlPix                       := LJSONObject.AsString['qrCodeUrl'];
                    ARetornoWS.DadosRet.TituloRet.TxId                         := LJSONObject.AsString['txId'];

                    LJSONObject := LJsonArray.ItemAsJSONObject[0].AsJSONObject['payer'];
                    ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF               := LJSONObject.AsString['documentNumber'];
                    ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado            := LJSONObject.AsString['name'];
                    ARetornoWS.DadosRet.TituloRet.Sacado.Logradouro            := LJSONObject.AsString['address'];
                    ARetornoWS.DadosRet.TituloRet.Sacado.Bairro                := LJSONObject.AsString['neighborhood'];
                    ARetornoWS.DadosRet.TituloRet.Sacado.Cidade                := LJSONObject.AsString['city'];
                    ARetornoWS.DadosRet.TituloRet.Sacado.UF                    := LJSONObject.AsString['state'];
                    ARetornoWS.DadosRet.TituloRet.Sacado.Cep                   := LJSONObject.AsString['zipCode'];

                    LJSONObject := LJsonArray.ItemAsJSONObject[0].AsJSONObject['beneficiary'];
                    ARetornoWS.DadosRet.TituloRet.SacadoAvalista.CNPJCPF       := LJSONObject.AsString['documentNumber'];
                    ARetornoWS.DadosRet.TituloRet.SacadoAvalista.NomeAvalista  := LJSONObject.AsString['name'];
                  end;
                end;
              end;
            tpConsultaDetalhe :
              begin
                case HTTPResultCode of
                  200 :
                    begin

                      LTotalObjetos := LJsonArray.Count;

                      LJSONObject := LJsonArray.ItemAsJSONObject[0];

                      //if not LJSONObject.ValueExists('_content') then
                      if LJSONObject.ValueExists('bankSlipData') then
                         begin
                          LExisteBankSlip := true;
                          ARetornoWS.DadosRet.TituloRet.NossoNumero          := LJSONObject.AsString['bankNumber'];
                          ARetornoWS.DadosRet.TituloRet.SeuNumero            := LJSONObject.AsString['clientNumber'];
                          ARetornoWS.DadosRet.TituloRet.Vencimento           := StringToDateTimeDef(LJSONObject.AsString['dueDate'], 0, 'yyyy-mm-dd');
                          ARetornoWS.DadosRet.TituloRet.DataDocumento        := StringToDateTimeDef(LJSONObject.AsString['issueDate'], 0, 'yyyy-mm-dd');
                          ARetornoWS.DadosRet.TituloRet.ValorDocumento       := LJSONObject.AsFloat['nominalValue'];
                          ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := LJSONObject.AsString['status'];

                          //bankSlipData
                          LJSONObject := LJsonArray.ItemAsJSONObject[0].AsJSONObject['bankSlipData'];
                          ARetornoWS.DadosRet.TituloRet.DataProcessamento     := StringToDateTimeDef(LJSONObject.AsString['processingDate'], 0, 'yyyy-mm-dd');
                          ARetornoWS.DadosRet.TituloRet.DataRegistro          := StringToDateTimeDef(LJSONObject.AsString['entryDate'], 0, 'yyyy-mm-dd');
                          ARetornoWS.DadosRet.TituloRet.DiasDeProtesto        := LJSONObject.AsInteger['protestQuantityDays'];
                          ARetornoWS.DadosRet.TituloRet.QtdeParcelas          := LJSONObject.AsInteger['parcelsQuantity'];
                          ARetornoWS.DadosRet.TituloRet.QtdePagamentoParcial  := LJSONObject.AsInteger['paidParcelsQuantity'];
                          ARetornoWS.DadosRet.TituloRet.ValorRecebido         := LJSONObject.AsFloat['amountReceived'];
                          ARetornoWS.DadosRet.TituloRet.ValorMoraJuros        := LJSONObject.AsFloat['interestPercentage'];
                          ARetornoWS.DadosRet.TituloRet.LinhaDig              := LJSONObject.AsString['digitableLine'];
                          ARetornoWS.DadosRet.TituloRet.CodBarras             := LJSONObject.AsString['barCode'];

                          // payerData
                          LJSONObject := LJsonArray.ItemAsJSONObject[0].AsJSONObject['payerData'];
                          ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF    := LJSONObject.AsString['payerDocumentNumber'];
                          ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado := LJSONObject.AsString['payerName'];
                          ARetornoWS.DadosRet.TituloRet.Sacado.Logradouro := LJSONObject.AsString['payerAddress'];
                          ARetornoWS.DadosRet.TituloRet.Sacado.Bairro     := LJSONObject.AsString['payerNeighborhood'];
                          ARetornoWS.DadosRet.TituloRet.Sacado.Cidade     := LJSONObject.AsString['payerCounty'];
                          ARetornoWS.DadosRet.TituloRet.Sacado.UF         := LJSONObject.AsString['payerStateAbbreviation'];
                          ARetornoWS.DadosRet.TituloRet.Sacado.Cep        := LJSONObject.AsString['payerZipCode'];

                          if LJsonArray.ItemAsJSONObject[0].ValueExists('qrCodeData') then
                            begin
                              LJSONObject := LJsonArray.ItemAsJSONObject[0].AsJSONObject['qrCodeData'];
                              ARetornoWS.DadosRet.TituloRet.EMV               := LJSONObject.AsString['qrCode'];
                            end;

                         end;


                      //consulta nosso numero  NN
                      //Se a qtde objeto da resposta > 1 a lista para ler _content = 1
                      //caso exista apenas um obj na resposta a lista inicia no 0
                      if LExisteBankSlip then
                         nIndiceOBJ := strtoint(IfThen(LTotalObjetos>1,'1','0'))
                      else
                         nIndiceOBJ := 0;

                      if (LJsonArray.ItemAsJSONObject[nIndiceOBJ].AsJSONArray['_content'].Count > 0) then
                      begin
                        LJSONObject := LJsonArray.ItemAsJSONObject[nIndiceOBJ].AsJSONArray['_content'].ItemAsJSONObject[0];
                        if nIndiceOBJ = 0 then
                           begin
                              ARetornoWS.DadosRet.TituloRet.NossoNumero                 := LJSONObject.AsString['bankNumber'];
                              ARetornoWS.DadosRet.TituloRet.SeuNumero                   := LJSONObject.AsString['clientNumber'];
                              if ARetornoWS.DadosRet.TituloRet.Vencimento = 0 then
                               ARetornoWS.DadosRet.TituloRet.Vencimento                  := StringToDateTimeDef(LJSONObject.AsString['dueDate'], 0, 'yyyy-mm-dd');
                              if ARetornoWS.DadosRet.TituloRet.DataDocumento = 0 then
                               ARetornoWS.DadosRet.TituloRet.DataDocumento               := StringToDateTimeDef(LJSONObject.AsString['issueDate'], 0, 'yyyy-mm-dd');
                              if ARetornoWS.DadosRet.TituloRet.ValorDocumento = 0 then
                               ARetornoWS.DadosRet.TituloRet.ValorDocumento              := LJSONObject.AsFloat['nominalValue'];
                              if NaoEstaVazio(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca) then
                               ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := LJSONObject.AsString['status'];
                           end;


                        if EstaVazio(ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca) then
                           ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca:= LJSONObject.AsString['statusComplement'];

                        ARetornoWS.DadosRet.TituloRet.ValorDesconto               := LJSONObject.AsFloat['discountValue'];
                        ARetornoWS.DadosRet.TituloRet.ValorPago                   := LJSONObject.AsFloat['paidValue'];
                        ARetornoWS.DadosRet.TituloRet.ValorMoraJuros              := LJSONObject.AsFloat['interestValue'];
                        ARetornoWS.DadosRet.TituloRet.ValorAbatimento             := LJSONObject.AsFloat['deductionValue'];

                      if LExisteBankSlip then
                         nIndiceOBJ := strtoint(IfThen(LTotalObjetos>2,'2','1'))
                      else
                         nIndiceOBJ := strtoint(IfThen(LTotalObjetos>1,'1','0'));

                      //settlementData consulta para pegar data do pagamento

                      if (LJsonArray.ItemAsJSONObject[nIndiceOBJ].AsJSONArray['settlementData'].Count > 0) then
                         begin
                          if LJsonArray.ItemAsJSONObject[nIndiceOBJ].ValueExists('settlementData') then
                             begin
                                LJSONObject := LJsonArray.ItemAsJSONObject[nIndiceOBJ].AsJSONArray['settlementData'].ItemAsJSONObject[0];
                                if ARetornoWS.DadosRet.TituloRet.DataCredito = 0 then
                                 ARetornoWS.DadosRet.TituloRet.DataCredito       := StringToDateTimeDef(LJSONObject.AsString['settlementCreditDate'], 0, 'yyyy-mm-dd');
                                if ARetornoWS.DadosRet.TituloRet.DataMovimento = 0 then
                                 ARetornoWS.DadosRet.TituloRet.DataMovimento        := StringToDateTimeDef(LJSONObject.AsString['settlementDate'], 0, 'yyyy-mm-dd');
                                if ARetornoWS.DadosRet.TituloRet.DataBaixa = 0 then
                                 ARetornoWS.DadosRet.TituloRet.DataBaixa            := StringToDateTimeDef(LJSONObject.AsString['settlementDate'], 0, 'yyyy-mm-dd');
                                if EstaVazio(ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca) then
                                 ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := LJSONObject.AsString['settlementDescription'];
                                if EstaVazio(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca) then
                                 ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := LJSONObject.AsString['status'];
                             end;
                         end;

                      if (LJsonArray.ItemAsJSONObject[nIndiceOBJ].AsJSONArray['writeOffData'].Count > 0) then
                         begin
                          if LJsonArray.ItemAsJSONObject[nIndiceOBJ].ValueExists('writeOffData') then
                             begin
                                LJSONObject := LJsonArray.ItemAsJSONObject[nIndiceOBJ].AsJSONArray['writeOffData'].ItemAsJSONObject[0];
                                if ARetornoWS.DadosRet.TituloRet.DataBaixa = 0 then
                                 ARetornoWS.DadosRet.TituloRet.DataBaixa := StringToDateTimeDef(LJSONObject.AsString['writeOffDate'], 0, 'yyyy-mm-dd');
                                if ARetornoWS.DadosRet.TituloRet.ValorPago = 0 then
                                 ARetornoWS.DadosRet.TituloRet.ValorPago := LJSONObject.AsFloat['writeOffValue'];
                                if EstaVazio(ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca) then
                                 ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := LJSONObject.AsString['writeOffDescription'];

                             end;
                         end;


                      end;
                    end;
                end;
              end;
          end;
        end else
        begin
          LJsonErrorArray := LJsonArray.ItemAsJSONObject[0].AsJSONArray['_errors'];
          if LJsonErrorArray.Count > 0  then
          begin
            ARetornoWS.CodRetorno := LJsonArray.ItemAsJSONObject[0].AsString['_errorCode'];
            ARetornoWS.MsgRetorno := LJsonArray.ItemAsJSONObject[0].AsString['_message'];
            for I := 0 to Pred(LJsonErrorArray.Count) do
            begin
              LRejeicao          := ARetornoWS.CriarRejeicaoLista;

              LRejeicao.Codigo   := LJsonErrorArray.ItemAsJSONObject[I].AsString['_code'];
              LRejeicao.Campo    := LJsonErrorArray.ItemAsJSONObject[I].AsString['_field'];
              LRejeicao.Mensagem := LJsonErrorArray.ItemAsJSONObject[I].AsString['_message'];
              LRejeicao.Valor    := '';
            end;
          end;
        end;
      except
        Result := False;
      end;
    finally
     LJsonArray.free;
    end;
  end
  else
  begin
    case HTTPResultCode of
      401 :
      begin
        LRejeicao            := ARetornoWS.CriarRejeicaoLista;
        LRejeicao.Codigo     := '401';
        LRejeicao.Mensagem   := 'Não autorizado/Autenticado';
        LRejeicao.Ocorrencia := '401 - Não autorizado/Autenticado';
      end;
      403 :
      begin
        LRejeicao            := ARetornoWS.CriarRejeicaoLista;
        LRejeicao.Codigo     := '403';
        LRejeicao.Mensagem   := 'Não Autorizado';
        LRejeicao.Ocorrencia := '403 - Não Autorizado';
      end;
      404 :
      begin
        LRejeicao            := ARetornoWS.CriarRejeicaoLista;
        LRejeicao.Codigo     := '404';
        LRejeicao.Mensagem   := 'Informação não encontrada';
        LRejeicao.Ocorrencia := '404 - Informação não encontrada';
      end;
      406 :
      begin
        LRejeicao            := ARetornoWS.CriarRejeicaoLista;
        LRejeicao.Codigo     := '406';
        LRejeicao.Mensagem   := 'O recurso de destino não possui uma representação atual que seria aceitável';
        LRejeicao.Ocorrencia := '406 - O recurso de destino não possui uma representação atual que seria aceitável';
      end;
      500 :
      begin
        LRejeicao            := ARetornoWS.CriarRejeicaoLista;
        LRejeicao.Codigo     := '500';
        LRejeicao.Mensagem   := 'Erro de Servidor, Aplicação está fora';
        LRejeicao.Ocorrencia := '500 - Erro de Servidor, Aplicação está fora';
      end;
      501 :
      begin
        LRejeicao            := ARetornoWS.CriarRejeicaoLista;
        LRejeicao.Codigo     := '501';
        LRejeicao.Mensagem   := 'Erro de Servidor, Aplicação está fora';
        LRejeicao.Ocorrencia := '501 - O servidor não oferece suporte à funcionalidade necessária para atender à solicitação';
      end;
    end;
  end;
end;
function TRetornoEnvio_Santander_API.RetornoEnvio(const AIndex: Integer): Boolean;
begin
  Result:=inherited RetornoEnvio(AIndex);
end;

end.

