{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{ Colaboradores nesse arquivo: Willian Delan de Oliveira                       }
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
//incluido em 15/08/2023

{$I ACBr.inc}

unit ACBrBoletoRet_Cresol;

interface

uses
   SysUtils,
   StrUtils,
   ACBrBoleto,
   ACBrBoletoRetorno,
   ACBrBoletoWS.Rest,
   ACBrJSON;

type
   { TRetornoEnvio_Cresol_API }
   TRetornoEnvio_Cresol = class(TRetornoEnvioREST)
   private
      function DateCresolToDateTime(const AValue: String): TDateTime;
   public
      constructor Create(ABoletoWS: TACBrBoleto); override;
      destructor  Destroy; Override;
      function LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean; override;
      function LerListaRetorno: Boolean; override;
      function RetornoEnvio(const AIndex: Integer): Boolean; override;
   end;

implementation

uses
   ACBrBoletoConversao,
   ACBrUtil.Strings;

{ TRetornoEnvio }

constructor TRetornoEnvio_Cresol.Create(ABoletoWS: TACBrBoleto);
begin
   inherited Create(ABoletoWS);
end;

function TRetornoEnvio_Cresol.DateCresolToDateTime(const AValue: String): TDateTime;
var
   LDia, LMes, LAno: String;
begin
   if trim(AValue) = '' then
     Result := 0
   else
   begin
     LAno := Copy(AValue, 0, 4);
     LMes := Copy(AValue, 6, 2);
     LDia := Copy(AValue, 9, 2);
     Result := StrToDate(LDia+'/'+LMes+'/'+LAno);
   end;
end;

destructor TRetornoEnvio_Cresol.Destroy;
begin
   inherited Destroy;
end;

function TRetornoEnvio_Cresol.LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;
var
   LJsonArray: TACBrJSONArray;
   LJsonObject: TACBrJSONObject;
   ARejeicao: TACBrBoletoRejeicao;
   LJsonOcorrencias: TACBrJSONArray;
   LJsonOcorrencia: TACBrJSONObject;
   ATipoOperacao : TOperacao;
   i:Integer;
begin
   Result := True;
   ATipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;
   ARetornoWs.JSONEnvio      := EnvWs;
   ARetornoWS.HTTPResultCode := HTTPResultCode;
   if Trim(RetWS) <> '' then
   begin
      try
        if Pos('[', RetWS) = 1 then
          begin
            LJsonArray := TACBrJSONArray.Parse(RetWS);
            try
               if LJsonArray.Count > 1 then
                 raise Exception.Create('Retorno com mais de um título.');
               
               ARetornoWS.JSON:=   LJsonArray.ItemAsJSONObject[0].ToJSON;
            finally
              LJsonArray.Free;
            end;
          end
        else
            ARetornoWS.JSON:=RetWS; 

         LJsonObject := TACBrJSONObject.Parse(ARetornoWS.JSON);
         try
            case HttpResultCode of
               207, 400, 406, 500 :
               begin
                  ARejeicao            := ARetornoWS.CriarRejeicaoLista;
                  ARejeicao.Codigo     := LJsonObject.AsString['code'];
                  ARejeicao.mensagem   := LJsonObject.AsString['message'];
               end;
               200 :
               //retorna quando tiver sucesso
               if (ARetornoWS.ListaRejeicao.Count = 0) then
               begin
                  if (ATipoOperacao = tpInclui) then
                  begin
                     ARetornoWS.DadosRet.IDBoleto.IDBoleto        := LJsonObject.AsString['id'];
                     ARetornoWS.DadosRet.IDBoleto.CodBarras       := LJsonObject.AsString['codigoBarras'];
                     ARetornoWS.DadosRet.IDBoleto.LinhaDig        := LJsonObject.AsString['linhaDigitavel'];
                     ARetornoWS.DadosRet.IDBoleto.NossoNum        := LJsonObject.AsString['nossoNumero'];
                     ARetornoWS.DadosRet.TituloRet.CodBarras      := ARetornoWS.DadosRet.IDBoleto.CodBarras;
                     ARetornoWS.DadosRet.TituloRet.LinhaDig       := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
                     ARetornoWS.DadosRet.TituloRet.NossoNumero    := ARetornoWS.DadosRet.IDBoleto.NossoNum;
                     ARetornoWS.DadosRet.TituloRet.Vencimento     := DateCresolToDateTime(LJsonObject.AsString['dtVencimento']);
                     ARetornoWS.DadosRet.TituloRet.NossoNumero    := LJsonObject.AsString['nossoNumero'];
                     ARetornoWS.DadosRet.TituloRet.SeuNumero      := StrUtils.IfThen(LJsonObject.AsString['NumeroDocumento'] <> '',
                                                                                 LJsonObject.AsString['NumeroDocumento'],
                                                                                 OnlyNumber(LJsonObject.AsString['nossoNumero'])
                                                                              );
                     ARetornoWS.DadosRet.TituloRet.EspecieDoc     := LJsonObject.AsString['idEspecie'];
                     ARetornoWS.DadosRet.TituloRet.DataDocumento  := DateCresolToDateTime(LJsonObject.AsString['dtDocumento']);
                     ARetornoWS.DadosRet.TituloRet.ValorDocumento := LJsonObject.AsFloat['valorNominal'];
                     ARetornoWS.DadosRet.TituloRet.ValorDesconto  := LJsonObject.AsFloat['valorDesconto'];
                     ARetornoWS.DadosRet.TituloRet.NossoNumeroCorrespondente  := ARetornoWS.DadosRet.IDBoleto.IDBoleto;
                  end
                  else
                  if (ATipoOperacao = tpConsultaDetalhe) then
                  begin
                     if LJsonObject.AsString['id'] <> '' then
                     begin
                        ARetornoWS.DadosRet.IDBoleto.IDBoleto         := LJsonObject.AsString['id'];
                        ARetornoWS.DadosRet.IDBoleto.CodBarras        := LJsonObject.AsString['codigoBarras'];
                        ARetornoWS.DadosRet.IDBoleto.LinhaDig         := LJsonObject.AsString['linhaDigitavel'];
                        ARetornoWS.DadosRet.IDBoleto.NossoNum         := LJsonObject.AsString['nossoNumero'];
                        ARetornoWS.indicadorContinuidade              := false;
                        ARetornoWS.DadosRet.TituloRet.NossoNumero     := ARetornoWS.DadosRet.IDBoleto.NossoNum;
                        ARetornoWS.DadosRet.TituloRet.Vencimento      := DateCresolToDateTime(LJsonObject.AsString['dtVencimento']);
                        ARetornoWS.DadosRet.TituloRet.ValorDocumento  := LJsonObject.AsFloat['valorNominal'];
                        if LJsonObject.AsString['cdTipoMulta'] = 'ISENTO' then//Sem multa.
                           ARetornoWS.DadosRet.TituloRet.PercentualMulta := 0
                        else
                           ARetornoWS.DadosRet.TituloRet.PercentualMulta  := LJsonObject.AsFloat['valorMulta'];
                        if LJsonObject.AsString['cdTipoJuros'] = 'ISENTO' then
                        begin//Sem juros.
                           ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros := cjIsento;
                           ARetornoWS.DadosRet.TituloRet.ValorMoraJuros  := 0;
                        end
                        else
                        begin
                           if LJsonObject.AsString['cdTipoJuros'] = 'VALOR_FIXO' then
                           begin//VALOR_FIXO > Valor Dia.
                              ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros := cjValorDia;
                              ARetornoWS.DadosRet.TituloRet.ValorMoraJuros  := LJsonObject.AsFloat['valorJuros'];
                           end
                           else
                           begin
                              ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros := cjTaxaMensal;//VALOR_PERCENTUAL > Taxa Mensal.
                              ARetornoWS.DadosRet.TituloRet.ValorMoraJuros  := LJsonObject.AsFloat['valorJuros'];
                           end;
                        end;
                        ARetornoWS.DadosRet.TituloRet.CodBarras       := ARetornoWS.DadosRet.IDBoleto.CodBarras;
                        ARetornoWS.DadosRet.TituloRet.LinhaDig        := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
                        ARetornoWS.DadosRet.TituloRet.SeuNumero      := StrUtils.IfThen(LJsonObject.AsString['NumeroDocumento'] <> '',
                                                                                    LJsonObject.AsString['NumeroDocumento'],
                                                                                    OnlyNumber(LJsonObject.AsString['nossoNumero'])
                                                                                 );
                        ARetornoWS.DadosRet.TituloRet.DataRegistro    := DateCresolToDateTime(LJsonObject.AsString['dtDocumento']);
                        ARetornoWS.DadosRet.TituloRet.Vencimento      := DateCresolToDateTime(LJsonObject.AsString['dtVencimento']);
                        ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca  := LJsonObject.AsString['status'];//0-EM_ABERTO|3-BAIXADO_MANUALMENTE|5-LIQUIDADO
                        if Pos('EM_ABERTO', UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca)) > 0 then//ABERTO
                           ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '1';
                        if Pos('BAIXADO_MANUALMENTE',UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca)) > 0 then//BAIXADO
                           ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '7';
                        if Pos('LIQUIDADO', UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca)) > 0 then//LÍQUIDADO
                           ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '6';
                        ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado     := LJsonObject.AsString['pagadorNome'];
                        ARetornoWS.DadosRet.TituloRet.Sacado.Cidade         := LJsonObject.AsString['pagadorCidade'];
                        ARetornoWS.DadosRet.TituloRet.Sacado.UF             := LJsonObject.AsString['pagadorUf'];
                        ARetornoWS.DadosRet.TituloRet.Sacado.Bairro         := LJsonObject.AsString['pagadorBairro'];
                        ARetornoWS.DadosRet.TituloRet.Sacado.Cep            := LJsonObject.AsString['pagadorCep'];
                        ARetornoWS.DadosRet.TituloRet.Sacado.Numero         := LJsonObject.AsString['pagadorEnderecoNumero'];
                        ARetornoWS.DadosRet.TituloRet.Sacado.Logradouro     := LJsonObject.AsString['pagadorEndereco'];
                        ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF        := LJsonObject.AsString['docPagador'];

                         // WorkAround para pegar DataPagamento e Valor de Pagamento
                        if ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca = '6' then                    
                        begin                      
                          LJsonOcorrencias :=  LJSonObject.AsJSONArray['ocorrencias'];
                          for i := 0 to Pred(LJsonOcorrencias.Count) do
                          begin
                            LJsonOcorrencia := LJsonOcorrencias.ItemAsJSONObject[i];
                            if LJsonOcorrencia.AsString['tipo'] = 'C' then // D = Debito tarifa, C = Liquidação
                            begin
                              ARetornoWS.DadosRet.TituloRet.DataMovimento := DateCresolToDateTime(LJsonOcorrencia.AsString['dtOcorrencia']);
                              ARetornoWS.DadosRet.TituloRet.DataBaixa := DateCresolToDateTime(LJsonOcorrencia.AsString['dtOcorrencia']);
                              ARetornoWS.DadosRet.TituloRet.DataCredito := DateCresolToDateTime(LJsonOcorrencia.AsString['dtCredito']);
                              ARetornoWS.DadosRet.TituloRet.ValorPago := LJsonOcorrencia.AsCurrency['valor'];
                            end;
                          end;
                        end
                        else
                        begin
                          ARetornoWS.DadosRet.TituloRet.DataCredito     := 0;
                          ARetornoWS.DadosRet.TituloRet.ValorPago       := 0;
                          ARetornoWS.DadosRet.TituloRet.DataMovimento   := DateCresolToDateTime(LJsonObject.AsString['dtDocumento']){????};
                        end;
                     end;
                  end
                  else
                  if (ATipoOperacao = tpBaixa) then
                  begin
                     // não possui dados de retorno..
                  end
                  else if (ATipoOperacao = tpAltera) then
                  begin
                     // não possui dados de retorno..
                  end;
               end;
            else
               ARejeicao            := ARetornoWS.CriarRejeicaoLista;
               ARejeicao.Codigo     := LJsonObject.AsString['code'];
               ARejeicao.mensagem   := LJsonObject.AsString['message'];
            end;
         Finally
            LJsonObject.free;
         End;
      except
         Result := False;
      end;
   end;
end;

function TRetornoEnvio_Cresol.LerListaRetorno: Boolean;
var
  ListaRetorno: TACBrBoletoRetornoWS;
  LJsonObject, LJsonBoletoObject, LJsonOcorrencia : TACBrJSONObject;
  LJsonArray,LJsonOcorrencias: TACBrJSONArray;
  LListaRejeicao : TACBrBoletoRejeicao;
  I, j: Integer;
begin
  Result := True;
  ListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;
  ListaRetorno.HTTPResultCode := HTTPResultCode;
  if RetWS <> '' then
  begin
    try
      LJsonObject := TACBrJSONObject.Parse(RetWS);
      try
           case HTTPResultCode of
           207, 400, 406, 500:
           begin
             LListaRejeicao            := ListaRetorno.CriarRejeicaoLista;
             LListaRejeicao.Codigo     := LJsonObject.AsString['code'];
             LListaRejeicao.mensagem   := LJsonObject.AsString['message'];
           end;
           200:
             if (ListaRetorno.ListaRejeicao.Count = 0) then
             begin
               if LJsonObject.IsJSONObject('page') then
               begin
                 ListaRetorno.indicadorContinuidade := LJsonObject.AsBoolean['last'] = false;
                 if ListaRetorno.indicadorContinuidade then
                   ListaRetorno.proximoIndice         := LJsonObject.AsInteger['number'] + 1;
               end;

               LJsonArray := LJsonObject.AsJSONArray['content'];
               if LJsonArray.Count = 0 then
               begin
                 LListaRejeicao            := ListaRetorno.CriarRejeicaoLista;
                 LListaRejeicao.Codigo     := '00';
                 LListaRejeicao.Mensagem   := 'Lista de retorno vazia!';
               end;

               for I := 0 to Pred(LJsonArray.Count) do
               begin
                 if I > 0 then
                   ListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;
                 LJsonBoletoObject  := LJsonArray.ItemAsJSONObject[I];

                 ListaRetorno.DadosRet.IDBoleto.IDBoleto         := LJsonBoletoObject.AsString['id'];
                 ListaRetorno.DadosRet.IDBoleto.CodBarras        := LJsonBoletoObject.AsString['codigoBarras'];
                 ListaRetorno.DadosRet.IDBoleto.LinhaDig         := LJsonBoletoObject.AsString['linhaDigitavel'];
                 ListaRetorno.DadosRet.IDBoleto.NossoNum         := LJsonBoletoObject.AsString['nossoNumero'];
                 ListaRetorno.DadosRet.TituloRet.NossoNumero     := ListaRetorno.DadosRet.IDBoleto.NossoNum;
                 ListaRetorno.DadosRet.TituloRet.Vencimento      := DateCresolToDateTime(LJsonBoletoObject.AsString['dtVencimento']);
                 ListaRetorno.DadosRet.TituloRet.ValorDocumento  := LJsonBoletoObject.AsFloat['valorNominal'];
                 ListaRetorno.DadosRet.TituloRet.NossoNumeroCorrespondente  := LJsonBoletoObject.AsString['id'];

                 if LJsonBoletoObject.AsString['cdTipoMulta'] = 'ISENTO' then//Sem multa.
                    ListaRetorno.DadosRet.TituloRet.PercentualMulta := 0
                 else
                    ListaRetorno.DadosRet.TituloRet.PercentualMulta  := LJsonBoletoObject.AsFloat['valorMulta'];
                 if LJsonBoletoObject.AsString['cdTipoJuros'] = 'ISENTO' then
                 begin//Sem juros.
                    ListaRetorno.DadosRet.TituloRet.CodigoMoraJuros := cjIsento;
                    ListaRetorno.DadosRet.TituloRet.ValorMoraJuros  := 0;
                 end
                 else
                 begin
                    if LJsonBoletoObject.AsString['cdTipoJuros'] = 'VALOR_FIXO' then
                    begin//VALOR_FIXO > Valor Dia.
                       ListaRetorno.DadosRet.TituloRet.CodigoMoraJuros := cjValorDia;
                       ListaRetorno.DadosRet.TituloRet.ValorMoraJuros  := LJsonBoletoObject.AsFloat['valorJuros'];
                    end
                    else
                    begin
                       ListaRetorno.DadosRet.TituloRet.CodigoMoraJuros := cjTaxaMensal;//VALOR_PERCENTUAL > Taxa Mensal.
                       ListaRetorno.DadosRet.TituloRet.ValorMoraJuros  := LJsonBoletoObject.AsFloat['valorJuros'];
                    end;
                 end;
                 ListaRetorno.DadosRet.TituloRet.CodBarras       := ListaRetorno.DadosRet.IDBoleto.CodBarras;
                 ListaRetorno.DadosRet.TituloRet.LinhaDig        := ListaRetorno.DadosRet.IDBoleto.LinhaDig;
                 ListaRetorno.DadosRet.TituloRet.SeuNumero      := StrUtils.IfThen(LJsonBoletoObject.AsString['NumeroDocumento'] <> '',
                                                                             LJsonBoletoObject.AsString['NumeroDocumento'],
                                                                             OnlyNumber(LJsonBoletoObject.AsString['nossoNumero'])
                                                                          );
                 ListaRetorno.DadosRet.TituloRet.DataRegistro    := DateCresolToDateTime(LJsonBoletoObject.AsString['dtDocumento']);
                 ListaRetorno.DadosRet.TituloRet.Vencimento      := DateCresolToDateTime(LJsonBoletoObject.AsString['dtVencimento']);
                 ListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca  := LJsonBoletoObject.AsString['status'];//0-EM_ABERTO|3-BAIXADO_MANUALMENTE|5-LIQUIDADO
                 if Pos('EM_ABERTO', UpperCase(ListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca)) > 0 then//ABERTO
                    ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '1';
                 if Pos('BAIXADO_MANUALMENTE',UpperCase(ListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca)) > 0 then//BAIXADO
                    ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '7';
                 if Pos('LIQUIDADO', UpperCase(ListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca)) > 0 then//LÍQUIDADO
                    ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '6';
                 ListaRetorno.DadosRet.TituloRet.Sacado.NomeSacado     := LJsonBoletoObject.AsString['pagadorNome'];
                 ListaRetorno.DadosRet.TituloRet.Sacado.Cidade         := LJsonBoletoObject.AsString['pagadorCidade'];
                 ListaRetorno.DadosRet.TituloRet.Sacado.UF             := LJsonBoletoObject.AsString['pagadorUf'];
                 ListaRetorno.DadosRet.TituloRet.Sacado.Bairro         := LJsonBoletoObject.AsString['pagadorBairro'];
                 ListaRetorno.DadosRet.TituloRet.Sacado.Cep            := LJsonBoletoObject.AsString['pagadorCep'];
                 ListaRetorno.DadosRet.TituloRet.Sacado.Numero         := LJsonBoletoObject.AsString['pagadorEnderecoNumero'];
                 ListaRetorno.DadosRet.TituloRet.Sacado.Logradouro     := LJsonBoletoObject.AsString['pagadorEndereco'];
                 ListaRetorno.DadosRet.TituloRet.Sacado.CNPJCPF        := LJsonBoletoObject.AsString['docPagador'];

                  // WorkAround para pegar DataPagamento e Valor de Pagamento
                 if ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca = '6' then
                 begin
                   LJsonOcorrencias :=  LJsonBoletoObject.AsJSONArray['ocorrencias'];
                   for j := 0 to Pred(LJsonOcorrencias.Count) do
                   begin
                     LJsonOcorrencia := LJsonOcorrencias.ItemAsJSONObject[j];
                     if LJsonOcorrencia.AsString['tipo'] = 'C' then // D = Debito tarifa, C = Liquidação
                     begin
                       ListaRetorno.DadosRet.TituloRet.DataMovimento := DateCresolToDateTime(LJsonOcorrencia.AsString['dtOcorrencia']);
                       ListaRetorno.DadosRet.TituloRet.DataBaixa := DateCresolToDateTime(LJsonOcorrencia.AsString['dtOcorrencia']);
                       ListaRetorno.DadosRet.TituloRet.DataCredito := DateCresolToDateTime(LJsonOcorrencia.AsString['dtCredito']);
                       ListaRetorno.DadosRet.TituloRet.ValorPago := LJsonOcorrencia.AsCurrency['valor'];
                     end;
                   end;
                 end
                 else
                 begin
                   ListaRetorno.DadosRet.TituloRet.DataCredito     := 0;
                   ListaRetorno.DadosRet.TituloRet.ValorPago       := 0;
                   ListaRetorno.DadosRet.TituloRet.DataMovimento   := DateCresolToDateTime(LJsonBoletoObject.AsString['dtDocumento']){????};
                 end;

               end;
             end;
           end;
         finally
           LJsonObject.free;
         end;
       except
         Result := False;
       end;
     end;
end;

function TRetornoEnvio_Cresol.RetornoEnvio(const AIndex: Integer): Boolean;
begin
   Result:=inherited RetornoEnvio(AIndex);
end;

end.

