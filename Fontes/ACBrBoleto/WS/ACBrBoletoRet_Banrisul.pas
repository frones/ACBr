{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
//incluido em 12/09/2024
{$I ACBr.inc}

unit ACBrBoletoRet_Banrisul;

interface

uses
  SysUtils,
  ACBrBoleto,
  ACBrBoletoWS,
  ACBrBoletoRetorno,
  ACBrBoletoWS.Rest;
type

{ TRetornoEnvio_Banrisul }

 TRetornoEnvio_Banrisul = class(TRetornoEnvioREST)
 private
   function DateBBtoDateTime(Const AValue : String) : TDateTime;
 public
   constructor Create(ABoletoWS: TACBrBoleto); override;
   destructor  Destroy; Override;
   function LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean; override;
   function RetornoEnvio(const AIndex: Integer): Boolean; override;
   function TrataNossoNumero(const ANossoNumero: string):string;

 end;

implementation

uses
  ACBrBoletoConversao,
  ACBrUtil.Strings,
  ACBrUtil.Base,
  ACBrJSON;

{ TRetornoEnvio }

constructor TRetornoEnvio_Banrisul.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);
end;

function TRetornoEnvio_Banrisul.DateBBtoDateTime(const AValue: String): TDateTime;
var lsData : String;
begin
   lsData := copy(AValue,9,2) + '/' + copy(AValue,6,2) + '/' + copy(AValue,1,4);
   Result := StrToDateDef( lsData,0);
end;

destructor TRetornoEnvio_Banrisul.Destroy;
begin
  inherited Destroy;
end;

function TRetornoEnvio_Banrisul.LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;
var
   LJsonObject, LItemObject: TACBrJSONObject;
   LJsonArray: TACBrJSONArray;
   LMensagemRejeicao: TACBrBoletoRejeicao;
   LTipoOperacao: TOperacao;
   I: Integer;
begin
   Result := True;

   LTipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;

   ARetornoWS.HTTPResultCode := HTTPResultCode;
   ARetornoWS.JSONEnvio := EnvWs;
   ARetornoWS.Header.Operacao := LTipoOperacao;

   if RetWS <> '' then
   begin
      try
         LJsonObject := TACBrJSONObject.Parse(RetWS);
         try
            ARetornoWS.JSON := LJsonObject.ToJSON;

            if HTTPResultCode >= 400 then
            begin
               LJsonArray := LJsonObject.AsJSONArray['ocorrencias'];

               if LJsonArray.Count > 0 then
               begin
                  for I := 0 to Pred(LJsonArray.Count) do
                  begin
                     LItemObject := LJsonArray.ItemAsJSONObject[I];
                     LMensagemRejeicao := ARetornoWS.CriarRejeicaoLista;

                     if NaoEstaVazio(LItemObject.AsString['codigo']) or
                       NaoEstaVazio(LItemObject.AsString['mensagem']) or
                       NaoEstaVazio(LItemObject.AsString['complemento']) then
                     begin
                        LMensagemRejeicao.Codigo := LItemObject.AsString['codigo'];
                        LMensagemRejeicao.Mensagem := LItemObject.AsString['mensagem'];
                        LMensagemRejeicao.Ocorrencia := LItemObject.AsString['complemento'];
                     end;
                  end;
               end
               else if LJsonObject.AsString['ocorrencias'] <> '' then
               begin
                  LMensagemRejeicao := ARetornoWS.CriarRejeicaoLista;
                  LMensagemRejeicao.Codigo := LJsonObject.AsString['statusCode'];
                  LMensagemRejeicao.Versao := LJsonObject.AsString['error'];
                  LMensagemRejeicao.Mensagem := LJsonObject.AsString['message'];
               end;
            end;

            // retorna quando tiver sucesso
            if (ARetornoWS.ListaRejeicao.Count = 0) then
            begin
               if (LTipoOperacao = tpInclui) then
               begin
                  ARetornoWS.DadosRet.IDBoleto.CodBarras := LJsonObject.AsJSONObject['titulo'].AsString['codigo_barras'];
                  ARetornoWS.DadosRet.IDBoleto.LinhaDig  := LJsonObject.AsJSONObject['titulo'].AsString['linha_digitavel'];
                  ARetornoWS.DadosRet.IDBoleto.NossoNum  := TrataNossoNumero(LJsonObject.AsJSONObject['titulo'].AsString['nosso_numero']);

                  ARetornoWS.DadosRet.TituloRet.CodBarras   := ARetornoWS.DadosRet.IDBoleto.CodBarras;
                  ARetornoWS.DadosRet.TituloRet.LinhaDig    := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
                  ARetornoWS.DadosRet.TituloRet.NossoNumero := ARetornoWS.DadosRet.IDBoleto.NossoNum;
                  // ARetornoWS.DadosRet.TituloRet.Carteira    := LJsonObject.AsJSONObject['titulo'].AsString['carteira'];
                  // ARetornoWS.DadosRet.TituloRet.Modalidade    := LJsonObject.AsInteger['numeroVariacaoCarteira'];
                  // ARetornoWS.DadosRet.TituloRet.CodigoCliente := LJsonObject.AsFloat['codigoCliente'];
                  ARetornoWS.DadosRet.TituloRet.Contrato    := LJsonObject.AsJSONObject['titulo'].AsJSONObject['beneficiario'].AsString['codigo'];
                  ARetornoWS.DadosRet.TituloRet.NossoNumeroCorrespondente := TrataNossoNumero(LJsonObject.AsJSONObject['titulo'].AsString['nosso_numero']);

                  if LJsonObject.AsJSONObject['titulo'].IsJSONObject('hibrido') then
                  begin
                     LItemObject := LJsonObject.AsJSONObject['titulo'].AsJSONObject['hibrido'];

                     ARetornoWS.DadosRet.TituloRet.UrlPix := LItemObject.AsString['location'];
                     ARetornoWS.DadosRet.TituloRet.TxId   := LItemObject.AsString['txid'];
                     ARetornoWS.DadosRet.TituloRet.EMV    := LItemObject.AsString['copia_cola'];
                  end;

               end
               else if (LTipoOperacao = tpConsultaDetalhe) then
               begin
                  ARetornoWS.DadosRet.IDBoleto.CodBarras := LJsonObject.AsJSONObject['titulo'].AsString['codigo_barras'];
                  ARetornoWS.DadosRet.IDBoleto.LinhaDig  := LJsonObject.AsJSONObject['titulo'].AsString['linha_digitavel'];
                  ARetornoWS.DadosRet.IDBoleto.NossoNum  := TrataNossoNumero(LJsonObject.AsJSONObject['titulo'].AsString['nosso_numero']);

                  ARetornoWS.DadosRet.TituloRet.CodBarras   := ARetornoWS.DadosRet.IDBoleto.CodBarras;
                  ARetornoWS.DadosRet.TituloRet.LinhaDig    := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
                  ARetornoWS.DadosRet.TituloRet.NossoNumero := ARetornoWS.DadosRet.IDBoleto.NossoNum;
                  ARetornoWS.DadosRet.TituloRet.NossoNumeroCorrespondente := TrataNossoNumero(LJsonObject.AsJSONObject['titulo'].AsString['nosso_numero']);
                  ARetornoWS.DadosRet.TituloRet.Carteira    := LJsonObject.AsJSONObject['titulo'].AsString['carteira'];
                  ARetornoWS.DadosRet.TituloRet.Contrato    := LJsonObject.AsJSONObject['titulo'].AsJSONObject['beneficiario'].AsString['codigo'];


                  if LJsonObject.AsJSONObject['titulo'].IsJSONObject('hibrido') then
                  begin

                     LItemObject := LJsonObject.AsJSONObject['titulo'].AsJSONObject['hibrido'];

                     ARetornoWS.DadosRet.TituloRet.UrlPix := LItemObject.AsString['location'];
                     ARetornoWS.DadosRet.TituloRet.TxId   := LItemObject.AsString['txid'];
                     ARetornoWS.DadosRet.TituloRet.EMV    := LItemObject.AsString['copia_cola'];
                  end;


                  // Dados Adicionais

                  ARetornoWS.DadosRet.TituloRet.NumeroDocumento:= LJsonObject.AsJSONObject['titulo'].AsString['seu_numero'];
                  ARetornoWS.DadosRet.TituloRet.DataRegistro   := DateBBtoDateTime(LJsonObject.AsJSONObject['titulo'].AsString['data_emissao'] );
                  ARetornoWS.DadosRet.TituloRet.Vencimento     := DateBBtoDateTime(LJsonObject.AsJSONObject['titulo'].AsString['data_vencimento']);
                  ARetornoWS.DadosRet.TituloRet.ValorDocumento := LJsonObject.AsJSONObject['titulo'].AsFloat['valor_nominal'];
                  ARetornoWS.DadosRet.TituloRet.Carteira       := LJsonObject.AsJSONObject['titulo'].AsString['carteira'];


                  ARetornoWS.DadosRet.TituloRet.codigoEstadoTituloCobranca :=  LJsonObject.AsJSONObject['titulo'].AsString['situacao_banrisul'];
                  // situacao_banrisul
                  if LJsonObject.AsJSONObject['titulo'].AsString['situacao_banrisul'] = 'A' then
                        ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := 'ATIVO'
                  else if LJsonObject.AsJSONObject['titulo'].AsString['situacao_banrisul'] = 'B' then
                        ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := 'BAIXADO POR PAGAMENTO'
                  else if LJsonObject.AsJSONObject['titulo'].AsString['situacao_banrisul'] = 'D' then
                        ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := 'BAIXADO POR DEVOLUÇÃO'
                  else if LJsonObject.AsJSONObject['titulo'].AsString['situacao_banrisul'] = 'L' then
                        ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := 'LIQUIDADO'
                  else if LJsonObject.AsJSONObject['titulo'].AsString['situacao_banrisul'] = 'R' then
                        ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := 'REEMBOLSADO'
                  else if LJsonObject.AsJSONObject['titulo'].AsString['situacao_banrisul'] = 'T' then
                        ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := 'TRANSFERIDO PARA CL'
                  else if LJsonObject.AsJSONObject['titulo'].AsString['situacao_banrisul'] = 'P' then
                        ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := 'PROTESTADO';

                  if ARetornoWS.DadosRet.TituloRet.codigoEstadoTituloCobranca = 'L' then
                  begin
                    ARetornoWS.DadosRet.TituloRet.Contrato                   := LJsonObject.AsJSONObject['titulo'].AsJSONObject['beneficiario'].AsString['codigo'];
                    ARetornoWS.DadosRet.TituloRet.DataMovimento              := DateBBtoDateTime(LJsonObject.AsJSONObject['titulo'].AsString['data_emissao']);
                    ARetornoWS.DadosRet.TituloRet.Vencimento                 := DateBBtoDateTime(LJsonObject.AsJSONObject['titulo'].AsString['data_vencimento']);
                    ARetornoWS.DadosRet.TituloRet.DataDocumento              := DateBBtoDateTime(LJsonObject.AsJSONObject['titulo'].AsString['data_emissao']);
                    ARetornoWS.DadosRet.TituloRet.DataCredito                := DateBBtoDateTime(LJsonObject.AsJSONObject['titulo'].AsJSONObject['operacoes'].AsString['data_credito']);
                    ARetornoWS.DadosRet.TituloRet.DataBaixa                  := DateBBtoDateTime(LJsonObject.AsJSONObject['titulo'].AsJSONObject['operacoes'].AsString['data_pagamento']);
                    ARetornoWS.DadosRet.TituloRet.ValorAtual                 := LJsonObject.AsJSONObject['titulo'].AsJSONObject['operacoes'].AsFloat['valor_pagamento'];
                    ARetornoWS.DadosRet.TituloRet.ValorPago                  := LJsonObject.AsJSONObject['titulo'].AsJSONObject['operacoes'].AsFloat['valor_cobrado'];

                    ARetornoWS.DadosRet.TituloRet.ValorRecebido              := LJsonObject.AsJSONObject['titulo'].AsJSONObject['operacoes'].AsFloat['valor_creditado_debitado'];
                    ARetornoWS.DadosRet.TituloRet.ValorMoraJuros             := LJsonObject.AsJSONObject['titulo'].AsJSONObject['operacoes'].AsFloat['valor_juros_recebido'];
                    ARetornoWS.DadosRet.TituloRet.ValorDesconto              := LJsonObject.AsJSONObject['titulo'].AsJSONObject['operacoes'].AsFloat['valor_desconto_utilizado'];

                    // Na documentação não tem informações de retorno com desconto
                    if (ARetornoWS.DadosRet.TituloRet.ValorRecebido < ARetornoWS.DadosRet.TituloRet.ValorDocumento) and
                       (ARetornoWS.DadosRet.TituloRet.ValorDesconto <= 0) then
                       ARetornoWS.DadosRet.TituloRet.ValorDesconto              := ARetornoWS.DadosRet.TituloRet.ValorDocumento -  ARetornoWS.DadosRet.TituloRet.ValorRecebido;



                    //ARetornoWS.DadosRet.TituloRet.ValorOutrosCreditos        := LJsonObject.AsJSONObject['titulo'].AsJSONObject['operacoes'].AsFloat['valorOutroRecebido'];
                    //ARetornoWS.DadosRet.TituloRet.ValorIOF                   := LJsonObject.AsJSONObject['titulo'].AsJSONObject['operacoes'].AsFloat['valorImpostoSobreOprFinanceirasRecebidoTitulo'];
                    //ARetornoWS.DadosRet.TituloRet.ValorAbatimento            := LJsonObject.AsJSONObject['titulo'].AsJSONObject['operacoes'].AsFloat['valorAbatimentoTotal'];
                    //ARetornoWS.DadosRet.TituloRet.MultaValorFixo             := true;
                    //ARetornoWS.DadosRet.TituloRet.PercentualMulta            := LJsonObject.AsJSONObject['titulo'].AsJSONObject['operacoes'].AsFloat['valorMultaRecebido'];
                    //ARetornoWS.DadosRet.TituloRet.CodigoOcorrenciaCartorio   := IntToStr(LJsonObject.AsJSONObject['titulo'].AsJSONObject['operacoes'].AsInteger['codigoOcorrenciaCartorio']);

                  end;

                  ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF    :=  LJsonObject.AsJSONObject['titulo'].AsJSONObject['pagador'].AsString['cpf_cnpj'];
                  ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado :=  LJsonObject.AsJSONObject['titulo'].AsJSONObject['pagador'].AsString['nome'];
                  ARetornoWS.DadosRet.TituloRet.Sacado.Logradouro :=  LJsonObject.AsJSONObject['titulo'].AsJSONObject['pagador'].AsString['endereco'];
                  ARetornoWS.DadosRet.TituloRet.Sacado.CEP        :=  LJsonObject.AsJSONObject['titulo'].AsJSONObject['pagador'].AsString['cep'];
                  ARetornoWS.DadosRet.TituloRet.Sacado.Cidade     :=  LJsonObject.AsJSONObject['titulo'].AsJSONObject['pagador'].AsString['cidade'];
                  ARetornoWS.DadosRet.TituloRet.Sacado.UF         :=  LJsonObject.AsJSONObject['titulo'].AsJSONObject['pagador'].AsString['uf'];


               end
               else if (LTipoOperacao = tpBaixa) then
               begin

                  ARetornoWS.DadosRet.IDBoleto.CodBarras := LJsonObject.AsJSONObject['titulo'].AsString['codigo_barras'];
                  ARetornoWS.DadosRet.IDBoleto.LinhaDig  := LJsonObject.AsJSONObject['titulo'].AsString['linha_digitavel'];
                  ARetornoWS.DadosRet.IDBoleto.NossoNum  := TrataNossoNumero(LJsonObject.AsJSONObject['titulo'].AsString['nosso_numero']);

                  ARetornoWS.DadosRet.TituloRet.CodBarras   := ARetornoWS.DadosRet.IDBoleto.CodBarras;
                  ARetornoWS.DadosRet.TituloRet.LinhaDig    := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
                  ARetornoWS.DadosRet.TituloRet.NossoNumero := ARetornoWS.DadosRet.IDBoleto.NossoNum;

                  ARetornoWS.DadosRet.TituloRet.Contrato  := LJsonObject.AsJSONObject['titulo'].AsJSONObject['beneficiario'].AsString['codigo'];
               end;

            end;

         except
            Result := False;
         end;
      finally
         LJsonObject.free;
      end;
   end
   else
   begin
      case HTTPResultCode of
         404:
            begin
               LMensagemRejeicao := ARetornoWS.CriarRejeicaoLista;
               LMensagemRejeicao.Codigo := '404';
               LMensagemRejeicao.Mensagem :=
                 'NÃO ENCONTRADO. O servidor não conseguiu encontrar o recurso solicitado.';
            end;
         503:
            begin
               LMensagemRejeicao := ARetornoWS.CriarRejeicaoLista;
               LMensagemRejeicao.Codigo := '503';
               LMensagemRejeicao.Versao := 'ERRO INTERNO BB';
               LMensagemRejeicao.Mensagem :=
                 'SERVIÇO INDISPONÍVEL. O servidor está impossibilitado de lidar com a requisição no momento. Tente mais tarde.';
               LMensagemRejeicao.Ocorrencia :=
                 'ERRO INTERNO nos servidores do Banco Banrisul.';
            end;
      end;
   end;
end;

function TRetornoEnvio_Banrisul.RetornoEnvio(const AIndex: Integer): Boolean;
begin

  Result:=inherited RetornoEnvio(AIndex);

end;

function TRetornoEnvio_Banrisul.TrataNossoNumero( const ANossoNumero: string): string;
begin
   if NaoEstaVazio(ANossoNumero) then
       Result := PadLeft(ANossoNumero,10,'0');
end;

end.
