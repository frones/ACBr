{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  José M S Junior, Victor Hugo Gonzales          }
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

unit ACBrBoletoRet_Itau_API;

{ TRetornoEnvio_Itau_API_API }

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

{ TRetornoEnvio_Itau_API_API }

 TRetornoEnvio_Itau_API = class(TRetornoEnvioREST)
  private
    function DateToDateTimeItau(const AValue: String): TDateTime;

  public
    constructor Create(ABoletoWS: TACBrBoleto); override;
    destructor  Destroy; Override;
    function LerListaRetorno: Boolean; override;
    function LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;override;
    function RetornoEnvio(const AIndex: Integer): Boolean; override;

  end;

implementation

uses
 ACBrUtil.Strings, ACBrUtil.DateTime,ACBrBoletoConversao;

{ TRetornoEnvio }

constructor TRetornoEnvio_Itau_API.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);

end;

destructor TRetornoEnvio_Itau_API.Destroy;
begin
  inherited Destroy;
end;

function TRetornoEnvio_Itau_API.LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;
var
  AJson: TJson;
  AJSonRejeicao, AJSonObject, AJSonDadoBoleto: TJsonObject;
  ARejeicao: TACBrBoletoRejeicao;
  AJSonResp,  AJSonRespDadosIndivBlt: TJsonArray;
  I: Integer;
  TipoOperacao : TOperacao;
begin
  Result := True;
  if RetWS <> '' then
  begin
    try
      with ARetornoWS do
      begin
        TipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;
        ARetornoWS.HTTPResultCode := HTTPResultCode;
        ARetornoWS.JSONEnvio      := EnvWs;
        ARetornoWS.Header.Operacao := TipoOperacao;
        AJSon := TJson.Create;
        try
          AJSon.Parse(RetWS);
          JSON           := AJson.Stringify;

          //retorna quando houver erro
          if (AJson.Values['codigo'].AsString <> '') or (AJson.Values['mensagem'].AsString <> '') or
             (AJson.Values['codigo_erro'].AsString <> '') or (AJson.Values['mensagem_erro'].AsString <> '') then
          begin
            CodRetorno := AJson.Values['codigo'].AsString;
            if StrToIntDef(AJson.Values['codigo'].AsString, 0)> 0 then
            begin
              MsgRetorno := AJson.Values['mensagem'].AsString;
              AJSonResp := AJson.Values['campos'].AsArray;
              for I := 0 to Pred(AJSonResp.Count) do
              begin
                AJSonRejeicao        := AJSonResp[I].AsObject;
                ARejeicao            := ARetornoWS.CriarRejeicaoLista;
                ARejeicao.Codigo     := AJSonRejeicao.Values['codigo'].AsString;
                ARejeicao.Versao     := AJSonRejeicao.Values['versao'].AsString;
                ARejeicao.Mensagem   := AJSonRejeicao.Values['mensagem'].AsString;
                ARejeicao.Ocorrencia := AJSonRejeicao.Values['ocorrencia'].AsString;
                ARejeicao.Valor      := AJSonRejeicao.Values['valor'].AsString;
              end;
            end
            else if StrToIntDef(AJson.Values['codigo_erro'].AsString, 0) > 0 then
            begin
              ARejeicao := CriarRejeicaoLista;
              ARejeicao.Codigo := AJson.Values['codigo_erro'].AsString;
              ARejeicao.Mensagem := AJson.Values['mensagem_erro'].AsString;
            end;
          end
          else
          begin

            //retorna quando tiver sucesso
            AJSonDadoBoleto := AJson.Values['dado_boleto'].AsObject;
            DadosRet.TituloRet.Carteira       := AJSonDadoBoleto.Values['codigo_carteira'].AsString;
            DadosRet.TituloRet.ValorDocumento := StrToFloatDef( AJSonDadoBoleto.Values['valor_total_titulo'].AsString, 0)/100;
            DadosRet.TituloRet.EspecieDoc     := AJSonDadoBoleto.Values['codigo_especie'].AsString;
            DadosRet.TituloRet.DataDocumento  := StringToDateTimeDef(AJSonDadoBoleto.Values['data_emissao'].AsString, 0, 'yyyy-mm-dd');

            DadosRet.TituloRet.Sacado.NomeSacado     := AJSonDadoBoleto.Values['pagador'].AsObject.Values['pessoa'].AsObject.Values['nome_pessoa'].AsString;
            if (DadosRet.TituloRet.Sacado.NomeSacado = '') then
              DadosRet.TituloRet.Sacado.NomeSacado   := AJSonDadoBoleto.Values['pagador'].AsObject.Values['pessoa'].AsObject.Values['nome_razao_social_pagador'].AsString;

            if AJSonDadoBoleto.Values['pagador'].AsObject.Values['pessoa'].AsObject.Values['tipo_pessoa'].AsObject.Values['codigo_tipo_pessoa'].AsString = 'F' then
              DadosRet.TituloRet.Sacado.CNPJCPF        := AJSonDadoBoleto.Values['pagador'].AsObject.Values['pessoa'].AsObject.Values['tipo_pessoa'].AsObject.Values['numero_cadastro_pessoa_fisica'].AsString
            else
              DadosRet.TituloRet.Sacado.CNPJCPF        := AJSonDadoBoleto.Values['pagador'].AsObject.Values['pessoa'].AsObject.Values['tipo_pessoa'].AsObject.Values['numero_cadastro_nacional_pessoa_juridica'].AsString;

            DadosRet.TituloRet.Sacado.Logradouro := AJSonDadoBoleto.Values['pagador'].AsObject.Values['endereco'].AsObject.Values['nome_logradouro'].AsString;
            DadosRet.TituloRet.Sacado.Bairro     := AJSonDadoBoleto.Values['pagador'].AsObject.Values['endereco'].AsObject.Values['nome_bairro'].AsString;
            DadosRet.TituloRet.Sacado.Cidade     := AJSonDadoBoleto.Values['pagador'].AsObject.Values['endereco'].AsObject.Values['nome_cidade'].AsString;
            DadosRet.TituloRet.Sacado.UF         := AJSonDadoBoleto.Values['pagador'].AsObject.Values['endereco'].AsObject.Values['sigla_UF'].AsString;
            DadosRet.TituloRet.Sacado.Cep        := AJSonDadoBoleto.Values['pagador'].AsObject.Values['endereco'].AsObject.Values['numero_CEP'].AsString;

             DadosRet.TituloRet.SacadoAvalista.CNPJCPF         := AJSonDadoBoleto.Values['sacador_avalista'].AsObject.Values['numero_cadastro_pessoa_fisica'].AsString;
             DadosRet.TituloRet.SacadoAvalista.NomeAvalista    := AJSonDadoBoleto.Values['sacador_avalista'].AsObject.Values['nome_pessoa'].AsString;
             if DadosRet.TituloRet.SacadoAvalista.NomeAvalista = '' then
               DadosRet.TituloRet.SacadoAvalista.NomeAvalista  := AJSonDadoBoleto.Values['sacador_avalista'].AsObject.Values['nome_fantasia'].AsString;


               AJSonRespDadosIndivBlt := AJSonDadoBoleto.Values['dados_individuais_boleto'].AsArray;
              if AJSonRespDadosIndivBlt.Count < 1 then
              begin
                   AJSonRespDadosIndivBlt := AJSonDadoBoleto.Values['dados_individuais_boleto'].AsArray;
              end;

              For I := 0 to AJSonRespDadosIndivBlt.Count-1 do
              begin
                with AJSonRespDadosIndivBlt[I].AsObject do
                begin
                  DadosRet.IDBoleto.IDBoleto  := AJSonDadoBoleto.Values['id_boleto_individual'].AsString;
                  DadosRet.IDBoleto.CodBarras := AJSonDadoBoleto.Values['codigo_barras'].AsString;
                  DadosRet.IDBoleto.LinhaDig  := AJSonDadoBoleto.Values['numero_linha_digitavel'].AsString;
                  DadosRet.IDBoleto.NossoNum  := AJSonDadoBoleto.Values['numero_nosso_numero'].AsString;

                  DadosRet.TituloRet.Vencimento           := StringToDateTimeDef(Values['data_vencimento'].AsString, 0, 'yyyy-mm-dd');
                  DadosRet.TituloRet.NossoNumero          := Values['numero_nosso_numero'].AsString;
                  DadosRet.TituloRet.SeuNumero            := Values['numero_nosso_numero'].AsString;
                  DadosRet.TituloRet.CodBarras            := Values['codigo_barras'].AsString;
                  DadosRet.TituloRet.LinhaDig             := Values['numero_linha_digitavel'].AsString;
                  DadosRet.TituloRet.DataProcessamento    := StringToDateTimeDef(Values['data_processamento'].AsString, 0, 'yyyy-mm-dd');
                  DadosRet.TituloRet.UsoBanco             := Values['uso_banco'].AsString;
                  DadosRet.TituloRet.ValorDesconto        := StrToFloatDef( Values['valor_desconto'].AsString, 0);
                  DadosRet.TituloRet.ValorDespesaCobranca := StrToFloatDef( Values['valor_outra_deducao'].AsString, 0);
                  DadosRet.TituloRet.ValorMoraJuros       := StrToFloatDef( Values['valor_juro_multa'].AsString, 0);
                  DadosRet.TituloRet.ValorOutrosCreditos  := StrToFloatDef( Values['valor_outro_acrescimo'].AsString, 0);
                  DadosRet.TituloRet.ValorPago            := StrToFloatDef(  Values['valor_total_cobrado'].AsString, 0);

                  DadosRet.TituloRet.Informativo.Add( Values['texto_informacao_cliente_beneficiario'].AsString );
                  DadosRet.TituloRet.Mensagem.Add(Values['local_pagamento'].AsString);
                end;
              end;
            //end;
          end;

        finally
          AJson.free;
        end;

      end;
    except
      Result := False;
    end;

  end;

end;

function TRetornoEnvio_Itau_API.RetornoEnvio(const AIndex: Integer): Boolean;
begin

  Result:=inherited RetornoEnvio(AIndex);

end;

function TRetornoEnvio_Itau_API.DateToDateTimeItau(const AValue: String): TDateTime;
begin
  Result :=EncodeDataHora(StringReplace(AValue,'-','/',[rfReplaceAll]));
end;

function RetornaCodigoOcorrencia(pSituacaoGeralBoleto: string) : String;
begin
  if pSituacaoGeralBoleto  = 'EM ABERTO' then
    Result := '01'
  else if pSituacaoGeralBoleto  = 'AGUARDANDO PAGAMENTO' then
    Result := '02'
  else if (pSituacaoGeralBoleto  = 'PAGO') or
          (pSituacaoGeralBoleto  = 'BAIXA POR TER SIDO LIQUIDADO') then
    Result := '06'
  else if pSituacaoGeralBoleto  = 'PAGAMENTO DEVOLVIDO' then
    Result := '08'
  else if (pSituacaoGeralBoleto  = 'BAIXADO') or
          (pSituacaoGeralBoleto  = 'BAIXADA')then
    Result := '09'
  else
    Result := '99';

end;

function TRetornoEnvio_Itau_API.LerListaRetorno: Boolean;
var
  ListaRetorno: TACBrBoletoRetornoWS;
  AJson: TJson;
  AJSonObject: TJsonObject;
  ARejeicao: TACBrBoletoRejeicao;
  AJsonBoletos, AJSonRespDadosIndivBlt: TJsonArray;
  I, j: Integer;
begin
  Result := True;
  ListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;
  ListaRetorno.HTTPResultCode := HTTPResultCode;
  ListaRetorno.JSONEnvio      := EnvWs;
  if RetWS <> '' then
  begin
    try
      AJSon := TJson.Create;
      try
        AJSon.Parse(RetWS);
        if HTTPResultCode > 299 then
        begin
          if ( AJson.StructType = jsObject ) then
            if (AJson.Values['codigo'].AsString <> '') or (AJson.Values['mensagem'].AsString <> '') then
            begin
              CodRetorno := strtointdef(AJson.Values['codigo'].AsString, 0);
              if CodRetorno < 1 then
                 CodRetorno := strtointdef(AJson.Values['codigo'].AsString, 0);

               ARejeicao            := ListaRetorno.CriarRejeicaoLista;
               ARejeicao.Codigo     := inttostr(CodRetorno);
               ARejeicao.Mensagem   := AJson.Values['mensagem'].AsString;
              // ARejeicao.Ocorrencia := AJSonRejeicao.Values['codigoRetorno'].AsString;
            end;

        end;

        //retorna quando tiver sucesso
        if (ListaRetorno.ListaRejeicao.Count = 0) then
        begin
          AJsonBoletos := AJson.Values['data'].AsArray;
          if AJsonBoletos.Count = 0 then
          begin
            ARejeicao            := ListaRetorno.CriarRejeicaoLista;
            ARejeicao.Codigo     := '00';
            ARejeicao.Mensagem   := 'Lista de retorno vazia!';
          end;
          
          for I := 0 to Pred(AJsonBoletos.Count) do
          begin
            if I > 0 then
              ListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;

            AJSonObject  := AJsonBoletos[I].AsObject;
            ListaRetorno.DadosRet.IDBoleto.IDBoleto        := AJSonObject.Values['id_boleto'].AsString;



            with AJSonObject.Values['dado_boleto'].AsObject do
            begin
              ListaRetorno.DadosRet.TituloRet.DataRegistro         := DateToDateTimeItau(Values['data_emissao'].AsString);
              AJSonRespDadosIndivBlt := Values['dados_individuais_boleto'].AsArray;
              For j := 0 to AJSonRespDadosIndivBlt.Count-1 do
              begin
                with AJSonRespDadosIndivBlt[j].AsObject do
                begin
                  ListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca := Values['situacao_geral_boleto'].AsString;
                  ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := RetornaCodigoOcorrencia(UpperCase(Values['situacao_geral_boleto'].AsString));
                  ListaRetorno.DadosRet.TituloRet.NossoNumero          := Values['numero_nosso_numero'].AsString;
                  ListaRetorno.DadosRet.TituloRet.Vencimento           := StringToDateTimeDef(Values['data_vencimento'].AsString, 0, 'yyyy-mm-dd');
                  ListaRetorno.DadosRet.TituloRet.SeuNumero            := Values['texto_seu_numero'].AsString;
                  ListaRetorno.DadosRet.TituloRet.CodBarras            := Values['codigo_barras'].AsString;
                  ListaRetorno.DadosRet.TituloRet.LinhaDig             := Values['numero_linha_digitavel'].AsString;
                  ListaRetorno.DadosRet.TituloRet.ValorDocumento       := AJSonObject.Values['valor_titulo'].AsNumber;
                 // ListaRetorno.DadosRet.TituloRet.Mensagem.Add(Values['mensagens_cobranca'].AsString);
                end;
              end;

              AJSonRespDadosIndivBlt := Values['pagamentos_cobranca'].AsArray;
              For j := 0 to AJSonRespDadosIndivBlt.Count-1 do
              begin
                with AJSonRespDadosIndivBlt[j].AsObject do
                begin
                  ListaRetorno.DadosRet.TituloRet.DataProcessamento    := StringToDateTimeDef(Values['data_inclusao_pagamento'].AsString, 0, 'yyyy-mm-dd');
                  ListaRetorno.DadosRet.TituloRet.ValorPago            := StrToFloatDef( Values['valor_pago_total_cobranca'].AsString, 0) / 100;
                end;
              end;

              with Values['baixa'].AsObject do
              begin
                ListaRetorno.DadosRet.TituloRet.DataBaixa              := StringToDateTimeDef(Values['data_inclusao_alteracao_baixa'].AsString, 0, 'yyyy-mm-dd');
                ListaRetorno.DadosRet.TituloRet.Mensagem.Text          := Values['motivo_baixa'].AsString;
                if (UpperCase(Values['motivo_baixa'].AsString) = 'BAIXA POR TER SIDO LIQUIDADO') then
                begin
                  ListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca       := UpperCase(Values['motivo_baixa'].AsString);
                  ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := RetornaCodigoOcorrencia(UpperCase(Values['motivo_baixa'].AsString));
                end;

              end;
            end;

          end;
        end;

      finally
        AJson.free;
      end;

    except
      Result := False;
    end;

  end else
  begin
    case HTTPResultCode of
      404 :
        begin
          ARejeicao            := ListaRetorno.CriarRejeicaoLista;
          ARejeicao.Codigo     := '404';
          ARejeicao.Mensagem   := 'O servidor não conseguiu encontrar o recurso solicitado.';
        end;
      503 :
        begin
          ARejeicao            := ListaRetorno.CriarRejeicaoLista;
          ARejeicao.Codigo     := '503';
          ARejeicao.Versao     := 'ERRO INTERNO ITAU';
          ARejeicao.Mensagem   := 'SERVIÇO INDISPONÍVEL. O servidor está impossibilitado de lidar com a requisição no momento. Tente mais tarde.';
          ARejeicao.Ocorrencia := 'ERRO INTERNO nos servidores do Banco do Itaú.';
        end;
    end;

  end;
end;

end.

