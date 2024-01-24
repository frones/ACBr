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
    function RetornaCodigoOcorrencia(pSituacaoGeralBoleto: string): String;

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
  AJSonResp,  AJSonRespDadosIndivBlt,  AJSonRespDadosIndivBltPagamento: TJsonArray;
  I, J: Integer;
  TipoOperacao : TOperacao;
begin
  Result := True;
  TipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;
  ARetornoWS.HTTPResultCode  := HTTPResultCode;
  ARetornoWS.JSONEnvio       := EnvWs;
  ARetornoWS.Header.Operacao := TipoOperacao;

  if Trim(RetWS) <> '' then
  begin
    try
      AJSon := TJson.Create;
      try
        AJSon.Parse(RetWS);
        ARetornoWS.JSON := AJson.Stringify;

        if TipoOperacao = tpInclui then
        begin
          //retorna quando houver erro
          if (AJson.Values['codigo'].AsString <> '') or (AJson.Values['mensagem'].AsString <> '') or
             (AJson.Values['codigo_erro'].AsString <> '') or (AJson.Values['mensagem_erro'].AsString <> '') then
          begin
            ARetornoWS.CodRetorno := AJson.Values['codigo'].AsString;
            if StrToIntDef(AJson.Values['codigo'].AsString, 0)> 0 then
            begin
              ARetornoWS.MsgRetorno := AJson.Values['mensagem'].AsString;
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
              ARejeicao := ARetornoWS.CriarRejeicaoLista;
              ARejeicao.Codigo := AJson.Values['codigo_erro'].AsString;
              ARejeicao.Mensagem := AJson.Values['mensagem_erro'].AsString;
            end;
          end
          else
          begin

            if AJson.Values['data'].IsEmpty then
              AJSonDadoBoleto := AJSon.Values['dado_boleto'].AsObject
            else
              AJSonDadoBoleto  := AJson.Values['data'].AsObject.Values['dado_boleto'].AsObject;

            ARetornoWS.DadosRet.TituloRet.Carteira       := AJSonDadoBoleto.Values['codigo_carteira'].AsString;
            ARetornoWS.DadosRet.TituloRet.ValorDocumento := StrToFloatDef( AJSonDadoBoleto.Values['valor_total_titulo'].AsString, 0)/100;
            ARetornoWS.DadosRet.TituloRet.EspecieDoc     := AJSonDadoBoleto.Values['codigo_especie'].AsString;
            ARetornoWS.DadosRet.TituloRet.DataDocumento  := StringToDateTimeDef(AJSonDadoBoleto.Values['data_emissao'].AsString, 0, 'yyyy-mm-dd');

            ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado     := AJSonDadoBoleto.Values['pagador'].AsObject.Values['pessoa'].AsObject.Values['nome_pessoa'].AsString;
            if (ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado = '') then
              ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado   := AJSonDadoBoleto.Values['pagador'].AsObject.Values['pessoa'].AsObject.Values['nome_razao_social_pagador'].AsString;

            if AJSonDadoBoleto.Values['pagador'].AsObject.Values['pessoa'].AsObject.Values['tipo_pessoa'].AsObject.Values['codigo_tipo_pessoa'].AsString = 'F' then
              ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF        := AJSonDadoBoleto.Values['pagador'].AsObject.Values['pessoa'].AsObject.Values['tipo_pessoa'].AsObject.Values['numero_cadastro_pessoa_fisica'].AsString
            else
              ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF        := AJSonDadoBoleto.Values['pagador'].AsObject.Values['pessoa'].AsObject.Values['tipo_pessoa'].AsObject.Values['numero_cadastro_nacional_pessoa_juridica'].AsString;

            ARetornoWS.DadosRet.TituloRet.Sacado.Logradouro := AJSonDadoBoleto.Values['pagador'].AsObject.Values['endereco'].AsObject.Values['nome_logradouro'].AsString;
            ARetornoWS.DadosRet.TituloRet.Sacado.Bairro     := AJSonDadoBoleto.Values['pagador'].AsObject.Values['endereco'].AsObject.Values['nome_bairro'].AsString;
            ARetornoWS.DadosRet.TituloRet.Sacado.Cidade     := AJSonDadoBoleto.Values['pagador'].AsObject.Values['endereco'].AsObject.Values['nome_cidade'].AsString;
            ARetornoWS.DadosRet.TituloRet.Sacado.UF         := AJSonDadoBoleto.Values['pagador'].AsObject.Values['endereco'].AsObject.Values['sigla_UF'].AsString;
            ARetornoWS.DadosRet.TituloRet.Sacado.Cep        := AJSonDadoBoleto.Values['pagador'].AsObject.Values['endereco'].AsObject.Values['numero_CEP'].AsString;

             ARetornoWS.DadosRet.TituloRet.SacadoAvalista.CNPJCPF         := AJSonDadoBoleto.Values['sacador_avalista'].AsObject.Values['numero_cadastro_pessoa_fisica'].AsString;
             ARetornoWS.DadosRet.TituloRet.SacadoAvalista.NomeAvalista    := AJSonDadoBoleto.Values['sacador_avalista'].AsObject.Values['nome_pessoa'].AsString;
             if ARetornoWS.DadosRet.TituloRet.SacadoAvalista.NomeAvalista = '' then
               ARetornoWS.DadosRet.TituloRet.SacadoAvalista.NomeAvalista  := AJSonDadoBoleto.Values['sacador_avalista'].AsObject.Values['nome_fantasia'].AsString;


               AJSonRespDadosIndivBlt := AJSonDadoBoleto.Values['dados_individuais_boleto'].AsArray;
              if AJSonRespDadosIndivBlt.Count < 1 then
              begin
                   AJSonRespDadosIndivBlt := AJSonDadoBoleto.Values['dados_individuais_boleto'].AsArray;
              end;

              For I := 0 to AJSonRespDadosIndivBlt.Count-1 do
              begin
                ARetornoWS.DadosRet.IDBoleto.IDBoleto  := AJSonRespDadosIndivBlt[I].AsObject.Values['id_boleto_individual'].AsString;
                ARetornoWS.DadosRet.IDBoleto.CodBarras := AJSonRespDadosIndivBlt[I].AsObject.Values['codigo_barras'].AsString;
                ARetornoWS.DadosRet.IDBoleto.LinhaDig  := AJSonRespDadosIndivBlt[I].AsObject.Values['numero_linha_digitavel'].AsString;
                ARetornoWS.DadosRet.IDBoleto.NossoNum  := AJSonRespDadosIndivBlt[I].AsObject.Values['numero_nosso_numero'].AsString;

                ARetornoWS.DadosRet.TituloRet.Vencimento           := StringToDateTimeDef(AJSonRespDadosIndivBlt[I].AsObject.Values['data_vencimento'].AsString, 0, 'yyyy-mm-dd');
                ARetornoWS.DadosRet.TituloRet.NossoNumero          := AJSonRespDadosIndivBlt[I].AsObject.Values['numero_nosso_numero'].AsString;
                ARetornoWS.DadosRet.TituloRet.SeuNumero            := AJSonRespDadosIndivBlt[I].AsObject.Values['texto_seu_numero'].AsString;
                ARetornoWS.DadosRet.TituloRet.CodBarras            := AJSonRespDadosIndivBlt[I].AsObject.Values['codigo_barras'].AsString;
                ARetornoWS.DadosRet.TituloRet.LinhaDig             := AJSonRespDadosIndivBlt[I].AsObject.Values['numero_linha_digitavel'].AsString;
                ARetornoWS.DadosRet.TituloRet.DataProcessamento    := StringToDateTimeDef(AJSonRespDadosIndivBlt[I].AsObject.Values['data_processamento'].AsString, 0, 'yyyy-mm-dd');
                ARetornoWS.DadosRet.TituloRet.UsoBanco             := AJSonRespDadosIndivBlt[I].AsObject.Values['uso_banco'].AsString;
                ARetornoWS.DadosRet.TituloRet.ValorDesconto        := StrToFloatDef( AJSonRespDadosIndivBlt[I].AsObject.Values['valor_desconto'].AsString, 0);
                ARetornoWS.DadosRet.TituloRet.ValorDespesaCobranca := StrToFloatDef(AJSonRespDadosIndivBlt[I].AsObject.Values['valor_outra_deducao'].AsString, 0);
                ARetornoWS.DadosRet.TituloRet.ValorMoraJuros       := StrToFloatDef( AJSonRespDadosIndivBlt[I].AsObject.Values['valor_juro_multa'].AsString, 0);
                ARetornoWS.DadosRet.TituloRet.ValorOutrosCreditos  := StrToFloatDef( AJSonRespDadosIndivBlt[I].AsObject.Values['valor_outro_acrescimo'].AsString, 0);
                ARetornoWS.DadosRet.TituloRet.ValorPago            := StrToFloatDef( AJSonRespDadosIndivBlt[I].AsObject.Values['valor_total_cobrado'].AsString, 0);

                ARetornoWS.DadosRet.TituloRet.Informativo.Add( AJSonRespDadosIndivBlt[I].AsObject.Values['texto_informacao_cliente_beneficiario'].AsString );
                ARetornoWS.DadosRet.TituloRet.Mensagem.Add(AJSonRespDadosIndivBlt[I].AsObject.Values['local_pagamento'].AsString);
              end;
          end;
        end else
        if TipoOperacao = tpConsultaDetalhe then
        begin
          if HTTPResultCode > 299 then
          begin
            if ( AJson.StructType = jsObject ) then
              if (AJson.Values['codigo'].AsString <> '') or (AJson.Values['mensagem'].AsString <> '') then
              begin
                CodRetorno := strtointdef(AJson.Values['codigo'].AsString, 0);
                if CodRetorno < 1 then
                   CodRetorno := strtointdef(AJson.Values['codigo'].AsString, 0);

                 ARejeicao            := ARetornoWS.CriarRejeicaoLista;
                 ARejeicao.Codigo     := inttostr(CodRetorno);
                 ARejeicao.Mensagem   := AJson.Values['mensagem'].AsString;
              end;
          end;
          //retorna quando tiver sucesso
          if (ARetornoWS.ListaRejeicao.Count = 0) then
          begin
            if AJson.Values['data'].IsEmpty then
              AJSonResp := AJSon.Values['dado_boleto'].AsArray
            else
              AJSonResp  := AJson.Values['data'].AsArray;

            if AJSonResp.Count = 0 then
            begin
              ARejeicao            := ARetornoWS.CriarRejeicaoLista;
              ARejeicao.Codigo     := '00';
              ARejeicao.Mensagem   := 'Lista de retorno vazia!';
            end;

            for I := 0 to Pred(AJSonResp.Count) do
            begin
              //if I > 0 then
              //  ARetornoWS := ACBrBoleto.CriarRetornoWebNaLista;

              AJSonObject  := AJSonResp[I].AsObject;
              ARetornoWS.DadosRet.IDBoleto.IDBoleto        := AJSonObject.Values['id_boleto'].AsString;

              with AJSonObject.Values['dado_boleto'].AsObject do
              begin
                ARetornoWS.DadosRet.TituloRet.DataRegistro         := DateToDateTimeItau(Values['data_emissao'].AsString);
                ARetornoWS.DadosRet.TituloRet.DataDocumento        := DateToDateTimeItau(Values['data_emissao'].AsString);
                AJSonRespDadosIndivBlt := Values['dados_individuais_boleto'].AsArray;
                For j := 0 to AJSonRespDadosIndivBlt.Count-1 do
                begin
                  with AJSonRespDadosIndivBlt[j].AsObject do
                  begin
                    ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := Values['situacao_geral_boleto'].AsString;
                    ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := RetornaCodigoOcorrencia(UpperCase(Values['situacao_geral_boleto'].AsString));
                    ARetornoWS.DadosRet.TituloRet.NossoNumero          := Values['numero_nosso_numero'].AsString;
                    ARetornoWS.DadosRet.TituloRet.Vencimento           := StringToDateTimeDef(Values['data_vencimento'].AsString, 0, 'yyyy-mm-dd');
                    ARetornoWS.DadosRet.TituloRet.SeuNumero            := Values['texto_seu_numero'].AsString;
                    ARetornoWS.DadosRet.TituloRet.CodBarras            := Values['codigo_barras'].AsString;
                    ARetornoWS.DadosRet.TituloRet.LinhaDig             := Values['numero_linha_digitavel'].AsString;
                    ARetornoWS.DadosRet.TituloRet.ValorDocumento       := Values['valor_titulo'].AsNumber;
                   // ARetornoWS.DadosRet.TituloRet.Mensagem.Add(Values['mensagens_cobranca'].AsString);
                  end;
                end;

                AJSonRespDadosIndivBltPagamento := Values['pagamentos_cobranca'].AsArray;
                For j := 0 to AJSonRespDadosIndivBltPagamento.Count-1 do
                begin
                  with AJSonRespDadosIndivBltPagamento[j].AsObject do
                  begin
                    ARetornoWS.DadosRet.TituloRet.DataProcessamento    := StringToDateTimeDef(Values['data_inclusao_pagamento'].AsString, 0, 'yyyy-mm-dd');
                    ARetornoWS.DadosRet.TituloRet.ValorPago            := StrToFloatDef( StringReplace(AJSonRespDadosIndivBltPagamento[j].AsObject.values['valor_pago_total_cobranca'].AsString,'.',',',[rfReplaceAll]), 0);
                    if ARetornoWS.DadosRet.TituloRet.ValorPago > ARetornoWS.DadosRet.TituloRet.ValorDocumento then
                       ARetornoWS.DadosRet.TituloRet.ValorOutrasDespesas := (ARetornoWS.DadosRet.TituloRet.ValorPago - ARetornoWS.DadosRet.TituloRet.ValorDocumento)

                  end;
                end;

                with Values['baixa'].AsObject do
                begin
                  ARetornoWS.DadosRet.TituloRet.DataBaixa              := StringToDateTimeDef(Values['data_inclusao_alteracao_baixa'].AsString, 0, 'yyyy-mm-dd');
                  ARetornoWS.DadosRet.TituloRet.Mensagem.Text          := Values['motivo_baixa'].AsString;
                  if (UpperCase(Values['motivo_baixa'].AsString) = 'BAIXA POR TER SIDO LIQUIDADO') then
                  begin
                    ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca       := UpperCase(Values['motivo_baixa'].AsString);
                    ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := RetornaCodigoOcorrencia(UpperCase(Values['motivo_baixa'].AsString));
                  end;

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

function TRetornoEnvio_Itau_API.RetornaCodigoOcorrencia(pSituacaoGeralBoleto: string) : String;
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

