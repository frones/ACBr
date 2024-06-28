{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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
  ACBrBoleto,
  ACBrBoletoRetorno,
  ACBrBoletoWS.Rest;

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
  SysUtils,
  ACBrJSON,
  ACBrBoletoConversao,
  ACBrUtil.DateTime;

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
  LJsonObject, LJsonBoletoObject : TACBrJSONObject;
  LJsonArray, LJsonBoletoIndividualArray : TACBrJSONArray;
  LListaRejeicao: TACBrBoletoRejeicao;
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
      try
        LJsonObject := TACBrJsonObject.Parse( RetWS );

        ARetornoWS.JSON := LJsonObject.ToJSON;

        if TipoOperacao = tpInclui then
        begin
          //retorna quando houver erro
          if (LJsonObject.AsString['codigo'] <> '') or (LJsonObject.AsString['mensagem'] <> '') or
             (LJsonObject.AsString['codigo_erro'] <> '') or (LJsonObject.AsString['mensagem_erro'] <> '') then
          begin
            ARetornoWS.CodRetorno := LJsonObject.AsString['codigo'];
            if StrToIntDef(LJsonObject.AsString['codigo'], 0) > 0 then
            begin
              ARetornoWS.MsgRetorno := LJsonObject.AsString['mensagem'];
              LJsonArray := LJsonObject.AsJSONArray['campos'];
              for I := 0 to Pred(LJsonArray.Count) do
              begin
                LListaRejeicao            := ARetornoWS.CriarRejeicaoLista;
                LListaRejeicao.Codigo     := LJsonArray.ItemAsJSONObject[I].AsString['codigo'];
                LListaRejeicao.Versao     := LJsonArray.ItemAsJSONObject[I].AsString['versao'];
                LListaRejeicao.Mensagem   := LJsonArray.ItemAsJSONObject[I].AsString['mensagem'];
                LListaRejeicao.Ocorrencia := LJsonArray.ItemAsJSONObject[I].AsString['ocorrencia'];
                LListaRejeicao.Valor      := LJsonArray.ItemAsJSONObject[I].AsString['valor'];
              end;
            end
            else if StrToIntDef(LJsonObject.AsString['codigo_erro'], 0) > 0 then
            begin
              LListaRejeicao          := ARetornoWS.CriarRejeicaoLista;
              LListaRejeicao.Codigo   := LJsonObject.AsString['codigo_erro'];
              LListaRejeicao.Mensagem := LJsonObject.AsString['mensagem_erro'];
            end;
          end
          else
          begin

            if LJsonObject.ValueExists('data') then
              LJsonBoletoObject  := LJsonObject.AsJSONObject['data'].AsJSONObject['dado_boleto']
            else
              LJsonBoletoObject := LJsonObject.AsJSONObject['dado_boleto'];


            ARetornoWS.DadosRet.TituloRet.Carteira       := LJsonBoletoObject.AsString['codigo_carteira'];
            ARetornoWS.DadosRet.TituloRet.ValorDocumento := StrToFloatDef( LJsonBoletoObject.AsString['valor_total_titulo'], 0)/100;
            ARetornoWS.DadosRet.TituloRet.EspecieDoc     := LJsonBoletoObject.AsString['codigo_especie'];
            ARetornoWS.DadosRet.TituloRet.DataDocumento  := StringToDateTimeDef(LJsonBoletoObject.AsString['data_emissao'], 0, 'yyyy-mm-dd');

            if LJsonBoletoObject.IsJSONObject('pagador') then
            begin
              ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado     := LJsonBoletoObject.AsJSONObject['pagador'].AsJSONObject['pessoa'].AsString['nome_pessoa'];
              if (ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado = '') then
                ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado   := LJsonBoletoObject.AsJSONObject['pagador'].AsJSONObject['pessoa'].AsString['nome_razao_social_pagador'];

              if LJsonBoletoObject.AsJSONObject['pagador'].AsJSONObject['pessoa'].AsJSONObject['tipo_pessoa'].AsString['codigo_tipo_pessoa'] = 'F' then
                ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF        := LJsonBoletoObject.AsJSONObject['pagador'].AsJSONObject['pessoa'].AsJSONObject['tipo_pessoa'].AsString['numero_cadastro_pessoa_fisica']
              else
                ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF        := LJsonBoletoObject.AsJSONObject['pagador'].AsJSONObject['pessoa'].AsJSONObject['tipo_pessoa'].AsString['numero_cadastro_nacional_pessoa_juridica'];

              ARetornoWS.DadosRet.TituloRet.Sacado.Logradouro := LJsonBoletoObject.AsJSONObject['pagador'].AsJSONObject['endereco'].AsString['nome_logradouro'];
              ARetornoWS.DadosRet.TituloRet.Sacado.Bairro     := LJsonBoletoObject.AsJSONObject['pagador'].AsJSONObject['endereco'].AsString['nome_bairro'];
              ARetornoWS.DadosRet.TituloRet.Sacado.Cidade     := LJsonBoletoObject.AsJSONObject['pagador'].AsJSONObject['endereco'].AsString['nome_cidade'];
              ARetornoWS.DadosRet.TituloRet.Sacado.UF         := LJsonBoletoObject.AsJSONObject['pagador'].AsJSONObject['endereco'].AsString['sigla_UF'];
              ARetornoWS.DadosRet.TituloRet.Sacado.Cep        := LJsonBoletoObject.AsJSONObject['pagador'].AsJSONObject['endereco'].AsString['numero_CEP'];
            end;

            if LJsonBoletoObject.IsJSONObject('sacador_avalista') then
            begin
              ARetornoWS.DadosRet.TituloRet.SacadoAvalista.CNPJCPF         := LJsonBoletoObject.AsJSONObject['sacador_avalista'].AsString['numero_cadastro_pessoa_fisica'];
              ARetornoWS.DadosRet.TituloRet.SacadoAvalista.NomeAvalista    := LJsonBoletoObject.AsJSONObject['sacador_avalista'].AsString['nome_pessoa'];
              if ARetornoWS.DadosRet.TituloRet.SacadoAvalista.NomeAvalista = '' then
                ARetornoWS.DadosRet.TituloRet.SacadoAvalista.NomeAvalista  := LJsonBoletoObject.AsJSONObject['sacador_avalista'].AsString['nome_fantasia'];

            end;

            LJsonBoletoIndividualArray := LJsonBoletoObject.AsJSONArray['dados_individuais_boleto'];

            for I := 0 to Pred(LJsonBoletoIndividualArray.Count) do
            begin
              ARetornoWS.DadosRet.IDBoleto.IDBoleto  := LJsonBoletoIndividualArray.ItemAsJSONObject[I].AsString['id_boleto_individual'];
              ARetornoWS.DadosRet.IDBoleto.CodBarras := LJsonBoletoIndividualArray.ItemAsJSONObject[I].AsString['codigo_barras'];
              ARetornoWS.DadosRet.IDBoleto.LinhaDig  := LJsonBoletoIndividualArray.ItemAsJSONObject[I].AsString['numero_linha_digitavel'];
              ARetornoWS.DadosRet.IDBoleto.NossoNum  := LJsonBoletoIndividualArray.ItemAsJSONObject[I].AsString['numero_nosso_numero'];

              ARetornoWS.DadosRet.TituloRet.Vencimento           := StringToDateTimeDef(LJsonBoletoIndividualArray.ItemAsJSONObject[I].AsString['data_vencimento'], 0, 'yyyy-mm-dd');
              ARetornoWS.DadosRet.TituloRet.NossoNumero          := LJsonBoletoIndividualArray.ItemAsJSONObject[I].AsString['numero_nosso_numero'];
              ARetornoWS.DadosRet.TituloRet.SeuNumero            := LJsonBoletoIndividualArray.ItemAsJSONObject[I].AsString['texto_seu_numero'];
              ARetornoWS.DadosRet.TituloRet.CodBarras            := LJsonBoletoIndividualArray.ItemAsJSONObject[I].AsString['codigo_barras'];
              ARetornoWS.DadosRet.TituloRet.LinhaDig             := LJsonBoletoIndividualArray.ItemAsJSONObject[I].AsString['numero_linha_digitavel'];
              ARetornoWS.DadosRet.TituloRet.DataProcessamento    := StringToDateTimeDef(LJsonBoletoIndividualArray.ItemAsJSONObject[I].AsString['data_processamento'], 0, 'yyyy-mm-dd');
              ARetornoWS.DadosRet.TituloRet.UsoBanco             := LJsonBoletoIndividualArray.ItemAsJSONObject[I].AsString['uso_banco'];
              ARetornoWS.DadosRet.TituloRet.ValorDesconto        := StrToFloatDef( LJsonBoletoIndividualArray.ItemAsJSONObject[I].AsString['valor_desconto'], 0);
              ARetornoWS.DadosRet.TituloRet.ValorDespesaCobranca := StrToFloatDef(LJsonBoletoIndividualArray.ItemAsJSONObject[I].AsString['valor_outra_deducao'], 0);
              ARetornoWS.DadosRet.TituloRet.ValorMoraJuros       := StrToFloatDef( LJsonBoletoIndividualArray.ItemAsJSONObject[I].AsString['valor_juro_multa'], 0);
              ARetornoWS.DadosRet.TituloRet.ValorOutrosCreditos  := StrToFloatDef( LJsonBoletoIndividualArray.ItemAsJSONObject[I].AsString['valor_outro_acrescimo'], 0);
              ARetornoWS.DadosRet.TituloRet.ValorPago            := StrToFloatDef( LJsonBoletoIndividualArray.ItemAsJSONObject[I].AsString['valor_total_cobrado'], 0);
              ARetornoWS.DadosRet.TituloRet.ValorDocumento       := StrToFloatDef( LJsonBoletoIndividualArray.ItemAsJSONObject[I].AsString['valor_titulo'], 0);

              ARetornoWS.DadosRet.TituloRet.Informativo.Add( LJsonBoletoIndividualArray.ItemAsJSONObject[I].AsString['texto_informacao_cliente_beneficiario'] );
              ARetornoWS.DadosRet.TituloRet.Mensagem.Add(LJsonBoletoIndividualArray.ItemAsJSONObject[I].AsString['local_pagamento']);
            end;

            if LJsonObject.IsJSONObject('data') and LJsonObject.AsJSONObject['data'].IsJSONObject('dados_qrcode') then
            begin
              ARetornoWS.DadosRet.TituloRet.EMV    := LJsonObject.AsJSONObject['data'].AsJSONObject['dados_qrcode'].AsString['emv'];
              ARetornoWS.DadosRet.TituloRet.TxId   := LJsonObject.AsJSONObject['data'].AsJSONObject['dados_qrcode'].AsString['txid'];
              ARetornoWS.DadosRet.TituloRet.UrlPix := LJsonObject.AsJSONObject['data'].AsJSONObject['dados_qrcode'].AsString['location'];
              ARetornoWS.DadosRet.TituloRet.Url    := LJsonObject.AsJSONObject['data'].AsJSONObject['dados_qrcode'].AsString['location'];
            end;

          end;
        end else
        if TipoOperacao = tpConsultaDetalhe then
        begin
          if HTTPResultCode > 299 then
          begin
            if ( LJsonObject.IsJSONObject('codigo') ) then
              if (LJsonObject.AsString['codigo'] <> '') or (LJsonObject.AsString['mensagem'] <> '') then
              begin
                CodRetorno := strtointdef(LJsonObject.AsString['codigo'], 0);
                if CodRetorno < 1 then
                   CodRetorno := StrToIntDef(LJsonObject.AsString['codigo'], 0);

                 LListaRejeicao            := ARetornoWS.CriarRejeicaoLista;
                 LListaRejeicao.Codigo     := inttostr(CodRetorno);
                 LListaRejeicao.Mensagem   := LJsonObject.AsString['mensagem'];
              end;
          end;
          //retorna quando tiver sucesso
          if (ARetornoWS.ListaRejeicao.Count = 0) then
          begin

            if LJsonObject.ValueExists('data') then
              LJsonArray  := LJsonObject.AsJSONArray['data']
            else
              LJsonArray := LJsonObject.AsJSONArray['dado_boleto'];

            if LJsonArray.Count = 0 then
            begin
              LListaRejeicao            := ARetornoWS.CriarRejeicaoLista;
              LListaRejeicao.Codigo     := '00';
              LListaRejeicao.Mensagem   := 'Lista de retorno vazia!';
            end;

            for I := 0 to Pred(LJsonArray.Count) do
            begin

              LJsonBoletoObject  := LJsonArray.ItemAsJSONObject[I];

              ARetornoWS.DadosRet.IDBoleto.IDBoleto        := LJsonBoletoObject.AsString['id_boleto'];


              ARetornoWS.DadosRet.TituloRet.DataRegistro         := DateToDateTimeItau(LJsonBoletoObject.AsJSONObject['dado_boleto'].AsString['data_emissao']);
              ARetornoWS.DadosRet.TituloRet.DataDocumento        := DateToDateTimeItau(LJsonBoletoObject.AsJSONObject['dado_boleto'].AsString['data_emissao']);
              ARetornoWS.DadosRet.TituloRet.ValorAbatimento      := LJsonBoletoObject.AsJSONObject['dado_boleto'].AsFloat['valor_abatimento'];

              if LJsonBoletoObject.AsJSONObject['dado_boleto'].IsJSONObject('qrcode_pix') then
                ARetornoWS.DadosRet.TituloRet.EMV := LJsonBoletoObject.AsJSONObject['dado_boleto'].AsJSONObject['qrcode_pix'].AsString['emv'];


              LJsonBoletoIndividualArray := LJsonBoletoObject.AsJSONObject['dado_boleto'].AsJSONArray['dados_individuais_boleto'];

              for j := 0 to Pred(LJsonBoletoIndividualArray.Count) do
              begin
                ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca       := LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsString['situacao_geral_boleto'];
                ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := RetornaCodigoOcorrencia(UpperCase(LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsString['situacao_geral_boleto']));
                ARetornoWS.DadosRet.TituloRet.NossoNumero                := LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsString['numero_nosso_numero'];
                ARetornoWS.DadosRet.TituloRet.Vencimento                 := StringToDateTimeDef(LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsString['data_vencimento'], 0, 'yyyy-mm-dd');
                ARetornoWS.DadosRet.TituloRet.SeuNumero                  := LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsString['texto_seu_numero'];
                ARetornoWS.DadosRet.TituloRet.CodBarras                  := LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsString['codigo_barras'];
                ARetornoWS.DadosRet.TituloRet.LinhaDig                   := LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsString['numero_linha_digitavel'];
                ARetornoWS.DadosRet.TituloRet.ValorDocumento             := LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsFloat['valor_titulo'];
                ARetornoWS.DadosRet.IDBoleto.CodBarras                   := ARetornoWS.DadosRet.TituloRet.CodBarras;
                ARetornoWS.DadosRet.IDBoleto.LinhaDig                    := ARetornoWS.DadosRet.TituloRet.LinhaDig;
                ARetornoWS.DadosRet.IDBoleto.NossoNum                    := ARetornoWS.DadosRet.TituloRet.NossoNumero;
              end;

              if LJsonBoletoObject.AsJSONObject['dado_boleto'].IsJSONArray('pagamentos_cobranca') then
              begin
                for j := 0 to Pred(LJsonBoletoObject.AsJSONObject['dado_boleto'].AsJSONArray['pagamentos_cobranca'].Count) do
                begin
                  ARetornoWS.DadosRet.TituloRet.DataProcessamento    := StringToDateTimeDef(LJsonBoletoObject.AsJSONObject['dado_boleto'].AsJSONArray['pagamentos_cobranca'].ItemAsJSONObject[J].AsString['data_inclusao_pagamento'], 0, 'yyyy-mm-dd');
                  ARetornoWS.DadosRet.TituloRet.ValorPago            := StrToFloatDef( StringReplace(LJsonBoletoObject.AsJSONObject['dado_boleto'].AsJSONArray['pagamentos_cobranca'].ItemAsJSONObject[J].AsString['valor_pago_total_cobranca'],'.',',',[rfReplaceAll]), 0);
                  if ARetornoWS.DadosRet.TituloRet.ValorPago > ARetornoWS.DadosRet.TituloRet.ValorDocumento then
                     ARetornoWS.DadosRet.TituloRet.ValorOutrasDespesas := (ARetornoWS.DadosRet.TituloRet.ValorPago - ARetornoWS.DadosRet.TituloRet.ValorDocumento)
                end;
              end;

              if LJsonBoletoObject.AsJSONObject['dado_boleto'].IsJSONObject('baixa') then
              begin
                ARetornoWS.DadosRet.TituloRet.DataBaixa              := StringToDateTimeDef(LJsonBoletoObject.AsJSONObject['dado_boleto'].AsJSONObject['baixa'].AsString['data_inclusao_alteracao_baixa'], 0, 'yyyy-mm-dd');
                ARetornoWS.DadosRet.TituloRet.Mensagem.Text          := LJsonBoletoObject.AsJSONObject['dado_boleto'].AsJSONObject['baixa'].AsString['motivo_baixa'];
                if (UpperCase(LJsonBoletoObject.AsJSONObject['dado_boleto'].AsJSONObject['baixa'].AsString['motivo_baixa']) = 'BAIXA POR TER SIDO LIQUIDADO') then
                begin
                  ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca       := UpperCase(LJsonBoletoObject.AsJSONObject['dado_boleto'].AsJSONObject['baixa'].AsString['motivo_baixa']);
                  ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := RetornaCodigoOcorrencia(UpperCase(LJsonBoletoObject.AsJSONObject['dado_boleto'].AsJSONObject['baixa'].AsString['motivo_baixa']));
                end;
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
  else if (pSituacaoGeralBoleto  = 'PAGO') or (pSituacaoGeralBoleto  = 'PAGA') or
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
  LJsonObject, LJsonBoletoObject: TACBrJSONObject;
  LJsonArray, LJsonBoletosArray, LJsonBoletoIndividualArray : TACBrJSONArray;
  LListaRejeicao : TACBrBoletoRejeicao;
  I, j: Integer;
begin
  Result := True;
  ListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;
  ListaRetorno.HTTPResultCode := HTTPResultCode;
  ListaRetorno.JSONEnvio      := EnvWs;
  if RetWS <> '' then
  begin
    try
      try
        LJsonObject := TACBrJSONObject.Parse(RetWS);
        if HTTPResultCode > 299 then
        begin
          if ( LJsonObject.IsJSONObject('codigo') ) then
            if (LJsonObject.AsString['codigo'] <> '') or (LJsonObject.AsString['mensagem'] <> '') then
            begin
              CodRetorno := strtointdef(LJsonObject.AsString['codigo'], 0);
              if CodRetorno < 1 then
                 CodRetorno := strtointdef(LJsonObject.AsString['codigo'], 0);

               LListaRejeicao            := ListaRetorno.CriarRejeicaoLista;
               LListaRejeicao.Codigo     := inttostr(CodRetorno);
               LListaRejeicao.Mensagem   := LJsonObject.AsString['mensagem'];
            end;

        end;

        //retorna quando tiver sucesso
        if (ListaRetorno.ListaRejeicao.Count = 0) then
        begin
          if LJsonObject.IsJSONObject('page') then
          begin
            ListaRetorno.indicadorContinuidade := LJsonObject.AsJSONObject['page'].AsInteger['total_pages'] - LJsonObject.AsJSONObject['page'].AsInteger['page'] > 0;
            if ListaRetorno.indicadorContinuidade then
              ListaRetorno.proximoIndice         := LJsonObject.AsJSONObject['page'].AsInteger['page'] + 1;
          end;
          LJsonArray := LJsonObject.AsJSONArray['data'];
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
            ListaRetorno.DadosRet.IDBoleto.IDBoleto        := LJsonBoletoObject.AsString['id_boleto'];

            ListaRetorno.DadosRet.TituloRet.DataRegistro   := DateToDateTimeItau(LJsonBoletoObject.AsJSONObject['dado_boleto'].AsString['data_emissao']);
            LJsonBoletosArray := LJsonBoletoObject.AsJSONObject['dado_boleto'].AsJSONArray['dados_individuais_boleto'];

            for j := 0 to Pred(LJsonBoletosArray.Count) do
            begin
              ListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca := LJsonBoletosArray.ItemAsJSONObject[J].AsString['situacao_geral_boleto'];
              ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := RetornaCodigoOcorrencia(UpperCase(LJsonBoletosArray.ItemAsJSONObject[J].AsString['situacao_geral_boleto']));
              ListaRetorno.DadosRet.TituloRet.NossoNumero          := LJsonBoletosArray.ItemAsJSONObject[J].AsString['numero_nosso_numero'];
              ListaRetorno.DadosRet.IDBoleto.NossoNum              := ListaRetorno.DadosRet.TituloRet.NossoNumero;
              ListaRetorno.DadosRet.TituloRet.Vencimento           := StringToDateTimeDef(LJsonBoletosArray.ItemAsJSONObject[J].AsString['data_vencimento'], 0, 'yyyy-mm-dd');
              ListaRetorno.DadosRet.TituloRet.SeuNumero            := LJsonBoletosArray.ItemAsJSONObject[J].AsString['texto_seu_numero'];
              ListaRetorno.DadosRet.TituloRet.CodBarras            := LJsonBoletosArray.ItemAsJSONObject[J].AsString['codigo_barras'];
              ListaRetorno.DadosRet.IDBoleto.CodBarras             := ListaRetorno.DadosRet.TituloRet.CodBarras;
              ListaRetorno.DadosRet.TituloRet.LinhaDig             := LJsonBoletosArray.ItemAsJSONObject[J].AsString['numero_linha_digitavel'];
              ListaRetorno.DadosRet.IDBoleto.LinhaDig              := ListaRetorno.DadosRet.TituloRet.LinhaDig;
              ListaRetorno.DadosRet.TituloRet.ValorDocumento       := LJsonBoletoObject.AsJSONObject['dado_boleto'].AsFloat['valor_titulo'];
            end;

            if LJsonBoletoObject.AsJSONObject['dado_boleto'].IsJSONArray('pagamentos_cobranca') then
            begin
              for J := 0 to Pred(LJsonBoletoObject.AsJSONObject['dado_boleto'].AsJSONArray['pagamentos_cobranca'].Count) do
              begin
                ListaRetorno.DadosRet.TituloRet.DataProcessamento    := StringToDateTimeDef(LJsonBoletoObject.AsJSONObject['dado_boleto'].AsJSONArray['pagamentos_cobranca'].ItemAsJSONObject[J].AsString['data_inclusao_pagamento'], 0, 'yyyy-mm-dd');
                ListaRetorno.DadosRet.TituloRet.ValorPago            := StrToFloatDef( LJsonBoletoObject.AsJSONObject['dado_boleto'].AsJSONArray['pagamentos_cobranca'].ItemAsJSONObject[J].AsString['valor_pago_total_cobranca'], 0) / 100;
              end;
            end;

            if LJsonBoletoObject.AsJSONObject['dado_boleto'].IsJSONArray('dados_individuais_boleto') then
			      begin
              LJsonBoletoIndividualArray := LJsonBoletoObject.AsJSONObject['dado_boleto'].AsJSONArray['dados_individuais_boleto'];

              for J := 0 to Pred(LJsonBoletoIndividualArray.Count) do
              begin
                ListaRetorno.DadosRet.IDBoleto.IDBoleto  := LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsString['id_boleto_individual'];
                ListaRetorno.DadosRet.IDBoleto.CodBarras := LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsString['codigo_barras'];
                ListaRetorno.DadosRet.IDBoleto.LinhaDig  := LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsString['numero_linha_digitavel'];
                ListaRetorno.DadosRet.IDBoleto.NossoNum  := LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsString['numero_nosso_numero'];

                ListaRetorno.DadosRet.TituloRet.Vencimento           := StringToDateTimeDef(LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsString['data_vencimento'], 0, 'yyyy-mm-dd');
                ListaRetorno.DadosRet.TituloRet.NossoNumero          := LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsString['numero_nosso_numero'];
                ListaRetorno.DadosRet.TituloRet.SeuNumero            := LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsString['texto_seu_numero'];
                ListaRetorno.DadosRet.TituloRet.CodBarras            := LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsString['codigo_barras'];
                ListaRetorno.DadosRet.TituloRet.LinhaDig             := LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsString['numero_linha_digitavel'];
                ListaRetorno.DadosRet.TituloRet.DataProcessamento    := StringToDateTimeDef(LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsString['data_processamento'], 0, 'yyyy-mm-dd');
                ListaRetorno.DadosRet.TituloRet.UsoBanco             := LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsString['uso_banco'];
                ListaRetorno.DadosRet.TituloRet.ValorDesconto        := LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsCurrency['valor_desconto'];
                ListaRetorno.DadosRet.TituloRet.ValorDespesaCobranca := LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsCurrency['valor_outra_deducao'];
                ListaRetorno.DadosRet.TituloRet.ValorMoraJuros       := LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsCurrency['valor_juro_multa'];
                ListaRetorno.DadosRet.TituloRet.ValorOutrosCreditos  := LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsCurrency['valor_outro_acrescimo'];
                ListaRetorno.DadosRet.TituloRet.ValorPago            := LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsCurrency['valor_total_cobrado'];
                ListaRetorno.DadosRet.TituloRet.ValorDocumento       := LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsFloat['valor_titulo'];

                ListaRetorno.DadosRet.TituloRet.Informativo.Add( LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsString['texto_informacao_cliente_beneficiario'] );
                ListaRetorno.DadosRet.TituloRet.Mensagem.Add(LJsonBoletoIndividualArray.ItemAsJSONObject[J].AsString['local_pagamento']);
              end;
            end;

            if LJsonBoletoObject.AsJSONObject['dado_boleto'].IsJSONObject('qrcode_pix') then
            begin
              ListaRetorno.DadosRet.TituloRet.EMV := LJsonBoletoObject.AsJSONObject['dado_boleto'].AsJSONObject['qrcode_pix'].AsString['emv'];
            end;

            if LJsonBoletoObject.AsJSONObject['dado_boleto'].IsJSONObject('baixa') then
            begin
              ListaRetorno.DadosRet.TituloRet.DataBaixa              := StringToDateTimeDef(LJsonBoletoObject.AsJSONObject['baixa'].AsString['data_inclusao_alteracao_baixa'], 0, 'yyyy-mm-dd');
              ListaRetorno.DadosRet.TituloRet.Mensagem.Text          := LJsonBoletoObject.AsJSONObject['baixa'].AsString['motivo_baixa'];
              if (UpperCase(LJsonBoletoObject.AsJSONObject['baixa'].AsString['motivo_baixa']) = 'BAIXA POR TER SIDO LIQUIDADO') then
              begin
                ListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca       := UpperCase(LJsonBoletoObject.AsJSONObject['baixa'].AsString['motivo_baixa']);
                ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := RetornaCodigoOcorrencia(UpperCase(LJsonBoletoObject.AsJSONObject['baixa'].AsString['motivo_baixa']));
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

  end else
  begin
    case HTTPResultCode of
      404 :
        begin
          LListaRejeicao            := ListaRetorno.CriarRejeicaoLista;
          LListaRejeicao.Codigo     := '404';
          LListaRejeicao.Mensagem   := 'O servidor não conseguiu encontrar o recurso solicitado.';
        end;
      503 :
        begin
          LListaRejeicao            := ListaRetorno.CriarRejeicaoLista;
          LListaRejeicao.Codigo     := '503';
          LListaRejeicao.Versao     := 'ERRO INTERNO ITAU';
          LListaRejeicao.Mensagem   := 'SERVIÇO INDISPONÍVEL. O servidor está impossibilitado de lidar com a requisição no momento. Tente mais tarde.';
          LListaRejeicao.Ocorrencia := 'ERRO INTERNO nos servidores do Banco do Itaú.';
        end;
    end;

  end;
end;

end.
