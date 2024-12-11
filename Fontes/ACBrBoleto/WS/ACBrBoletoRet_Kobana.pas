{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{ Colaboradores nesse arquivo: Samuel "Muka" David                             }
{                Victor H. Gonzales - Pandaaa                                  }
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
//INCLUIDO EM 11/12/2024

{$I ACBr.inc}

unit ACBrBoletoRet_Kobana;

interface

uses
  ACBrBoletoWS.Rest,
  ACBrBoletoRetorno,
  ACBrBoleto,
  ACBrBoleto.Kobana.Classes;

type
  TRetornoEnvio_Kobana = class(TRetornoEnvioRest)
  private
    function DateJsonToDateTime(const AValue: String): TDateTime;
  protected
    function LerListaRetorno: Boolean; override;
    function LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean; override;
    function RetornoEnvio(const AIndex: Integer): Boolean; Override;
  public
    constructor Create(ABoletoWS: TACBrBoleto); Override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Classes,
  ACBrJSON,
  ACBrBoletoConversao,
  ACBrUtil.Base;

resourcestring
  C_CANCELADO = 'CANCELED';
  C_BAIXADO = 'VERIFICAR';
  C_EXPIRADO = 'VERIFICAR';
  C_VENCIDO = 'OVERDUE';
  C_EMABERTO = 'OPENED';
  C_PAGO = 'PAID';

  { TRetornoEnvio_Kobana }

constructor TRetornoEnvio_Kobana.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);
end;

function TRetornoEnvio_Kobana.DateJsonToDateTime(const AValue: String): TDateTime;
var
  lAno, lMes, lDia, lData:string;
begin
  lAno := Copy(AValue, 0, 4);
  lMes := Copy(AValue, 6, 2);
  lDia := Copy(AValue, 9, 2);
  lData := Format('%s/%s/%s', [lDia, lMes, lAno]);
  Result := StrToDateDef(lData, 0);
end;

destructor TRetornoEnvio_Kobana.Destroy;
begin
  inherited Destroy;
end;

function TRetornoEnvio_Kobana.LerListaRetorno: Boolean;
var
  lListaRetorno: TACBrBoletoRetornoWS;
  lJSONRetKobanaItem: TBankBilletRetJSON;
  li: Integer;
  lJSONRetKobana: TBankBilletRetJSONList;
begin
  Result := True;
  {ACBrBoleto.CriarTituloNaLista;

  lListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;
  lListaRetorno.HTTPResultCode := HTTPResultCode;
  lListaRetorno.JSONEnvio := EnvWs;}

  if Trim(RetWS) <> '' then
  begin
    try
      lJSONRetKobana := TBankBilletRetJSONList.Create;
      lJSONRetKobana := lJSONRetKobana.FromJsonString(RetWS);
      try
        for li := 0 to Length(lJSONRetKobana.List) - 1 do
        begin
          lJSONRetKobanaItem := lJSONRetKobana.List[li];
          lListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;

          lListaRetorno.DadosRet.IDBoleto.IDBoleto := inttostr(lJSONRetKobanaItem.id);
          lListaRetorno.DadosRet.IDBoleto.CodBarras := lJSONRetKobanaItem.Barcode;
          lListaRetorno.DadosRet.IDBoleto.LinhaDig := lJSONRetKobanaItem.Line;
          lListaRetorno.DadosRet.IDBoleto.NossoNum := lJSONRetKobanaItem.Processed_our_number;
          lListaRetorno.indicadorContinuidade := false;
          lListaRetorno.DadosRet.TituloRet.CodBarras := lListaRetorno.DadosRet.IDBoleto.CodBarras;
          lListaRetorno.DadosRet.TituloRet.LinhaDig := lListaRetorno.DadosRet.IDBoleto.LinhaDig;
          lListaRetorno.DadosRet.TituloRet.NossoNumero := lListaRetorno.DadosRet.IDBoleto.NossoNum;
          lListaRetorno.DadosRet.TituloRet.Vencimento := DateJsonToDateTime(DateToStr(lJSONRetKobanaItem.Expire_at));
          lListaRetorno.DadosRet.TituloRet.ValorDocumento := lJSONRetKobanaItem.Amount;
          lListaRetorno.DadosRet.TituloRet.ValorAtual := lJSONRetKobanaItem.Amount;
          lListaRetorno.DadosRet.TituloRet.ValorPago := lJSONRetKobanaItem.Paid_amount;
          lListaRetorno.DadosRet.TituloRet.NumeroDocumento := lJSONRetKobanaItem.Document_number;
          lListaRetorno.DadosRet.TituloRet.SeuNumero := Trim(lJSONRetKobanaItem.Document_number);
          lListaRetorno.DadosRet.TituloRet.DataRegistro := DateJsonToDateTime(FormatDateTime('yyyy-mm-dd', lJSONRetKobanaItem.Issued_at));
          lListaRetorno.DadosRet.TituloRet.Vencimento := DateJsonToDateTime(FormatDateTime('yyyy-mm-dd', lJSONRetKobanaItem.Expire_at));
          lListaRetorno.DadosRet.TituloRet.DataMovimento := DateJsonToDateTime(FormatDateTime('yyyy-mm-dd', lJSONRetKobanaItem.Updated_at));
          lListaRetorno.DadosRet.TituloRet.Sacado.NomeSacado := lJSONRetKobanaItem.Customer_person_name;
          lListaRetorno.DadosRet.TituloRet.Sacado.Cidade := lJSONRetKobanaItem.Customer_city_name;
          lListaRetorno.DadosRet.TituloRet.Sacado.UF := lJSONRetKobanaItem.Customer_state;
          lListaRetorno.DadosRet.TituloRet.Sacado.Bairro := lJSONRetKobanaItem.Customer_neighborhood;
          lListaRetorno.DadosRet.TituloRet.Sacado.Cep := lJSONRetKobanaItem.Customer_zipcode;
          lListaRetorno.DadosRet.TituloRet.Sacado.Numero := lJSONRetKobanaItem.Customer_address_number;
          lListaRetorno.DadosRet.TituloRet.Sacado.Complemento := lJSONRetKobanaItem.Customer_address_complement;
          lListaRetorno.DadosRet.TituloRet.Sacado.Logradouro := lJSONRetKobanaItem.Customer_address;
          lListaRetorno.DadosRet.TituloRet.Sacado.CNPJCPF := lJSONRetKobanaItem.Customer_cnpj_cpf;
        end;
      finally
        lJSONRetKobana.Free;
      end;
    except
      Result := false;
    end;
  end;
end;

function TRetornoEnvio_Kobana.LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;
var
  LJSONObject, LErrorObject, LJson: TACBrJSONObject;
  LJsonArray: TACBrJSONArray;
  LTipoOperacao: TOperacao;
  LRejeicao: TACBrBoletoRejeicao;
begin
  Result := True;
  LTipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;

  ARetornoWS.HTTPResultCode := HTTPResultCode;
  ARetornoWS.JSONEnvio := EnvWs;
  ARetornoWS.Header.Operacao := LTipoOperacao;

  if (HTTPResultCode = 204) and (LTipoOperacao = tpConsultaDetalhe) then
  begin
    HTTPResultCode := 400;
    Result := False;

    LRejeicao := ARetornoWS.CriarRejeicaoLista;
    LRejeicao.Codigo := '400';
    LRejeicao.Ocorrencia := '204';
    LRejeicao.Mensagem := 'A requisição foi processada com êxito e não está retornando conteúdo.';
  end;

  if Trim(RetWS) <> '' then
  begin
    try
      if HTTPResultCode > 299 then
      begin
        Result := False;
        LRejeicao := ARetornoWS.CriarRejeicaoLista;

        lJSONObject := TACBrJSONObject.Parse(RetWS);

        try
          case HTTPResultCode of
            400 .. 404:
              begin
                LJSONArray := LJsonObject.AsJSONArray['errors'];

                LErrorObject := LJSONArray.ItemAsJSONObject[0];

                LRejeicao.Codigo := LErrorObject.AsString['code'];
                LRejeicao.Ocorrencia := LErrorObject.AsString['title'];
                LRejeicao.Mensagem := LErrorObject.AsString['detail'];
              end;
            500:
              begin
                if LErrorObject.ValueExists('error') then
                begin
                  LRejeicao.Codigo := IntToStr(HTTPResultCode);
                  LRejeicao.Mensagem := LErrorObject.AsString['error'];
                end;
              end;
          else
            begin
              LRejeicao.Codigo := IntToStr(HTTPResultCode);
              LRejeicao.Ocorrencia := 'Retorno mensagem de erro não tratado.';
              LRejeicao.Mensagem := 'JSON: ' + RetWS;
            end;
          end;
        finally
          lJSONObject.Free;
        end;
      end;

      if (ARetornoWS.ListaRejeicao.Count = 0) then
      begin
        lJson := TACBrJSONObject.Parse(RetWS);
        try
          ARetornoWS.JSON := lJson.ToJSON;
          if (LTipoOperacao = tpInclui) then
          begin
            ARetornoWS.DadosRet.IDBoleto.CodBarras := lJson.AsString['barcode'];
            ARetornoWS.DadosRet.IDBoleto.LinhaDig := lJson.AsString['line'];
            ARetornoWS.DadosRet.IDBoleto.NossoNum := lJson.AsString['processed_our_number'];
            ARetornoWS.DadosRet.TituloRet.CodBarras := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
            ARetornoWS.DadosRet.TituloRet.NossoNumero := ARetornoWS.DadosRet.IDBoleto.NossoNum;
            ARetornoWS.DadosRet.TituloRet.Vencimento := DateJsonToDateTime(lJson.AsString['expire_at']);
            ARetornoWS.DadosRet.TituloRet.NossoNumero := lJson.AsString['processed_our_number'];
            ARetornoWS.DadosRet.TituloRet.EspecieDoc := lJson.AsString['document_type_label'];
            ARetornoWS.DadosRet.TituloRet.DataDocumento := DateJsonToDateTime(lJson.AsString['document_date']);
            ARetornoWS.DadosRet.TituloRet.ValorDocumento := lJson.AsCurrency['amount'];
            ARetornoWS.DadosRet.TituloRet.ValorDesconto := lJson.AsCurrency['discount_value'];
            ARetornoWS.DadosRet.TituloRet.NumeroDocumento := lJson.AsString['document_number'];
            ARetornoWS.DadosRet.TituloRet.SeuNumero := Trim(lJson.AsString['document_number']);
          end else if (LTipoOperacao = tpConsultaDetalhe) then
          begin
            ARetornoWS.DadosRet.IDBoleto.IDBoleto := lJson.AsString['id'];
            ARetornoWS.DadosRet.IDBoleto.CodBarras := lJson.AsString['barcode'];
            ARetornoWS.DadosRet.IDBoleto.LinhaDig := lJson.AsString['line'];
            ARetornoWS.DadosRet.IDBoleto.NossoNum := lJson.AsString['processed_our_number'];
            ARetornoWS.indicadorContinuidade := false;
            ARetornoWS.DadosRet.TituloRet.NossoNumero := ARetornoWS.DadosRet.IDBoleto.NossoNum;
            ARetornoWS.DadosRet.TituloRet.Vencimento := DateJsonToDateTime(lJson.AsString['expire_at']);
            ARetornoWS.DadosRet.TituloRet.ValorDocumento := lJson.AsCurrency['amount'];
            ARetornoWS.DadosRet.TituloRet.EspecieDoc := lJson.AsString['document_type_label'];
            ARetornoWS.DadosRet.TituloRet.Contrato := lJson.AsString['bank_billet_account_id'];

            case lJson.AsInteger['fine_type'] of // TIPO MULTA
              0: // Inexistente (Padrão)
                begin
                  ARetornoWS.DadosRet.TituloRet.CodigoMulta := cmIsento;
                end;

              1: // Para percentual do valor do boleto
                begin
                  ARetornoWS.DadosRet.TituloRet.CodigoMulta := cmPercentual;
                  ARetornoWS.DadosRet.TituloRet.PercentualMulta := lJson.AsCurrency['fine_percentage']; // verificar se dever fazer o calculo;
                  ARetornoWS.DadosRet.TituloRet.MultaValorFixo := false;
                end;

              2: // Para valor fixo
                begin
                  ARetornoWS.DadosRet.TituloRet.CodigoMulta := cmValorFixo;
                  ARetornoWS.DadosRet.TituloRet.ValorMulta := lJson.AsCurrency['fine_value'];
                  ARetornoWS.DadosRet.TituloRet.MultaValorFixo := True;
                end;
            end;

            ARetornoWS.DadosRet.TituloRet.CodBarras := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
            ARetornoWS.DadosRet.TituloRet.NumeroDocumento := Trim(lJson.AsString['document_number']);
            ARetornoWS.DadosRet.TituloRet.SeuNumero := Trim(ARetornoWS.DadosRet.TituloRet.NumeroDocumento);
            ARetornoWS.DadosRet.TituloRet.DataRegistro := DateJsonToDateTime(lJson.AsString['created_at']);
            ARetornoWS.DadosRet.TituloRet.Vencimento := DateJsonToDateTime(lJson.AsString['expire_at']);
            ARetornoWS.DadosRet.TituloRet.DataCredito := DateJsonToDateTime(lJson.AsString['paid_at']);
            ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := lJson.AsString['status'];

            if UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca) = C_EMABERTO then
              ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '1';

            if (UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca) = C_BAIXADO) or
              (UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca) = C_CANCELADO) then
              ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '7';

            if (UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca) = C_PAGO) then
              ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '6';

            ARetornoWS.DadosRet.TituloRet.ValorPago := lJson.AsCurrency['paid_amount'];
            ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado := lJson.AsString['customer_person_name'];
            ARetornoWS.DadosRet.TituloRet.Sacado.Cidade := lJson.AsString['customer_city_name'];
            ARetornoWS.DadosRet.TituloRet.Sacado.UF := lJson.AsString['customer_state'];
            ARetornoWS.DadosRet.TituloRet.Sacado.Bairro := lJson.AsString['customer_neighborhood'];
            ARetornoWS.DadosRet.TituloRet.Sacado.Cep := lJson.AsString['customer_zipcode'];
            ARetornoWS.DadosRet.TituloRet.Sacado.Numero := lJson.AsString['customer_address_number'];
            ARetornoWS.DadosRet.TituloRet.Sacado.Complemento := lJson.AsString['customer_address_complement'];
            ARetornoWS.DadosRet.TituloRet.Sacado.Logradouro := lJson.AsString['customer_address'];
            ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF := lJson.AsString['customer_cnpj_cpf'];
          end
          else if (LTipoOperacao = tpBaixa) then
          begin
            // não possui dados de retorno..
          end
          else if (LTipoOperacao = tpAltera) then
          begin
            // não possui dados de retorno..
          end;

        finally
          lJson.Free;
        end;
      end;
    except
      Result := False;
    end;
  end;
end;

function TRetornoEnvio_Kobana.RetornoEnvio(const AIndex: Integer): Boolean;
begin
  Result := inherited RetornoEnvio(AIndex);
end;

end.

