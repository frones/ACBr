{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Luis Claudio Padilha Junior                    }
{                               Victor Hugo Gonzales - Pandaaa                 }
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

unit ACBrBoletoRet_Cora;

interface

uses
  Classes, SysUtils, ACBrBoleto, ACBrBoletoWS, ACBrBoletoRetorno,
  ACBrJSON,
  ACBrBoletoWS.Rest;

type

{ TRetornoEnvio_Cora }

  TRetornoEnvio_Cora = class(TRetornoEnvioREST)
  private
    function DateInterToDateTime(const AValue: string): TDateTime;
  public
    constructor Create(ABoletoWS: TACBrBoleto); override;
    destructor Destroy; override;
    function LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean; override;
    function LerListaRetorno: Boolean; override;
    function RetornoEnvio(const AIndex: Integer): Boolean; override;

  end;

implementation

uses
  ACBrBoletoConversao;

resourcestring
  C_CANCELADO = 'CANCELLED';
  C_VENCIDO   = 'LATE';
  C_EMABERTO  = 'OPEN';
  C_PAGO      = 'PAID';


{ TRetornoEnvio }

constructor TRetornoEnvio_Cora.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);

end;

function TRetornoEnvio_Cora.DateInterToDateTime(const AValue: string): TDateTime;
var
  data, ano, mes, dia: string;
begin

  ano := Copy(AValue, 0, 4);
  mes := Copy(AValue, 6, 2);
  dia := Copy(AValue, 9, 2);
  data := Format('%s/%s/%s', [dia, mes, ano]);
  Result := StrToDateDef(data, 0);
end;

destructor TRetornoEnvio_Cora.Destroy;
begin
  inherited Destroy;
end;

function TRetornoEnvio_Cora.LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;
var
  LJson, LPaymentOptions, LBankSlip, LJsonObject: TACBrJSONObject;
  LJsonArray : TACBrJSONArray;
  ARejeicao: TACBrBoletoRejeicao;
  TipoOperacao: TOperacao;
  I: Integer;
  LNossoNumeroInicial : Byte;
begin
  Result := True;
  TipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;
  ARetornoWS.HTTPResultCode  := HTTPResultCode;
  ARetornoWS.JSONEnvio       := EnvWs;
  ARetornoWS.Header.Operacao := TipoOperacao;

  if RetWS <> '' then
  begin
    try
      LJson := TACBrJSONObject.Parse(RetWS);
      try
        ARetornoWS.JSON := LJson.ToJSON;

        case HttpResultCode of
          400, 415:
            begin
              LJsonArray := LJson.AsJSONArray['errors'];
              for I := 0 to Pred(LJsonArray.Count) do
              begin
                ARejeicao := ARetornoWS.CriarRejeicaoLista;
                ARejeicao.Codigo := LJsonArray.ItemAsJSONObject[I].AsString['code'];
                ARejeicao.Mensagem := LJsonArray.ItemAsJSONObject[I].AsString['message'];
              end;
            end;

          404:
            begin
                if (LJson.ValueExists('errors')) and (LJson.AsString['errors'] <> '') then
                begin
                  ARejeicao := ARetornoWS.CriarRejeicaoLista;
                  ARejeicao.Codigo   := LJson.AsString['code'];
                  ARejeicao.Mensagem := LJson.AsString['message'];
                end;
            end;

            422:
            begin
              if (TipoOperacao = tpCancelar) then
              begin
                if (LJson.ValueExists('errors')) and (LJson.AsString['errors'] <> '') then
                begin
                  ARejeicao := ARetornoWS.CriarRejeicaoLista;
                  ARejeicao.Codigo := LJson.AsString['code'];
                  ARejeicao.Mensagem := LJson.AsString['message'];
                end;
              end;
            end;

        end;

        if (ARetornoWS.JSON = '{}') and (TipoOperacao = tpConsultaDetalhe) then
        begin
          ARejeicao := ARetornoWS.CriarRejeicaoLista;
          ARejeicao.Codigo   := '404';
          ARejeicao.Mensagem := 'Nenhum registro encontrado';
        end;


        //retorna quando tiver sucesso
        if (ARetornoWS.ListaRejeicao.Count = 0) then
        begin
          if (TipoOperacao = tpInclui) then
          begin
            LPaymentOptions                           := LJson.AsJSONObject['payment_options'];
            LBankSlip                                 := LPaymentOptions.AsJSONObject['bank_slip'];
            ARetornoWS.DadosRet.IDBoleto.CodBarras    := LBankSlip.AsString['barcode'];
            ARetornoWS.DadosRet.IDBoleto.LinhaDig     := LBankSlip.AsString['digitable'];

            LNossoNumeroInicial := Length(Trim(LBankSlip.AsString['our_number'])) - ACBrTitulo.ACBrBoleto.Banco.TamanhoMaximoNossoNum;
            Inc(LNossoNumeroInicial);

            ARetornoWS.DadosRet.IDBoleto.NossoNum     := Copy(LBankSlip.AsString['our_number'],LNossoNumeroInicial,ACBrTitulo.ACBrBoleto.Banco.TamanhoMaximoNossoNum);
            ARetornoWS.DadosRet.IDBoleto.URLPDF       := LBankSlip.AsString['url'];
            ARetornoWS.DadosRet.IDBoleto.IDBoleto     := LJson.AsString['id'];    //obrigatorio pra consultar
            ARetornoWS.DadosRet.TituloRet.NossoNumeroCorrespondente := ARetornoWS.DadosRet.IDBoleto.IDBoleto;
            ARetornoWS.DadosRet.TituloRet.CodBarras   := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig    := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
            ARetornoWS.DadosRet.TituloRet.NossoNumero := ARetornoWS.DadosRet.IDBoleto.NossoNum;

            ARetornoWS.DadosRet.TituloRet.SeuNumero   := LJson.AsString['code'];
            ARetornoWS.DadosRet.TituloRet.EMV         := LJson.AsJSONObject['pix'].AsString['emv'];

          end
          else if (TipoOperacao = tpConsultaDetalhe) then
          begin

            LPaymentOptions                           := LJson.AsJSONObject['payment_options'];
            LBankSlip                                 := LPaymentOptions.AsJSONObject['bank_slip'];

            LNossoNumeroInicial := Length(Trim(LBankSlip.AsString['our_number'])) - ACBrTitulo.ACBrBoleto.Banco.TamanhoMaximoNossoNum;
            Inc(LNossoNumeroInicial);
            ARetornoWS.DadosRet.IDBoleto.CodBarras       := LBankSlip.AsString['barcode'];
            ARetornoWS.DadosRet.IDBoleto.LinhaDig        := LBankSlip.AsString['digitable'];
            ARetornoWS.DadosRet.IDBoleto.URLPDF          := LBankSlip.AsString['url'];
            ARetornoWS.DadosRet.IDBoleto.NossoNum        := Copy(LBankSlip.AsString['our_number'],LNossoNumeroInicial,ACBrTitulo.ACBrBoleto.Banco.TamanhoMaximoNossoNum);
            ARetornoWS.indicadorContinuidade             := False;

            ARetornoWS.DadosRet.TituloRet.NossoNumero    := ARetornoWS.DadosRet.IDBoleto.NossoNum;
            ARetornoWS.DadosRet.TituloRet.Vencimento     := DateInterToDateTime(LJson.AsJSONObject['payment_terms'].AsString['due_date']);
            ARetornoWS.DadosRet.TituloRet.ValorDocumento := LJSON.AsFloat['total_amount'] / 100;
            ARetornoWS.DadosRet.TituloRet.ValorAtual     := LJSON.AsFloat['total_amount'] / 100;
            ARetornoWS.DadosRet.TituloRet.ValorPago      := LJSON.AsFloat['total_paid'] / 100;

            if LJSON.AsJSONObject['interest'].AsFloat['rate'] > 0  then
            begin
              ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros := cjValorDia;
              ARetornoWS.DadosRet.TituloRet.ValorMoraJuros := LJSON.AsJSONObject['interest'].AsFloat['rate'];
            end
            else
            begin
              ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros := cjIsento;
              ARetornoWS.DadosRet.TituloRet.ValorMoraJuros := 0;
            end;

            if LJSON.AsJSONObject['fine'].AsFloat['rate'] > 0 then
            begin
              ARetornoWS.DadosRet.TituloRet.PercentualMulta := LJSON.AsJSONObject['fine'].AsFloat['rate'];
              ARetornoWS.DadosRet.TituloRet.MultaValorFixo  := False;
              ARetornoWS.DadosRet.TituloRet.DataMulta       := DateInterToDateTime(LJSON.AsJSONObject['fine'].AsString['date']);
            end
            else if  LJSON.AsJSONObject['fine'].AsFloat['amount'] > 0 then
            begin
              ARetornoWS.DadosRet.TituloRet.PercentualMulta := LJSON.AsJSONObject['multa'].AsFloat['amount'];
              ARetornoWS.DadosRet.TituloRet.MultaValorFixo  := True;
              ARetornoWS.DadosRet.TituloRet.ValorMulta      := ARetornoWS.DadosRet.TituloRet.PercentualMulta;
              ARetornoWS.DadosRet.TituloRet.DataMulta       := DateInterToDateTime(LJSON.AsJSONObject['fine'].AsString['date']);
            end
            else
            begin
              ARetornoWS.DadosRet.TituloRet.PercentualMulta := 0;
              ARetornoWS.DadosRet.TituloRet.ValorMulta      := 0;
              ARetornoWS.DadosRet.TituloRet.MultaValorFixo  := False;
            end;

            if LJSON.AsJSONObject['discount'].AsString['type'] = 'FIXED'  then
            begin
              ARetornoWS.DadosRet.TituloRet.ValorDesconto  := LJSON.AsJSONObject['discount'].AsFloat['value'] / 100;
              ARetornoWS.DadosRet.TituloRet.CodigoDesconto := cdValorFixo;
              ARetornoWS.DadosRet.TituloRet.DataDesconto   := DateInterToDateTime(LJSON.AsJSONObject['discount'].AsString['data']);
            end
            else if LJSON.AsJSONObject['discount'].AsString['type'] = 'PERCENT'then
            begin
              ARetornoWS.DadosRet.TituloRet.ValorDesconto  := LJSON.AsJSONObject['discount'].AsFloat['value'];
              ARetornoWS.DadosRet.TituloRet.CodigoDesconto := cdPercentual;
              ARetornoWS.DadosRet.TituloRet.DataDesconto   := DateInterToDateTime(LJSON.AsJSONObject['discount'].AsString['data']);
            end
            else
            begin
              ARetornoWS.DadosRet.TituloRet.ValorDesconto  := 0;
              ARetornoWS.DadosRet.TituloRet.CodigoDesconto := cdSemDesconto;
            end;

            ARetornoWS.DadosRet.TituloRet.CodBarras            := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig             := ARetornoWS.DadosRet.IDBoleto.LinhaDig;

            ARetornoWS.DadosRet.TituloRet.SeuNumero            := LJSON.AsString['code'];
            ARetornoWS.DadosRet.TituloRet.DataRegistro         := DateIntertoDateTime(LJSON.AsString['created_at']);


            ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := LJSON.AsString['status'];
            ARetornoWS.DadosRet.TituloRet.DataMovimento        := DateIntertoDateTime(LJSON.AsString['occurrence_date']);
            ARetornoWS.DadosRet.TituloRet.DataCredito          := DateIntertoDateTime(LJSON.AsString['occurrence_date']);

            LJsonObject := LJSON.AsJSONObject['customer'];
            ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado := LJsonObject.AsString['name'];
            ARetornoWS.DadosRet.TituloRet.Sacado.Cidade     := LJsonObject.AsJSONObject['address'].AsString['city'];
            ARetornoWS.DadosRet.TituloRet.Sacado.UF         := LJsonObject.AsJSONObject['address'].AsString['state'];
            ARetornoWS.DadosRet.TituloRet.Sacado.Bairro     := LJsonObject.AsJSONObject['address'].AsString['district'];
            ARetornoWS.DadosRet.TituloRet.Sacado.Cep        := LJsonObject.AsJSONObject['address'].AsString['zip_code'];
            ARetornoWS.DadosRet.TituloRet.Sacado.Numero     := LJsonObject.AsJSONObject['address'].AsString['number'];
            ARetornoWS.DadosRet.TituloRet.Sacado.Logradouro := LJsonObject.AsJSONObject['address'].AsString['street'];
            ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF    := LJsonObject.AsJSONObject['document'].AsString['identity'];

            if (LJSON.AsString['status'] = C_CANCELADO) or (LJSON.AsString['status'] = C_PAGO) then
            begin
                 //   ARetornoWS.DadosRet.TituloRet.ValorPago                   := LJSON.Values['valorNominal'].AsNumber;
              ARetornoWS.DadosRet.TituloRet.DataBaixa := DateIntertoDateTime(LJSON.AsString['occurrence_date'])
            end;

          end
          else if (TipoOperacao = tpBaixa) then
          begin
            // não possui dados de retorno..
          end
          
        end;

      finally
        LJson.free;
      end;

    except
      Result := False;
    end;

  end;

end;

function TRetornoEnvio_Cora.LerListaRetorno: Boolean;
var
  ListaRetorno: TACBrBoletoRetornoWS;
  LJSON, LPaymentOptions, LBankSlip: TACBrJsonObject;
  LJSONArray : TACBrJSONArray;
  ARejeicao: TACBrBoletoRejeicao;
  I: Integer;
  Indice: Integer;
  LNossoNumeroInicial : Byte;
begin
  Result := True;
  ListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;
  ListaRetorno.HTTPResultCode := HTTPResultCode;
  ListaRetorno.JSONEnvio := EnvWs;

  if RetWS <> '' then
  begin
    try
      LJSON := TACBrJsonObject.Parse(RetWS);
      try
        ListaRetorno.JSON := LJSON.ToJSON;

        case HTTPResultCode of
          400, 404:
            begin
              if LJSON.ValueExists('message') and (LJSON.AsString['message'] <> '')then
              begin
                ARejeicao := ListaRetorno.CriarRejeicaoLista;
                ARejeicao.Codigo   := LJSON.AsString['codigo'];
                ARejeicao.Versao   := LJSON.AsString['parametro'];
                ARejeicao.Mensagem := LJSON.AsString['message'];
              end;
            end;
        end;

        //retorna quando tiver sucesso
        if (ListaRetorno.ListaRejeicao.Count = 0) then
        begin
          if not (LJSON.AsBoolean['last']) then
          begin
            ListaRetorno.indicadorContinuidade := True;
            Indice := Trunc(ACBrBoleto.Configuracoes.WebService.Filtro.indiceContinuidade);
            ListaRetorno.proximoIndice := (Indice + 1)
          end
          else
          begin
            ListaRetorno.indicadorContinuidade := False;
            ListaRetorno.proximoIndice := 0;
          end;

         // aJsonString := LJSON.Values[''].Stringify;
          LJSONArray := LJSON.AsJSONArray['items'];
          for I := 0 to Pred(LJSONArray.Count) do
          begin
            if I > 0 then
              ListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;


            LPaymentOptions                           := LJSONArray.ItemAsJSONObject[I].AsJSONObject['payment_options'];
            LBankSlip                                 := LPaymentOptions.AsJSONObject['bank_slip'];

            LNossoNumeroInicial := Length(Trim(LBankSlip.AsString['our_number'])) - ACBrTitulo.ACBrBoleto.Banco.TamanhoMaximoNossoNum;
            Inc(LNossoNumeroInicial);

            ListaRetorno.DadosRet.IDBoleto.CodBarras          := LBankSlip.AsString['barcode'];
            ListaRetorno.DadosRet.IDBoleto.LinhaDig           := LBankSlip.AsString['digitable'];
            ListaRetorno.DadosRet.IDBoleto.NossoNum           := Copy(LBankSlip.AsString['our_number'],LNossoNumeroInicial,ACBrTitulo.ACBrBoleto.Banco.TamanhoMaximoNossoNum);
            ListaRetorno.DadosRet.IDBoleto.URLPDF             := LBankSlip.AsString['url'];
            ListaRetorno.indicadorContinuidade                := False;
            ListaRetorno.DadosRet.TituloRet.CodBarras         := ListaRetorno.DadosRet.IDBoleto.CodBarras;
            ListaRetorno.DadosRet.TituloRet.LinhaDig          := ListaRetorno.DadosRet.IDBoleto.LinhaDig;

            ListaRetorno.DadosRet.TituloRet.NossoNumero       := ListaRetorno.DadosRet.IDBoleto.NossoNum;

            ListaRetorno.DadosRet.TituloRet.Vencimento        := DateInterToDateTime(LJSON.AsJSONObject['payment_terms'].AsString['due_date']);

            ListaRetorno.DadosRet.TituloRet.ValorDocumento    := LJSON.AsFloat['total_amount']/100;
            ListaRetorno.DadosRet.TituloRet.ValorAtual        := LJSON.AsFloat['total_amount']/100;
            ListaRetorno.DadosRet.TituloRet.ValorPago         := LJSON.AsFloat['total_paid']/100;

            if LJSON.AsJSONObject['interest'].AsFloat['rate'] > 0 then
            begin
             //nao definidio se é juros diario... mas nao tem mora mensal..
              ListaRetorno.DadosRet.TituloRet.CodigoMoraJuros := cjValorDia;
              ListaRetorno.DadosRet.TituloRet.ValorMoraJuros := LJSON.AsJSONObject['interest'].AsFloat['rate'];
            end

            else
            begin
              ListaRetorno.DadosRet.TituloRet.CodigoMoraJuros := cjIsento;
              ListaRetorno.DadosRet.TituloRet.ValorMoraJuros  := 0;
            end;

            if LJSON.AsJSONObject['fine'].AsFloat['rate'] > 0 then
            begin
              ListaRetorno.DadosRet.TituloRet.PercentualMulta := LJSON.AsJSONObject['fine'].AsFloat['rate'];
              ListaRetorno.DadosRet.TituloRet.MultaValorFixo  := False;
              ListaRetorno.DadosRet.TituloRet.DataMulta       := DateInterToDateTime(LJSON.AsJSONObject['fine'].AsString['date']);
            end
            else if LJSON.AsJSONObject['fine'].AsFloat['amount'] > 0 then
            begin
              ListaRetorno.DadosRet.TituloRet.PercentualMulta := LJSON.AsJSONObject['multa'].AsFloat['amount'];
              ListaRetorno.DadosRet.TituloRet.MultaValorFixo  := True;
              ListaRetorno.DadosRet.TituloRet.ValorMulta      := ListaRetorno.DadosRet.TituloRet.PercentualMulta;
              ListaRetorno.DadosRet.TituloRet.DataMulta       := DateInterToDateTime(LJSON.AsJSONObject['fine'].AsString['date']);
            end
            else
            begin
              ListaRetorno.DadosRet.TituloRet.PercentualMulta := 0;
              ListaRetorno.DadosRet.TituloRet.ValorMulta := 0;
              ListaRetorno.DadosRet.TituloRet.MultaValorFixo := False;
            end;

            if LJSON.AsJSONObject['discount'].AsString['type'] = 'FIXED' then
            begin
              ListaRetorno.DadosRet.TituloRet.ValorDesconto := LJSON.AsJSONObject['discount'].AsFloat['value'];
              ListaRetorno.DadosRet.TituloRet.CodigoDesconto := cdValorFixo;
              ListaRetorno.DadosRet.TituloRet.DataDesconto := DateInterToDateTime(LJSON.AsJSONObject['discount'].AsString['data']);
            end
            else if LJSON.AsJSONObject['discount'].AsString['type'] = 'PERCENT' then
            begin
              ListaRetorno.DadosRet.TituloRet.ValorDesconto := LJSON.AsJSONObject['discount'].AsFloat['value'];
              ListaRetorno.DadosRet.TituloRet.CodigoDesconto := cdPercentual;
              ListaRetorno.DadosRet.TituloRet.DataDesconto := DateInterToDateTime(LJSON.AsJSONObject['discount'].AsString['data']);
            end
            else
            begin
              ListaRetorno.DadosRet.TituloRet.ValorDesconto  := 0;
              ListaRetorno.DadosRet.TituloRet.CodigoDesconto := cdSemDesconto;
            end;

            ListaRetorno.DadosRet.TituloRet.SeuNumero            := LJSON.AsString['code'];
            ListaRetorno.DadosRet.TituloRet.DataRegistro         := DateIntertoDateTime(LJSON.AsString['created_at']);
            ListaRetorno.DadosRet.IDBoleto.IDBoleto              := LJSON.AsString['id'];
            ListaRetorno.DadosRet.TituloRet.NossoNumeroCorrespondente := ListaRetorno.DadosRet.IDBoleto.IDBoleto;
            ListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca := LJSON.AsString['status'];
            ListaRetorno.DadosRet.TituloRet.DataMovimento        := DateIntertoDateTime(LJSON.AsString['occurrence_date']);
            ListaRetorno.DadosRet.TituloRet.DataCredito          := DateIntertoDateTime(LJSON.AsString['occurrence_date']);

            if (LJSON.AsString['status'] = C_CANCELADO) or (LJSON.AsString['status'] = C_PAGO) then
            begin
              if LJSON.AsString['status'] <> C_PAGO then
                ListaRetorno.DadosRet.TituloRet.ValorPago := LJSON.AsFloat['total_amount'];
              ListaRetorno.DadosRet.TituloRet.DataBaixa := DateIntertoDateTime(LJSON.AsString['occurrence_date'])
            end;
          end;
        end;
      finally
        LJSON.free;
      end;
    except
      Result := False;
    end;
  end;
end;

function TRetornoEnvio_Cora.RetornoEnvio(const AIndex: Integer): Boolean;
begin
  Result := inherited RetornoEnvio(AIndex);
end;

end.

