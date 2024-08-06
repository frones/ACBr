{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Victor Hugo Gonzales - Panda                   }
{ Delmar de Lima, Daniel de Morais InfoCotidiano, ActioSistemas                }
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

unit ACBrBoletoRet_C6;

interface

uses
  Classes, SysUtils, ACBrBoleto,ACBrBoletoWS, ACBrBoletoRetorno,
  DateUtils, pcnConversao,ACBrBoletoWS.Rest, ACBrJSON, ACBrUtil.Base;

type

{ TRetornoEnvio_C6 }
 TLinhaDigitavelInfo = (TLDNossoNumero,TLDCodigoCarteira,TLDIdentificacaoLayout,TLDCodigoCedente,TLDFatorVencimento, TLDValorDocumento);
 TRetornoEnvio_C6 = class(TRetornoEnvioREST)
 private
   function DateToDateTime(const AValue : String) : TDateTime;

 public
   constructor Create(ABoletoWS: TACBrBoleto); override;
   destructor  Destroy; Override;
   function LinhaDigitavelExplodeInfo(const AValue : string; const ATipoInformacao : TLinhaDigitavelInfo) : string;
   function LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean; override;
   function LerListaRetorno: Boolean; override;
   function RetornoEnvio(const AIndex: Integer): Boolean; override;

 end;

implementation

uses
  ACBrBoletoConversao,
  ACBrUtil.Strings;

resourcestring
  C_CANCELADO = 'CANCELADO';
  C_EXPIRADO  = 'EXPIRADO';
  C_VENCIDO   = 'VENCIDO';
  C_EMABERTO  = 'EMABERTO';
  C_PAGO      = 'PAGO';
  // api V3 pix "RECEBIDO" "A_RECEBER" "MARCADO_RECEBIDO" "ATRASADO" "CANCELADO" "EXPIRADO"
  C_RECEBIDO          = 'RECEBIDO';
  C_ARECEBER          = 'A_RECEBER';
  C_MARCADO_RECEBIDO  = 'MARCADO_RECEBIDO';
  C_ATRASADO          = 'ATRASADO';





{ TRetornoEnvio }

constructor TRetornoEnvio_C6.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);
end;

function TRetornoEnvio_C6.DateToDateTime(const AValue: String): TDateTime;
var
  LData, LAno, LMes, LDia:String;
begin
  LAno := Copy( aValue, 0,4 );
  LMes := Copy( aValue, 6,2 );
  LDia := Copy( aValue, 9,2 );
  LData := Format( '%s/%s/%s' , [LDia,LMes,LAno]);
  Result := StrToDateDef( LData ,0 );
end;

destructor TRetornoEnvio_C6.Destroy;
begin
  inherited Destroy;
end;

function TRetornoEnvio_C6.LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;
var
  LJsonObject, AJSonObjectItem: TACBrJSONObject;
  LRejeicaoMensagem: TACBrBoletoRejeicao;
  LJsonArray: TACBrJSONArray;
  LTipoOperacao : TOperacao;
  X:Integer;
  LSituacao : AnsiString;
begin
  Result := True;
  LTipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;
  ARetornoWS.HTTPResultCode  := HTTPResultCode;
  ARetornoWS.JSONEnvio       := EnvWs;
  ARetornoWS.Header.Operacao := LTipoOperacao;

  if RetWS <> '' then
  begin
    RetWS := UTF8ToNativeString(RetWS);
    LJsonObject := TACBrJSONObject.Parse(RetWS);
    try
      try
        ARetornoWS.JSON := LJsonObject.ToJSON;

        if HttpResultCode >= 400 then
        begin
          if LJsonObject.ValueExists('status') then
          begin
            LRejeicaoMensagem            := ARetornoWS.CriarRejeicaoLista;
            LRejeicaoMensagem.Codigo     := LJsonObject.AsString['status'];
            LRejeicaoMensagem.Versao     := 'Correlation_id:' + LJsonObject.AsString['correlation_id'];
            LRejeicaoMensagem.Mensagem   := LJsonObject.AsString['title'];
            LRejeicaoMensagem.Campo      := LJsonObject.AsString['type'];
            LRejeicaoMensagem.Ocorrencia := LJsonObject.AsString['detail'];
          end;
        end;

        //retorna quando tiver sucesso
        if (ARetornoWS.ListaRejeicao.Count = 0) then
        begin
          if (LTipoOperacao = tpInclui) then
          begin

            ARetornoWS.DadosRet.IDBoleto.CodBarras      := OnlyNumber(LJsonObject.AsString['bar_code']);
            ARetornoWS.DadosRet.IDBoleto.LinhaDig       := OnlyNumber(LJsonObject.AsString['digitable_line']);
            ARetornoWS.DadosRet.IDBoleto.NossoNum       := LJsonObject.AsString['internal_id'];
            ARetornoWS.DadosRet.IDBoleto.IDBoleto       := LJsonObject.AsString['id'];

            ARetornoWS.DadosRet.TituloRet.NossoNumeroCorrespondente := ARetornoWS.DadosRet.IDBoleto.IDBoleto;
            ARetornoWS.DadosRet.TituloRet.CodBarras     := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig      := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
            ARetornoWS.DadosRet.TituloRet.NossoNumero   := ARetornoWS.DadosRet.IDBoleto.NossoNum;
            ARetornoWS.DadosRet.TituloRet.Carteira      := LJsonObject.AsString['billing_scheme'];

            if LJsonObject.AsString['billing_scheme'] = '3' then
              ARetornoWS.DadosRet.TituloRet.ResponsavelPelaEmissao := tbBancoEmite
            else
              ARetornoWS.DadosRet.TituloRet.ResponsavelPelaEmissao := tbCliEmite;

            ARetornoWS.DadosRet.TituloRet.SeuNumero     := '';

          end else
          if (LTipoOperacao in [tpConsultaDetalhe,tpAltera]) then
          begin
            AJSonObjectItem := TACBrJSONObject.Create;

            ARetornoWS.DadosRet.IDBoleto.CodBarras      := OnlyNumber(LJsonObject.AsString['bar_code']);
            ARetornoWS.DadosRet.IDBoleto.LinhaDig       := OnlyNumber(LJsonObject.AsString['digitable_line']);
            ARetornoWS.DadosRet.IDBoleto.NossoNum       := LJsonObject.AsString['internal_id'];
            ARetornoWS.DadosRet.IDBoleto.IDBoleto       := LJsonObject.AsString['id'];

            ARetornoWS.DadosRet.TituloRet.NossoNumeroCorrespondente := ARetornoWS.DadosRet.IDBoleto.IDBoleto;
            ARetornoWS.DadosRet.TituloRet.CodBarras     := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig      := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
            ARetornoWS.DadosRet.TituloRet.NossoNumero   := ARetornoWS.DadosRet.IDBoleto.NossoNum;
            ARetornoWS.DadosRet.TituloRet.Carteira      := LJsonObject.AsString['billing_scheme'];

            if LJsonObject.AsString['billing_scheme'] = '3' then
              ARetornoWS.DadosRet.TituloRet.ResponsavelPelaEmissao := tbBancoEmite
            else
              ARetornoWS.DadosRet.TituloRet.ResponsavelPelaEmissao := tbCliEmite;

            ARetornoWS.DadosRet.TituloRet.SeuNumero     := '';

            ARetornoWS.DadosRet.TituloRet.Vencimento      := DateToDateTime(LJsonObject.AsString['due_date']);
            ARetornoWS.DadosRet.TituloRet.ValorDocumento  := LJsonObject.AsCurrency['amount'];
            ARetornoWS.DadosRet.TituloRet.ValorAtual      := LJsonObject.AsCurrency['amount'];


            ARetornoWS.DadosRet.TituloRet.DataCredito     := DateToDateTime(LJsonObject.AsString['payment_date']);
            ARetornoWS.DadosRet.TituloRet.DataBaixa       := DateToDateTime(LJsonObject.AsString['payment_date']);
            ARetornoWS.DadosRet.TituloRet.DataMovimento   := DatetoDateTime( LJsonObject.asString['payment_date'] );
            ARetornoWS.DadosRet.TituloRet.ValorPago       := LJsonObject.AsCurrency['payment_amount'];
            ARetornoWS.DadosRet.TituloRet.ValorRecebido   := LJsonObject.AsCurrency['payment_amount'];

            ARetornoWS.DadosRet.TituloRet.DataRegistro    := DatetoDateTime( LJsonObject.asString['emission_date'] );

            ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca  := LJsonObject.asString['status'];
            //interest juros
            //fine multa
            {Mora}
            if LJsonObject.IsJSONObject('interest') then
            begin
              AJSonObjectItem := LJsonObject.AsJSONObject['interest'];
              if AJSonObjectItem.AsString['type'] = 'P' then
              begin
                ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros := cjTaxaMensal;
                ARetornoWS.DadosRet.TituloRet.ValorMoraJuros  := AJSonObjectItem.AsInteger['value'];
                ARetornoWS.DadosRet.TituloRet.DataMoraJuros   := IncDay(ARetornoWS.DadosRet.TituloRet.Vencimento, AJSonObjectItem.AsInteger['dead_line']);
              end
              else if AJSonObjectItem.AsString['type'] = 'V' then
              begin
                ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros :=  cjValorMensal;
                ARetornoWS.DadosRet.TituloRet.ValorMoraJuros  := AJSonObjectItem.AsCurrency['value'];
                ARetornoWS.DadosRet.TituloRet.DataMoraJuros   := IncDay(ARetornoWS.DadosRet.TituloRet.Vencimento, AJSonObjectItem.AsInteger['dead_line']);
              end
              else
              begin
                ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros :=  cjIsento;
                ARetornoWS.DadosRet.TituloRet.ValorMoraJuros   := 0;
              end;

            end;

            {Multa}
            if LJsonObject.IsJSONObject('fine') then
            begin
              AJSonObjectItem := LJsonObject.AsJSONObject['fine'];
              if AJSonObjectItem.AsString['type'] = 'P' then
              begin
                ARetornoWS.DadosRet.TituloRet.PercentualMulta       := AJSonObjectItem.AsFloat['value'];
                ARetornoWS.DadosRet.TituloRet.MultaValorFixo        := False;
                ARetornoWS.DadosRet.TituloRet.DataMulta             := IncDay(ARetornoWS.DadosRet.TituloRet.Vencimento, AJSonObjectItem.AsInteger['dead_line']);
              end
              else if AJSonObjectItem.AsString['codigo'] = 'V' then
              begin
                ARetornoWS.DadosRet.TituloRet.PercentualMulta       := AJSonObjectItem.AsCurrency['value'];
                ARetornoWS.DadosRet.TituloRet.MultaValorFixo        := True;
                ARetornoWS.DadosRet.TituloRet.ValorMulta            := ARetornoWS.DadosRet.TituloRet.PercentualMulta;
                ARetornoWS.DadosRet.TituloRet.DataMulta             := IncDay(ARetornoWS.DadosRet.TituloRet.Vencimento, AJSonObjectItem.AsInteger['dead_line']);
              end
              else
              begin
                ARetornoWS.DadosRet.TituloRet.PercentualMulta       := 0;
                ARetornoWS.DadosRet.TituloRet.ValorMulta            := 0;
                ARetornoWS.DadosRet.TituloRet.MultaValorFixo        := False;
              end;

            end;

            {Desconto1}
            if LJsonObject.IsJSONObject('discount') then
            begin
              AJSonObjectItem := LJsonObject.AsJSONObject['discount'];

              ARetornoWS.DadosRet.TituloRet.CodigoDesconto := cdSemDesconto;
              if AJSonObjectItem.AsJSONObject['first'].AsString['discount_type'] = 'V' then
                ARetornoWS.DadosRet.TituloRet.CodigoDesconto := cdValorFixo
              else
                ARetornoWS.DadosRet.TituloRet.CodigoDesconto := cdPercentual;

              ARetornoWS.DadosRet.TituloRet.ValorDesconto  := AJSonObjectItem.AsJSONObject['first'].AsCurrency['value'];
              ARetornoWS.DadosRet.TituloRet.DataDesconto   := IncDay(ARetornoWS.DadosRet.TituloRet.Vencimento, AJSonObjectItem.AsJSONObject['first'].AsInteger['dead_line']);

              if AJSonObjectItem.IsJSONObject('second') then
              begin
                ARetornoWS.DadosRet.TituloRet.ValorDesconto  := AJSonObjectItem.AsJSONObject['second'].AsCurrency['value'];
                ARetornoWS.DadosRet.TituloRet.DataDesconto   := IncDay(ARetornoWS.DadosRet.TituloRet.Vencimento, AJSonObjectItem.AsJSONObject['second'].AsInteger['dead_line']);
              end;
              if AJSonObjectItem.IsJSONObject('third') then
              begin
                ARetornoWS.DadosRet.TituloRet.ValorDesconto  := AJSonObjectItem.AsJSONObject['third'].AsCurrency['value'];
                ARetornoWS.DadosRet.TituloRet.DataDesconto   := IncDay(ARetornoWS.DadosRet.TituloRet.Vencimento, AJSonObjectItem.AsJSONObject['third'].AsInteger['dead_line']);
              end;
            end;
            {Desconto1}
            if LJsonObject.IsJSONObject('payer') then
            begin
              AJSonObjectItem := LJsonObject.AsJSONObject['payer'];
              ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado     := AJSonObjectItem.asString['name'];
              ARetornoWS.DadosRet.TituloRet.Sacado.Cidade         := AJSonObjectItem.AsJSONObject['address'].asString['city'];
              ARetornoWS.DadosRet.TituloRet.Sacado.UF             := AJSonObjectItem.AsJSONObject['address'].asString['state'];
              ARetornoWS.DadosRet.TituloRet.Sacado.Bairro         := '';
              ARetornoWS.DadosRet.TituloRet.Sacado.Cep            := AJSonObjectItem.AsJSONObject['address'].asString['zip_code'];
              ARetornoWS.DadosRet.TituloRet.Sacado.Numero         := IntToStr(AJSonObjectItem.AsJSONObject['address'].AsInt64['number']);
              ARetornoWS.DadosRet.TituloRet.Sacado.Logradouro     := AJSonObjectItem.AsJSONObject['address'].asString['street'];
              ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF        := AJSonObjectItem.asString['tax_id'];
              ARetornoWS.DadosRet.TituloRet.Sacado.Email          := AJSonObjectItem.asString['email'];
            end;
          end;
        end;
      except
        Result := False;
      end;
    finally
      LJsonObject.free;
    end;
  end;
end;

function TRetornoEnvio_C6.LinhaDigitavelExplodeInfo(const AValue : string; const ATipoInformacao : TLinhaDigitavelInfo) : string;
var LValue : string;
begin
  LValue := OnlyNumber(AValue);
  case ATipoInformacao of
    TLDCodigoCedente      : Result := Copy(LValue, 5, 5)  + Copy(LValue, 11, 7);
    TLDNossoNumero        : Result := Copy(LValue, 18, 3) + Copy(LValue, 22, 7);
    TLDCodigoCarteira     : Result := Copy(LValue, 29, 2);
    TLDIdentificacaoLayout: Result := Copy(LValue, 31, 1);
    TLDFatorVencimento    : Result := Copy(LValue, 34, 4);
    TLDValorDocumento     : Result := FloatToStr(StrToInt(Copy(LValue, 38, 10)) / 100);
  end;
end;

function TRetornoEnvio_C6.LerListaRetorno: Boolean;
var
  LListaRetorno: TACBrBoletoRetornoWS;
  LJsonObject, LJsonObjectItem: TACBrJSONObject;
  LMensagemRejeicao: TACBrBoletoRejeicao;
  LJsonArray: TACBrJSONArray;
  I, X: Integer;
  LSituacao : AnsiString;
begin
  Result := True;

  LListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;
  LListaRetorno.HTTPResultCode := HTTPResultCode;
  LListaRetorno.JSONEnvio      := EnvWs;
  if RetWS <> '' then
  begin
    try
      LJsonObject := TACBrJSONObject.Parse(RetWS);
      try
        case HTTPResultCode of
          400, 404 : begin
          if  NaoEstaVazio( LJsonObject.asString['codigo'] ) then
              begin
                LMensagemRejeicao            := LListaRetorno.CriarRejeicaoLista;
                LMensagemRejeicao.Codigo     := LJsonObject.AsString['codigo'];
                LMensagemRejeicao.Versao     := LJsonObject.AsString['parametro'];
                LMensagemRejeicao.Mensagem   := LJsonObject.AsString['mensagem'];
              end;
          end;
        end;

        LListaRetorno.JSON := LJsonObject.ToJSON;

        if (LListaRetorno.ListaRejeicao.Count = 0) then
        begin
          if not(LJsonObject.AsBoolean['last']) then
          begin
            LListaRetorno.indicadorContinuidade := True;
            X := Trunc(ACBrBoleto.Configuracoes.WebService.Filtro.indiceContinuidade );
            LListaRetorno.proximoIndice := (X+1)
          end
          else
          begin
            LListaRetorno.indicadorContinuidade := False;
            LListaRetorno.proximoIndice := 0;
          end;

          LJsonArray := LJsonObject.AsJSONArray['content'];
          for I := 0 to Pred(LJsonArray.Count) do
          begin
            if I > 0 then
              LListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;

            LJsonObjectItem  := LJsonArray.ItemAsJSONObject[I];

            LListaRetorno.DadosRet.IDBoleto.CodBarras             := LJsonObjectItem.AsString['codigoBarras'];
            LListaRetorno.DadosRet.IDBoleto.LinhaDig              := LJsonObjectItem.AsString['linhaDigitavel'];
            LListaRetorno.DadosRet.IDBoleto.NossoNum              := LJsonObjectItem.AsString['nossoNumero'];
            LListaRetorno.indicadorContinuidade                   := False;
            LListaRetorno.DadosRet.TituloRet.CodBarras            := LListaRetorno.DadosRet.IDBoleto.CodBarras;
            LListaRetorno.DadosRet.TituloRet.LinhaDig             := LListaRetorno.DadosRet.IDBoleto.LinhaDig;

            LListaRetorno.DadosRet.TituloRet.NossoNumero          := LListaRetorno.DadosRet.IDBoleto.NossoNum;
            LListaRetorno.DadosRet.TituloRet.Vencimento           := DatetoDateTime(LJsonObjectItem.AsString['dataVencimento']);

            LListaRetorno.DadosRet.TituloRet.ValorDocumento       := LJsonObjectItem.AsCurrency['valorNominal'];
            LListaRetorno.DadosRet.TituloRet.ValorAtual           := LJsonObjectItem.AsCurrency['valorNominal'];
            LListaRetorno.DadosRet.TituloRet.ValorPago            := LJsonObjectItem.AsCurrency['valorTotalRecebimento'];

            if LJsonObjectItem.AsJSONObject['mora'].AsString['codigo'] = 'TAXAMENSAL' then
            begin
              LListaRetorno.DadosRet.TituloRet.CodigoMoraJuros :=  cjTaxaMensal;
              LListaRetorno.DadosRet.TituloRet.ValorMoraJuros  := LJsonObjectItem.AsJSONObject['mora'].AsCurrency['taxa'];
              LListaRetorno.DadosRet.TituloRet.DataMoraJuros   := DatetoDateTime(LJsonObjectItem.AsJSONObject['mora'].AsString['data']);
            end
            else if LJsonObjectItem.AsJSONObject['mora'].AsString['codigo'] = 'VALORDIA'then
            begin
              LListaRetorno.DadosRet.TituloRet.CodigoMoraJuros :=  cjValorDia;
              LListaRetorno.DadosRet.TituloRet.ValorMoraJuros  := LJsonObjectItem.AsJSONObject['mora'].AsCurrency['valor'];
              LListaRetorno.DadosRet.TituloRet.DataMoraJuros   := DatetoDateTime(LJsonObjectItem.AsJSONObject['mora'].AsString['data']);
            end
            else
            begin
              LListaRetorno.DadosRet.TituloRet.CodigoMoraJuros :=  cjIsento;
              LListaRetorno.DadosRet.TituloRet.ValorMoraJuros   := 0;
            end;

            if LJsonObjectItem.AsJSONObject['multa'].AsString['codigo'] = 'PERCENTUAL' then
            begin
              LListaRetorno.DadosRet.TituloRet.PercentualMulta       :=  LJsonObjectItem.AsJSONObject['multa'].AsFloat['taxa'];
              LListaRetorno.DadosRet.TituloRet.MultaValorFixo        := False;
              LListaRetorno.DadosRet.TituloRet.DataMulta             := DatetoDateTime(LJsonObjectItem.AsJSONObject['multa'].AsString['data']);
            end
            else if LJsonObjectItem.AsJSONObject['multa'].AsString['codigo'] = 'VALORFIXO' then
            begin
              LListaRetorno.DadosRet.TituloRet.PercentualMulta       := LJsonObjectItem.AsJSONObject['multa'].AsFloat['valor'];
              LListaRetorno.DadosRet.TituloRet.MultaValorFixo        := True;
              LListaRetorno.DadosRet.TituloRet.ValorMulta            := LListaRetorno.DadosRet.TituloRet.PercentualMulta;
              LListaRetorno.DadosRet.TituloRet.DataMulta             := DatetoDateTime(LJsonObjectItem.AsJSONObject['multa'].AsString['data']);
            end
            else
            begin
              LListaRetorno.DadosRet.TituloRet.PercentualMulta       := 0;
              LListaRetorno.DadosRet.TituloRet.ValorMulta            := 0;
              LListaRetorno.DadosRet.TituloRet.MultaValorFixo        := False;
            end;


            if LJsonObjectItem.AsJSONObject['desconto1'].AsString['codigo'] = 'VALORFIXODATAINFORMADA' then
            begin
              LListaRetorno.DadosRet.TituloRet.ValorDesconto  := LJsonObjectItem.AsJSONObject['desconto1'].AsCurrency['valor'];
              LListaRetorno.DadosRet.TituloRet.CodigoDesconto := cdValorFixo;
              LListaRetorno.DadosRet.TituloRet.DataDesconto   := DatetoDateTime(LJsonObjectItem.AsJSONObject['desconto1'].AsString['data']);
            end
            else if LJsonObjectItem.AsJSONObject['desconto1'].AsString['codigo'] = 'PERCENTUALDATAINFORMADA' then
            begin
              LListaRetorno.DadosRet.TituloRet.ValorDesconto := LJsonObjectItem.AsJSONObject['desconto1'].AsFloat['taxa'];
              LListaRetorno.DadosRet.TituloRet.CodigoDesconto := cdPercentual;
              LListaRetorno.DadosRet.TituloRet.DataDesconto   := DatetoDateTime(LJsonObjectItem.AsJSONObject['desconto1'].AsString['data']);
            end
            else
            begin
              LListaRetorno.DadosRet.TituloRet.ValorDesconto := 0;
              LListaRetorno.DadosRet.TituloRet.CodigoDesconto := cdSemDesconto;
            end;

            
            LListaRetorno.DadosRet.TituloRet.SeuNumero            := LJsonObjectItem.asString['seuNumero'];
            LListaRetorno.DadosRet.TituloRet.DataRegistro         := DatetoDateTime( LJsonObjectItem.asString['dataEmissao'] );
            LListaRetorno.DadosRet.TituloRet.Vencimento           := DatetoDateTime( LJsonObjectItem.asString['dataVencimento'] );

            LListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca := LJsonObjectItem.asString['situacao'];
            LListaRetorno.DadosRet.TituloRet.DataMovimento        := DatetoDateTime( LJsonObjectItem.asString['dataHoraSituacao'] );
            LListaRetorno.DadosRet.TituloRet.DataCredito          := DatetoDateTime( LJsonObjectItem.asString['dataHoraSituacao'] );

            LListaRetorno.DadosRet.TituloRet.Sacado.NomeSacado    := LJsonObjectItem.AsJSONObject['pagador'].asString['nome'];
            LListaRetorno.DadosRet.TituloRet.Sacado.Cidade        := LJsonObjectItem.AsJSONObject['pagador'].asString['cidade'];
            LListaRetorno.DadosRet.TituloRet.Sacado.UF            := LJsonObjectItem.AsJSONObject['pagador'].asString['uf'];
            LListaRetorno.DadosRet.TituloRet.Sacado.Bairro        := LJsonObjectItem.AsJSONObject['pagador'].asString['bairro'];
            LListaRetorno.DadosRet.TituloRet.Sacado.Cep           := LJsonObjectItem.AsJSONObject['pagador'].asString['cep'];
            LListaRetorno.DadosRet.TituloRet.Sacado.Numero        := LJsonObjectItem.AsJSONObject['pagador'].asString['numero'];
            LListaRetorno.DadosRet.TituloRet.Sacado.Logradouro    := LJsonObjectItem.AsJSONObject['pagador'].asString['logradouro'];
            LListaRetorno.DadosRet.TituloRet.Sacado.CNPJCPF       := LJsonObjectItem.AsJSONObject['pagador'].asString['cpfCnpj'];
            LSituacao := AnsiUpperCase(LJsonObjectItem.asString['situacao']);
            if ( LSituacao = C_CANCELADO ) or
               ( LSituacao = C_EXPIRADO ) or
               ( LSituacao = C_PAGO ) or
               ( LSituacao = C_EXPIRADO ) then
             begin
                if LJsonObjectItem.asString['situacao'] = C_PAGO then
                  begin
                  LListaRetorno.DadosRet.TituloRet.ValorRecebido        := LJsonObjectItem.AsCurrency['valorTotalRecebimento'];
                  LListaRetorno.DadosRet.TituloRet.ValorPago            := LJsonObjectItem.AsCurrency['valorTotalRecebimento'];
                  end;
                LListaRetorno.DadosRet.TituloRet.DataBaixa    := DatetoDateTime( LJsonObjectItem.asString['dataHoraSituacao'] )
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

function TRetornoEnvio_C6.RetornoEnvio(const AIndex: Integer): Boolean;
begin
  Result := inherited RetornoEnvio(AIndex);
end;

end.

