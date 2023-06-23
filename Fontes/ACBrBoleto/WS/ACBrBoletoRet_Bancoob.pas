{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{ Colaboradores nesse arquivo:  Victor Hugo Gonzales - Panda, Marcelo Santos,  }
{ Delmar de Lima                                                               }
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
//incluido em 18/04/2023

{$I ACBr.inc}

unit ACBrBoletoRet_Bancoob;

interface

uses
  Classes,
  SysUtils,
  ACBrBoleto,
  ACBrBoletoWS,
  ACBrBoletoRetorno,
  Jsons,
  DateUtils,
  pcnConversao,
  ACBrBoletoWS.Rest,
  ACBrUtil.Base;

type

{ TRetornoEnvio_Sicoob_API }

 TRetornoEnvio_Bancoob = class(TRetornoEnvioREST)
 private
   function DateBancoobToDateTime(Const AValue : String) : TDateTime;
 public
   constructor Create(ABoletoWS: TACBrBoleto); override;
   destructor  Destroy; Override;
   function LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean; override;
   function LerListaRetorno: Boolean; override;
   function RetornoEnvio(const AIndex: Integer): Boolean; override;

 end;

implementation

uses
  ACBrBoletoConversao;

resourcestring
  C_CANCELADO = 'CANCELADO';
  C_EXPIRADO = 'EXPIRADO';
  C_VENCIDO = 'VENCIDO';
  C_EMABERTO = 'EMABERTO';
  C_PAGO = 'Liquidado';


{ TRetornoEnvio }

constructor TRetornoEnvio_Bancoob.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);

end;

function TRetornoEnvio_Bancoob.DateBancoobToDateTime(
  const AValue: String): TDateTime;
var
 data, ano, mes, dia : String;
begin
  ano := Copy( aValue, 0,4 );
  mes := Copy( aValue, 6,2 );
  dia := Copy( aValue, 9,2 );
  data := Format( '%s/%s/%s' , [dia,mes,ano]);
  Result := StrToDateDef( data ,0 );
end;

destructor TRetornoEnvio_Bancoob.Destroy;
begin
  inherited Destroy;
end;

function TRetornoEnvio_Bancoob.LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;
var
  //Retorno: TRetEnvio;
  AJson: TJson;
  AJSonObject, AJsonListaHistoricoObject: TJsonObject;
  ARejeicao: TACBrBoletoRejeicao;
  aJsonViolacao:TJsonObject;
  aJsonViolacoes, AJsonListaHistoricoArray: TJsonArray;
  TipoOperacao : TOperacao;
  x, i, vPos :Integer;
begin
  Result := True;
  TipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;

  ARetornoWs.JSONEnvio      := EnvWs;
  ARetornoWS.HTTPResultCode := HTTPResultCode;

  if RetWS <> '' then
  begin
    try
      AJSon := TJson.Create;
      try
        AJSon.Parse(RetWS);
        ARetornoWS.JSON := (aJson.Values['resultado'].Stringify);

        case HttpResultCode of
          207:
          begin
            aJsonViolacoes := aJson.Values['resultado'].AsArray;
            for x := 0 to aJsonViolacoes.Count -1 do
            begin
              aJsonViolacao        := aJsonViolacoes[x].AsObject;
              if (aJSonViolacao.Values['status'].AsObject.Values['codigo'].AsString <> '200') then
              begin
                ARejeicao            := ARetornoWS.CriarRejeicaoLista;
                ARejeicao.Codigo     := aJSonViolacao.Values['status'].AsObject.Values['codigo'].AsString;
                ARejeicao.mensagem   := aJSonViolacao.Values['status'].AsObject.Values['mensagem'].AsString;
              end;
            end;
          end;
          400,406,500 :
            begin
              aJsonViolacoes := aJson.Values['mensagens'].AsArray;
              for x := 0 to aJsonViolacoes.Count -1 do
                begin
                    aJsonViolacao        := aJsonViolacoes[x].AsObject;
                    ARejeicao            := ARetornoWS.CriarRejeicaoLista;
                    ARejeicao.Codigo     := AJsonViolacao.Values['codigo'].AsString;
                    ARejeicao.mensagem     := AJsonViolacao.Values['mensagem'].AsString;
                end;
            end;
        end;

        //retorna quando tiver sucesso
        if (ARetornoWS.ListaRejeicao.Count = 0) then
        begin
          if (TipoOperacao = tpInclui) then
          begin
            AJSonObject := aJson.Values['resultado'].AsArray.Items[0].AsObject.Values['boleto'].AsObject;

            ARetornoWS.DadosRet.IDBoleto.CodBarras      := AJSonObject.Values['codigoBarras'].AsString;
            ARetornoWS.DadosRet.IDBoleto.LinhaDig       := AJSonObject.Values['linhaDigitavel'].AsString;
            ARetornoWS.DadosRet.IDBoleto.NossoNum       := AJSonObject.Values['nossoNumero'].AsString;
            ARetornoWS.DadosRet.IDBoleto.URLPDF         := AJSonObject.Values['pdfBoleto'].AsString;

            ARetornoWS.DadosRet.TituloRet.CodBarras      := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig       := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
            ARetornoWS.DadosRet.TituloRet.NossoNumero    := ARetornoWS.DadosRet.IDBoleto.NossoNum;
            ARetornoWS.DadosRet.TituloRet.EMV            := AJSonObject.Values['qrcode'].AsString;
            ARetornoWS.DadosRet.TituloRet.Vencimento     := DateBancoobToDateTime(AJSonObject.Values['dataVencimento'].AsString);
            ARetornoWS.DadosRet.TituloRet.NossoNumero    := AJSonObject.Values['nossoNumero'].AsString;
            ARetornoWS.DadosRet.TituloRet.SeuNumero      := AJSonObject.Values['seuNumero'].AsString;
            ARetornoWS.DadosRet.TituloRet.EspecieDoc     := AJSonObject.Values['especieDocumento'].AsString;
            ARetornoWS.DadosRet.TituloRet.DataDocumento  := DateBancoobToDateTime(AJSonObject.Values['dataEmissao'].AsString);
            ARetornoWS.DadosRet.TituloRet.ValorDocumento := AJSonObject.Values['valor'].AsNumber;
            ARetornoWS.DadosRet.TituloRet.ValorDesconto  := AJSonObject.Values['valorPrimeiroDesconto'].AsNumber;

          end
          else if (TipoOperacao = tpConsultaDetalhe) then
          begin
            aJsonObject := TJsonObject.Create;
            aJsonObject.Parse( aJson.Values['resultado'].Stringify );
            ARetornoWS.DadosRet.IDBoleto.CodBarras       := AJSonObject.Values['codigoBarras'].AsString;
            ARetornoWS.DadosRet.IDBoleto.LinhaDig        := AJSonObject.Values['linhaDigitavel'].AsString;
            ARetornoWS.DadosRet.IDBoleto.NossoNum        := AJSonObject.Values['nossoNumero'].AsString;
            ARetornoWS.indicadorContinuidade             := false;

            ARetornoWS.DadosRet.TituloRet.NossoNumero     := ARetornoWS.DadosRet.IDBoleto.NossoNum;
            ARetornoWS.DadosRet.TituloRet.Vencimento      := DateBancoobToDateTime(AJSonObject.Values['dataVencimento'].AsString);
            ARetornoWS.DadosRet.TituloRet.ValorDocumento  := AJSonObject.Values['valor'].AsNumber;

            case AJSonObject.Values['tipoMulta'].AsInteger of
             1 : begin // Multa Valor Fixo
              ARetornoWS.DadosRet.TituloRet.PercentualMulta  := AJSonObject.Values['valorMulta'].AsNumber;
              ARetornoWS.DadosRet.TituloRet.DataMulta        := DateBancoobToDateTime(AJSonObject.Values['dataMulta'].AsString);
              ARetornoWS.DadosRet.TituloRet.DataMoraJuros    := DateBancoobToDateTime(AJSonObject.Values['dataJurosMora'].AsString);
             end;
             2 : begin // Multa com Valor em Percentual
              ARetornoWS.DadosRet.TituloRet.PercentualMulta := AJSonObject.Values['valorMulta'].AsNumber;
              ARetornoWS.DadosRet.TituloRet.DataMulta        := DateBancoobToDateTime(AJSonObject.Values['dataMulta'].AsString);
              ARetornoWS.DadosRet.TituloRet.DataMoraJuros    := DateBancoobToDateTime(AJSonObject.Values['dataJurosMora'].AsString);
             end;
             3 : begin // Sem Multa
               ARetornoWS.DadosRet.TituloRet.PercentualMulta := 0;
             end;

            end;

            case AJSonObject.Values['tipoJurosMora'].AsInteger of
             0 : begin // Isento
               ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros := cjIsento;
               ARetornoWS.DadosRet.TituloRet.ValorMoraJuros  := AJSonObject.Values['valorJurosMora'].AsNumber;
             end;
             2 : begin // Valor Dia
               ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros := cjValorDia;
               ARetornoWS.DadosRet.TituloRet.ValorMoraJuros  := AJSonObject.Values['valorJurosMora'].AsNumber;
             end;
             3 : begin // Taxa Mensal
               ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros := cjTaxaMensal;
               ARetornoWS.DadosRet.TituloRet.ValorMoraJuros  := AJSonObject.Values['valorJurosMora'].AsNumber;
             end;

            end;

            ARetornoWS.DadosRet.TituloRet.CodBarras       := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig        := ARetornoWS.DadosRet.IDBoleto.LinhaDig;

            ARetornoWS.DadosRet.TituloRet.SeuNumero       := AJSonObject.Values['seuNumero'].asString;
            ARetornoWS.DadosRet.TituloRet.DataRegistro    := DateBancoobToDateTime( AJSonObject.Values['dataEmissao'].asString );
            ARetornoWS.DadosRet.TituloRet.Vencimento      := DateBancoobToDateTime( AJSonObject.Values['dataVencimento'].asString );

            ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca  := AJSonObject.Values['situacaoBoleto'].asString;
            ARetornoWS.DadosRet.TituloRet.UrlPix                := AJSonObject.Values['qrCode'].AsString;

            //ARetornoWS.DadosRet.TituloRet.ValorAtual      := AJSonObject.Values['valor'].AsNumber;
            //ARetornoWS.DadosRet.TituloRet.ValorPago       := AJSonObject.Values['valorTotalRecebimento'].AsNumber;
            //ARetornoWS.DadosRet.TituloRet.DataMovimento         := DateBancoobToDateTime( AJSonObject.Values['dataHoraSituacao'].asString );
            //ARetornoWS.DadosRet.TituloRet.DataCredito           := DateBancoobToDateTime( AJSonObject.Values['dataHoraSituacao'].asString );

            ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado     := AJSonObject.Values['pagador'].asObject.Values['nome'].asString;
            ARetornoWS.DadosRet.TituloRet.Sacado.Cidade         := AJSonObject.Values['pagador'].asObject.Values['cidade'].asString;
            ARetornoWS.DadosRet.TituloRet.Sacado.UF             := AJSonObject.Values['pagador'].asObject.Values['uf'].asString;;
            ARetornoWS.DadosRet.TituloRet.Sacado.Bairro         := AJSonObject.Values['pagador'].asObject.Values['bairro'].asString;;
            ARetornoWS.DadosRet.TituloRet.Sacado.Cep            := AJSonObject.Values['pagador'].asObject.Values['cep'].asString;;
            ARetornoWS.DadosRet.TituloRet.Sacado.Numero         := AJSonObject.Values['pagador'].asObject.Values['numero'].asString;;
            ARetornoWS.DadosRet.TituloRet.Sacado.Logradouro     := AJSonObject.Values['pagador'].asObject.Values['endereco'].asString;;
            ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF        := AJSonObject.Values['pagador'].asObject.Values['numeroCpfCnpj'].asString;

            if(AJSonObject.Values['situacaoBoleto'].asString = C_PAGO) then
            begin
             // WorkAround para pegar DataPagamento e Valor de Pagamento

             AJsonListaHistoricoArray :=  AJSonObject.Values['listaHistorico'].AsArray;

              for i := 0 to Pred(AJsonListaHistoricoArray.Count) do
              begin
                AJsonListaHistoricoObject        := AJsonListaHistoricoArray[i].AsObject;

                if AJsonListaHistoricoObject.Values['tipoHistorico'].AsInteger = 6 then // 1 = Entrada, 4 = Tarifa Liquidação, 6 = Liquidação
                begin
                 ARetornoWS.DadosRet.TituloRet.DataBaixa := DateBancoobToDateTime(AJsonListaHistoricoObject.Values['dataHistorico'].AsString);

                 vPos := Pos('R$', AJsonListaHistoricoObject.Values['descricaoHistorico'].AsString);

                 ARetornoWS.DadosRet.TituloRet.ValorPago := 0;

                 if vpos > 0 then
                  ARetornoWS.DadosRet.TituloRet.ValorPago := StringToFloatDef(copy(AJsonListaHistoricoObject.Values['descricaoHistorico'].AsString, vPos+2, length(AJsonListaHistoricoObject.Values['descricaoHistorico'].AsString)), 0);
                end;
              end;
            end;

           { if(AJSonObject.Values['situacaoBoleto'].asString = C_CANCELADO) or (AJSonObject.Values['situacaoBoleto'].asString = C_EXPIRADO) or  (AJSonObject.Values['situacaoBoleto'].asString = C_PAGO) or (AJSonObject.Values['situacaoBoleto'].asString = C_EXPIRADO) then
            begin
              ARetornoWS.DadosRet.TituloRet.ValorPago                   := AJSonObject.Values['valorNominal'].AsNumber;
              ARetornoWS.DadosRet.TituloRet.DataBaixa                   := DateBancoobToDateTime( AJSonObject.Values['dataHoraSituacao'].asString )
            end;}

          end
          else if (TipoOperacao = tpBaixa) then
          begin
            // não possui dados de retorno..
          end
          else if (TipoOperacao = tpAltera) then
          begin
            // não possui dados de retorno..
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

function TRetornoEnvio_Bancoob.LerListaRetorno: Boolean;
var
  ListaRetorno: TACBrBoletoRetornoWS;
  AJson: TJson;
  AJSonObject: TJsonObject;
  ARejeicao: TACBrBoletoRejeicao;
  AJsonBoletos: TJsonArray;
  I: Integer;
  aJsonString:String;
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

        case HTTPResultCode of
          400, 404 : begin
            if ( AJson.StructType = jsObject ) then
              if( AJson.Values['codigo'].asString <> '' ) then
              begin
                ARejeicao            := ListaRetorno.CriarRejeicaoLista;
                ARejeicao.Codigo     := AJson.Values['codigo'].AsString;
                ARejeicao.Versao     := AJson.Values['parametro'].AsString;
                ARejeicao.Mensagem   := AJson.Values['mensagem'].AsString;
              end;
          end;
        end;

        //retorna quando tiver sucesso
        if (ListaRetorno.ListaRejeicao.Count = 0) then
        begin
          aJsonString := aJson.Stringify;
          aJsonString := AJson.Values['content'].Stringify;
          AJsonBoletos := AJson.Values['content'].AsArray;
          for I := 0 to Pred(AJsonBoletos.Count) do
          begin
            if I > 0 then
              ListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;

            AJSonObject  := AJsonBoletos[I].AsObject;

            ListaRetorno.DadosRet.IDBoleto.CodBarras             := AJSonObject.Values['codigoBarras'].AsString;;;
            ListaRetorno.DadosRet.IDBoleto.LinhaDig              := AJSonObject.Values['linhaDigitavel'].AsString;;
            ListaRetorno.DadosRet.IDBoleto.NossoNum              := AJSonObject.Values['nossoNumero'].AsString;
            ListaRetorno.indicadorContinuidade                   := false;
            ListaRetorno.DadosRet.TituloRet.CodBarras            := ListaRetorno.DadosRet.IDBoleto.CodBarras;
            ListaRetorno.DadosRet.TituloRet.LinhaDig             := ListaRetorno.DadosRet.IDBoleto.LinhaDig;

            ListaRetorno.DadosRet.TituloRet.NossoNumero          := ListaRetorno.DadosRet.IDBoleto.NossoNum;
            ListaRetorno.DadosRet.TituloRet.Vencimento           := DateBancoobToDateTime(AJSonObject.Values['dataVencimento'].AsString);

            ListaRetorno.DadosRet.TituloRet.ValorDocumento       := AJSonObject.Values['valorNominal'].AsNumber;
            ListaRetorno.DadosRet.TituloRet.ValorAtual           := AJSonObject.Values['valorNominal'].AsNumber;
            ListaRetorno.DadosRet.TituloRet.ValorPago            := AJSonObject.Values['valorTotalRecebimento'].AsNumber;
            ListaRetorno.DadosRet.TituloRet.ValorMoraJuros       := AJSonObject.Values['mora'].asObject.Values['valor'].asNumber;
            ListaRetorno.DadosRet.TituloRet.PercentualMulta      := AJSonObject.Values['multa'].asObject.Values['taxa'].asNumber;

            ListaRetorno.DadosRet.TituloRet.SeuNumero            := AJSonObject.Values['seuNumero'].asString;
            ListaRetorno.DadosRet.TituloRet.DataRegistro         := DateBancoobToDateTime( AJSonObject.Values['dataEmissao'].asString );
            ListaRetorno.DadosRet.TituloRet.Vencimento           := DateBancoobToDateTime( AJSonObject.Values['dataVencimento'].asString );

            ListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca := AJSonObject.Values['situacao'].asString;
            ListaRetorno.DadosRet.TituloRet.DataMovimento        := DateBancoobToDateTime( AJSonObject.Values['dataHoraSituacao'].asString );
            ListaRetorno.DadosRet.TituloRet.DataCredito          := DateBancoobToDateTime( AJSonObject.Values['dataHoraSituacao'].asString );

            ListaRetorno.DadosRet.TituloRet.Sacado.NomeSacado    := AJSonObject.Values['pagador'].asObject.Values['nome'].asString;
            ListaRetorno.DadosRet.TituloRet.Sacado.Cidade        := AJSonObject.Values['pagador'].asObject.Values['cidade'].asString;
            ListaRetorno.DadosRet.TituloRet.Sacado.UF            := AJSonObject.Values['pagador'].asObject.Values['uf'].asString;
            ListaRetorno.DadosRet.TituloRet.Sacado.Bairro        := AJSonObject.Values['pagador'].asObject.Values['bairro'].asString;
            ListaRetorno.DadosRet.TituloRet.Sacado.Cep           := AJSonObject.Values['pagador'].asObject.Values['cep'].asString;
            ListaRetorno.DadosRet.TituloRet.Sacado.Numero        := AJSonObject.Values['pagador'].asObject.Values['numero'].asString;
            ListaRetorno.DadosRet.TituloRet.Sacado.Logradouro    := AJSonObject.Values['pagador'].asObject.Values['logradouro'].asString;
            ListaRetorno.DadosRet.TituloRet.Sacado.CNPJCPF       := AJSonObject.Values['pagador'].asObject.Values['cpfCnpj'].asString;



              if( AJSonObject.Values['situacao'].asString = C_CANCELADO ) or
                 ( AJSonObject.Values['situacao'].asString = C_EXPIRADO ) or
                 ( AJSonObject.Values['situacao'].asString = C_PAGO ) or
                 ( AJSonObject.Values['situacao'].asString = C_EXPIRADO ) then
                 begin
                    ListaRetorno.DadosRet.TituloRet.ValorPago                   := AJSonObject.Values['valorNominal'].AsNumber;
                    ListaRetorno.DadosRet.TituloRet.DataBaixa                   := DateBancoobToDateTime( AJSonObject.Values['dataHoraSituacao'].asString )
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

function TRetornoEnvio_Bancoob.RetornoEnvio(const AIndex: Integer): Boolean;
begin
  Result:=inherited RetornoEnvio(AIndex);
end;

end.

