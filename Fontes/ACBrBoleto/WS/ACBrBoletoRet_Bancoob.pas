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
  DateUtils,
  pcnConversao,
  ACBrBoletoWS.Rest,
  ACBrUtil.Base,
  ACBrJSON;

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
  C_BAIXADO = 'BAIXADO';
  C_EXPIRADO = 'EXPIRADO';
  C_VENCIDO = 'VENCIDO';
  C_EMABERTO = 'EM ABERTO';
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
  LJson, LJSonObject, LJsonListaHistoricoObject, LJsonViolacao : TACBrJSONObject;
  LJsonListaHistoricoArray, LJsonViolacoesArray: TACBrJSONArray;
  LRejeicao: TACBrBoletoRejeicao;
  TipoOperacao : TOperacao;
  x, i, vPos :Integer;
begin
  Result := True;
  TipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;
  ARetornoWS.HTTPResultCode  := HTTPResultCode;
  ARetornoWS.JSONEnvio       := EnvWs;
  ARetornoWS.Header.Operacao := TipoOperacao;

  if (HttpResultCode = 204) and (TipoOperacao = tpConsultaDetalhe) then
  begin
    HttpResultCode   := 400;
    Result := False;
    LRejeicao := ARetornoWS.CriarRejeicaoLista;
    LRejeicao.Codigo := '400';
    LRejeicao.Ocorrencia  := '204';
    LRejeicao.Mensagem := 'A requisição foi processada com êxito e não está retornando conteúdo.';
  end;

  if Trim(RetWS) <> '' then
  begin
    try
      LJson := TACBrJSONObject.Parse(RetWS);
      try
        ARetornoWS.JSON :=  LJson.ToJSON;
        //if HTTPResultCode >= 300 then
        //begin
          if LJson.IsJSONArray('resultado') then
          begin
            LJsonViolacoesArray := LJson.AsJSONArray['resultado'];
            for x := 0 to LJsonViolacoesArray.Count-1 do
            begin
              LJsonViolacao := LJsonViolacoesArray.ItemAsJSONObject[x];
              if (LJsonViolacao.AsJSONObject['status'].AsInteger['codigo'] > 299) then
              begin
                LRejeicao            := ARetornoWS.CriarRejeicaoLista;
                LRejeicao.Codigo     := LJsonViolacao.AsJSONObject['status'].AsString['codigo'];
                LRejeicao.mensagem   := LJsonViolacao.AsJSONObject['status'].AsString['mensagem'];
              end;
            end;
          end;
        //end;
        if LJson.ValueExists('httpCode') then
        begin
          LRejeicao            := ARetornoWS.CriarRejeicaoLista;
          LRejeicao.Codigo     := LJson.AsString['httpCode'];
          LRejeicao.mensagem   := LJson.AsString['httpMessage'] + '. ' + LJson.AsString['moreInformation'];
          ARetornoWS.MsgRetorno:= LJson.AsString['httpMessage'] + '. ' + LJson.AsString['moreInformation'];
        end;

        if LJson.IsJSONObject('resultado') and LJson.AsJSONObject['resultado'].IsJSONArray('mensagens') then
        begin
          LJsonViolacoesArray := LJson.AsJSONArray['mensagens'];
          for x := 0 to LJsonViolacoesArray.Count-1 do
          begin
            LJsonViolacao        := LJsonViolacoesArray.ItemAsJSONObject[x];
            LRejeicao            := ARetornoWS.CriarRejeicaoLista;
            LRejeicao.Codigo     := LJsonViolacao.AsString['codigo'];
            LRejeicao.mensagem   := LJsonViolacao.AsString['mensagem'];
          end;
        end;


        //retorna quando tiver sucesso
        if (ARetornoWS.ListaRejeicao.Count = 0) then
        begin
          if (TipoOperacao = tpInclui) then
          begin
            LJSonObject := LJson.AsJSONArray['resultado'].ItemAsJSONObject[0].AsJSONObject['boleto'];
            ARetornoWS.DadosRet.IDBoleto.CodBarras      := LJSonObject.AsString['codigoBarras'];
            ARetornoWS.DadosRet.IDBoleto.LinhaDig       := LJSonObject.AsString['linhaDigitavel'];
            ARetornoWS.DadosRet.IDBoleto.NossoNum       := LJSonObject.AsString['nossoNumero'];
            ARetornoWS.DadosRet.IDBoleto.URLPDF         := LJSonObject.AsString['pdfBoleto'];

            ARetornoWS.DadosRet.TituloRet.CodBarras      := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig       := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
            ARetornoWS.DadosRet.TituloRet.NossoNumero    := ARetornoWS.DadosRet.IDBoleto.NossoNum;

            ARetornoWS.DadosRet.TituloRet.Contrato       := LJSonObject.AsString['numeroContrato'];
            ARetornoWS.DadosRet.TituloRet.EMV            := LJSonObject.AsString['qrCode'];
            ARetornoWS.DadosRet.TituloRet.UrlPix         := LJSonObject.AsString['qrCode'];
            ARetornoWS.DadosRet.TituloRet.Vencimento     := DateBancoobToDateTime(LJSonObject.AsString['dataVencimento']);
            ARetornoWS.DadosRet.TituloRet.NossoNumero    := LJSonObject.AsString['nossoNumero'];
            ARetornoWS.DadosRet.TituloRet.EspecieDoc     := LJSonObject.AsString['especieDocumento'];
            ARetornoWS.DadosRet.TituloRet.DataDocumento  := DateBancoobToDateTime(LJSonObject.AsString['dataEmissao']);
            ARetornoWS.DadosRet.TituloRet.ValorDocumento := LJSonObject.AsCurrency['valor'];
            ARetornoWS.DadosRet.TituloRet.ValorDesconto  := LJSonObject.AsCurrency['valorPrimeiroDesconto'];
            ARetornoWS.DadosRet.TituloRet.NumeroDocumento:= LJSonObject.AsString['seuNumero'];
            ARetornoWS.DadosRet.TituloRet.SeuNumero      := trim(LJSonObject.AsString['identificacaoBoletoEmpresa']);
          end
          else if (TipoOperacao = tpConsultaDetalhe) then
          begin
            //LJSonObject := TACBrJSONObject.Create;
            LJSonObject :=  LJson.AsJSONObject['resultado'] ;

            ARetornoWS.DadosRet.IDBoleto.CodBarras       := LJSonObject.AsString['codigoBarras'];
            ARetornoWS.DadosRet.IDBoleto.LinhaDig        := LJSonObject.AsString['linhaDigitavel'];
            ARetornoWS.DadosRet.IDBoleto.NossoNum        := LJSonObject.AsString['nossoNumero'];
            ARetornoWS.indicadorContinuidade             := false;

            ARetornoWS.DadosRet.TituloRet.NossoNumero     := ARetornoWS.DadosRet.IDBoleto.NossoNum;
            ARetornoWS.DadosRet.TituloRet.Vencimento      := DateBancoobToDateTime(LJSonObject.AsString['dataVencimento']);
            ARetornoWS.DadosRet.TituloRet.ValorDocumento  := LJSonObject.AsCurrency['valor'];
            ARetornoWS.DadosRet.TituloRet.EspecieDoc      := LJSonObject.AsString['especieDocumento'];
            ARetornoWS.DadosRet.TituloRet.Contrato        := LJSonObject.AsString['numeroContrato'];

            case LJSonObject.AsInteger['tipoMulta'] of
             1 : begin // Multa Valor Fixo
              ARetornoWS.DadosRet.TituloRet.CodigoMulta      := cmValorFixo;
              ARetornoWS.DadosRet.TituloRet.PercentualMulta  := LJSonObject.AsCurrency['valorMulta'];
              ARetornoWS.DadosRet.TituloRet.MultaValorFixo   := True;
              ARetornoWS.DadosRet.TituloRet.DataMulta        := DateBancoobToDateTime(LJSonObject.AsString['dataMulta']);
             end;
             2 : begin // Multa com Valor em Percentual
              ARetornoWS.DadosRet.TituloRet.CodigoMulta      := cmPercentual;
              ARetornoWS.DadosRet.TituloRet.PercentualMulta  := LJSonObject.AsCurrency['valorMulta'];
              ARetornoWS.DadosRet.TituloRet.MultaValorFixo   := False;
              ARetornoWS.DadosRet.TituloRet.DataMulta        := DateBancoobToDateTime(LJSonObject.AsString['dataMulta']);
             end;
             3 : begin // Sem Multa
               ARetornoWS.DadosRet.TituloRet.CodigoMulta     := cmIsento;
               ARetornoWS.DadosRet.TituloRet.PercentualMulta := 0;
             end;
            end;

            ARetornoWS.DadosRet.TituloRet.ValorMulta := ARetornoWS.DadosRet.TituloRet.PercentualMulta;

            case LJSonObject.AsInteger['tipoJurosMora'] of
             1 : begin // Valor Dia
               ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros := cjValorDia;
               ARetornoWS.DadosRet.TituloRet.ValorMoraJuros  := LJSonObject.AsCurrency['valorJurosMora'];
               ARetornoWS.DadosRet.TituloRet.DataMoraJuros   := DateBancoobToDateTime(LJSonObject.AsString['dataJurosMora']);
             end;
             2 : begin // Taxa Mensal
               ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros := cjTaxaMensal;
               ARetornoWS.DadosRet.TituloRet.ValorMoraJuros  := LJSonObject.AsCurrency['valorJurosMora'];
               ARetornoWS.DadosRet.TituloRet.DataMoraJuros   := DateBancoobToDateTime(LJSonObject.AsString['dataJurosMora']);
             end;
             3 : begin // Isento
               ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros := cjIsento;
               ARetornoWS.DadosRet.TituloRet.ValorMoraJuros  := LJSonObject.AsCurrency['valorJurosMora'];
             end;
            end;

            ARetornoWS.DadosRet.TituloRet.CodBarras       := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig        := ARetornoWS.DadosRet.IDBoleto.LinhaDig;

            ARetornoWS.DadosRet.TituloRet.NumeroDocumento := LJSonObject.asString['seuNumero'];
            ARetornoWS.DadosRet.TituloRet.SeuNumero       := Trim(LJSonObject.asString['identificacaoBoletoEmpresa']);

            ARetornoWS.DadosRet.TituloRet.DataRegistro    := DateBancoobToDateTime( LJSonObject.asString['dataEmissao'] );
            ARetornoWS.DadosRet.TituloRet.Vencimento      := DateBancoobToDateTime( LJSonObject.asString['dataVencimento'] );

            ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca  := LJSonObject.asString['situacaoBoleto'];

            if UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca) = C_EMABERTO then
               ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '1';

            if (UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca) = C_BAIXADO) or
               (UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca) = C_CANCELADO) then
               ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '7';

            if UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca) = UpperCase(C_PAGO) then
               ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '6';


            ARetornoWS.DadosRet.TituloRet.UrlPix  := LJSonObject.AsString['qrCode'];
            ARetornoWS.DadosRet.TituloRet.EMV     := LJSonObject.AsString['qrCode'];

            //ARetornoWS.DadosRet.TituloRet.ValorAtual      := LJSonObject.Values['valor'].AsNumber;
            //ARetornoWS.DadosRet.TituloRet.ValorPago       := LJSonObject.Values['valorTotalRecebimento'].AsNumber;
            //ARetornoWS.DadosRet.TituloRet.DataMovimento         := DateBancoobToDateTime( LJSonObject.Values['dataHoraSituacao'].asString );
            //ARetornoWS.DadosRet.TituloRet.DataCredito           := DateBancoobToDateTime( LJSonObject.Values['dataHoraSituacao'].asString );

            if LJSonObject.IsJSONObject('pagador') then
            begin
              ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado     := LJSonObject.AsJSONObject['pagador'].AsString['nome'];
              ARetornoWS.DadosRet.TituloRet.Sacado.Cidade         := LJSonObject.AsJSONObject['pagador'].asString['cidade'];
              ARetornoWS.DadosRet.TituloRet.Sacado.UF             := LJSonObject.AsJSONObject['pagador'].asString['uf'];
              ARetornoWS.DadosRet.TituloRet.Sacado.Bairro         := LJSonObject.AsJSONObject['pagador'].asString['bairro'];
              ARetornoWS.DadosRet.TituloRet.Sacado.Cep            := LJSonObject.AsJSONObject['pagador'].asString['cep'];
              ARetornoWS.DadosRet.TituloRet.Sacado.Numero         := LJSonObject.AsJSONObject['pagador'].asString['numero'];
              ARetornoWS.DadosRet.TituloRet.Sacado.Logradouro     := LJSonObject.AsJSONObject['pagador'].asString['endereco'];
              ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF        := LJSonObject.AsJSONObject['pagador'].asString['numeroCpfCnpj'];
            end;

            if(LJSonObject.asString['situacaoBoleto'] = C_PAGO) then
            begin
             // WorkAround para pegar DataPagamento e Valor de Pagamento

             LJsonListaHistoricoArray :=  LJSonObject.AsJSONArray['listaHistorico'];

              for i := 0 to Pred(LJsonListaHistoricoArray.Count) do
              begin
                LJsonListaHistoricoObject := LJsonListaHistoricoArray.ItemAsJSONObject[i];

                if LJsonListaHistoricoObject.AsInteger['tipoHistorico'] = 6 then // 1 = Entrada, 4 = Tarifa Liquidação, 6 = Liquidação
                begin
                 ARetornoWS.DadosRet.TituloRet.DataBaixa := DateBancoobToDateTime(LJsonListaHistoricoObject.AsString['dataHistorico']);

                 vPos := Pos('R$', LJsonListaHistoricoObject.AsString['descricaoHistorico']);

                 ARetornoWS.DadosRet.TituloRet.ValorPago := 0;

                 if vpos > 0 then
                  ARetornoWS.DadosRet.TituloRet.ValorPago := StringToFloatDef(copy(LJsonListaHistoricoObject.AsString['descricaoHistorico'], vPos+2, length(LJsonListaHistoricoObject.AsString['descricaoHistorico'])), 0);
                end;
              end;
            end;

           { if(LJSonObject.Values['situacaoBoleto'].asString = C_CANCELADO) or (LJSonObject.Values['situacaoBoleto'].asString = C_EXPIRADO) or  (LJSonObject.Values['situacaoBoleto'].asString = C_PAGO) or (LJSonObject.Values['situacaoBoleto'].asString = C_EXPIRADO) then
            begin
              ARetornoWS.DadosRet.TituloRet.ValorPago                   := LJSonObject.Values['valorNominal'].AsNumber;
              ARetornoWS.DadosRet.TituloRet.DataBaixa                   := DateBancoobToDateTime( LJSonObject.Values['dataHoraSituacao'].asString )
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
        LJson.free;
      end;

    except
      Result := False;
    end;

  end;

end;

function TRetornoEnvio_Bancoob.LerListaRetorno: Boolean;
var
  ListaRetorno: TACBrBoletoRetornoWS;
  AJson, AJSonObject, AJsonViolacao: TACBrJSONObject;
  AJsonBoletosArray: TACBrJSONArray;
  LMensagemRejeicao: TACBrBoletoRejeicao;
  LSituacao : string;
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
      AJSon := TACBrJSONObject.Parse(RetWS);
      try

        if HTTPResultCode >= 400 then
        begin
          if (AJson.AsString['mensagems'] <> '') then
          begin
            LMensagemRejeicao     := ListaRetorno.CriarRejeicaoLista;
            LMensagemRejeicao.Codigo     := AJson.AsString['mensagem'];
            LMensagemRejeicao.mensagem   := AJson.AsString['codigo'];
          end;
          if AJson.isJSONArray('mensagens') then
          begin
            AJsonBoletosArray := ajson.AsJSONArray['mensagens'];
            for I := 0 to Pred(AJsonBoletosArray.Count) do
            begin
              aJsonViolacao                := AJsonBoletosArray.ItemAsJSONObject[I];
              LMensagemRejeicao            := ListaRetorno.CriarRejeicaoLista;
              LMensagemRejeicao.Codigo     := AJsonViolacao.AsString['codigo'];
              LMensagemRejeicao.mensagem   := AJsonViolacao.AsString['mensagem'];
            end;
          end;
        end;

        //retorna quando tiver sucesso
        if (ListaRetorno.ListaRejeicao.Count = 0) then
        begin
          aJsonString := aJson.ToJSON;
          //aJsonString := AJson.Values['content'].Stringify;
          AJsonBoletosArray := AJson.AsJSONArray['resultado'];


          for I := 0 to Pred(AJsonBoletosArray.Count) do
          begin
            if I > 0 then
              ListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;

            AJSonObject  := AJsonBoletosArray.ItemAsJSONObject[I];

            ListaRetorno.DadosRet.IDBoleto.CodBarras             := AJSonObject.AsString['codigoBarras'];
            ListaRetorno.DadosRet.IDBoleto.LinhaDig              := AJSonObject.AsString['linhaDigitavel'];
            ListaRetorno.DadosRet.IDBoleto.NossoNum              := AJSonObject.AsString['nossoNumero'];
            ListaRetorno.indicadorContinuidade                   := false;
            ListaRetorno.DadosRet.TituloRet.CodBarras            := ListaRetorno.DadosRet.IDBoleto.CodBarras;
            ListaRetorno.DadosRet.TituloRet.LinhaDig             := ListaRetorno.DadosRet.IDBoleto.LinhaDig;

            ListaRetorno.DadosRet.TituloRet.NossoNumero          := ListaRetorno.DadosRet.IDBoleto.NossoNum;
            ListaRetorno.DadosRet.TituloRet.Vencimento           := DateBancoobToDateTime(AJSonObject.AsString['dataVencimento']);

            ListaRetorno.DadosRet.TituloRet.ValorDocumento       := AJSonObject.AsCurrency['valorNominal'];
            ListaRetorno.DadosRet.TituloRet.ValorAtual           := AJSonObject.AsCurrency['valorNominal'];
            ListaRetorno.DadosRet.TituloRet.ValorPago            := AJSonObject.AsCurrency['valorTotalRecebimento'];
            ListaRetorno.DadosRet.TituloRet.ValorMoraJuros       := AJSonObject.AsJSONObject['mora'].AsCurrency['valor'];
            ListaRetorno.DadosRet.TituloRet.PercentualMulta      := AJSonObject.AsJSONObject['multa'].AsFloat['taxa'];

            ListaRetorno.DadosRet.TituloRet.NumeroDocumento      := AJSonObject.asString['seuNumero'];
            ListaRetorno.DadosRet.TituloRet.SeuNumero            := trim(AJSonObject.asString['identificacaoBoletoEmpresa']);
            ListaRetorno.DadosRet.TituloRet.DataRegistro         := DateBancoobToDateTime( AJSonObject.asString['dataEmissao'] );
            ListaRetorno.DadosRet.TituloRet.Vencimento           := DateBancoobToDateTime( AJSonObject.asString['dataVencimento'] );

            ListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca := AJSonObject.asString['situacao'];
            ListaRetorno.DadosRet.TituloRet.DataMovimento        := DateBancoobToDateTime( AJSonObject.asString['dataHoraSituacao']);
            ListaRetorno.DadosRet.TituloRet.DataCredito          := DateBancoobToDateTime( AJSonObject.asString['dataHoraSituacao']);

            ListaRetorno.DadosRet.TituloRet.Sacado.NomeSacado    := AJSonObject.AsJSONObject['pagador'].asString['nome'];
            ListaRetorno.DadosRet.TituloRet.Sacado.Cidade        := AJSonObject.AsJSONObject['pagador'].asString['cidade'];
            ListaRetorno.DadosRet.TituloRet.Sacado.UF            := AJSonObject.AsJSONObject['pagador'].asString['uf'];
            ListaRetorno.DadosRet.TituloRet.Sacado.Bairro        := AJSonObject.AsJSONObject['pagador'].asString['bairro'];
            ListaRetorno.DadosRet.TituloRet.Sacado.Cep           := AJSonObject.AsJSONObject['pagador'].asString['cep'];
            ListaRetorno.DadosRet.TituloRet.Sacado.Numero        := AJSonObject.AsJSONObject['pagador'].asString['numero'];
            ListaRetorno.DadosRet.TituloRet.Sacado.Logradouro    := AJSonObject.AsJSONObject['pagador'].asString['logradouro'];
            ListaRetorno.DadosRet.TituloRet.Sacado.CNPJCPF       := AJSonObject.AsJSONObject['pagador'].asString['cpfCnpj'];

            LSituacao := AJSonObject.asString['situacao'];

              if(  LSituacao = C_CANCELADO ) or
                 ( LSituacao = C_EXPIRADO ) or
                 ( LSituacao = C_PAGO ) or
                 ( LSituacao = C_EXPIRADO ) then
                 begin
                    ListaRetorno.DadosRet.TituloRet.ValorPago                   := AJSonObject.AsCurrency['valorNominal'];
                    ListaRetorno.DadosRet.TituloRet.DataBaixa                   := DateBancoobToDateTime( AJSonObject.asString['dataHoraSituacao'])
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

