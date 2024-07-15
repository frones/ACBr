{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Victor Hugo Gonzales - Panda                   }
{                               Leandro do Couto                               }
{                               Delmar de Lima                                 }
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

unit ACBrBoletoRet_Inter_API;

interface

uses
  Classes, SysUtils, ACBrBoleto,ACBrBoletoWS, ACBrBoletoRetorno,
  DateUtils, pcnConversao,ACBrBoletoWS.Rest, ACBrJSON, ACBrUtil.Base;

type

{ TRetornoEnvio_Inter_API }

 TRetornoEnvio_Inter_API = class(TRetornoEnvioREST)
 private
   function  DateInterToDateTime(Const AValue : String) : TDateTime;
   procedure LerListaRetornoPix;
   procedure LerRetornoPix(const ARetornoWS: TACBrBoletoRetornoWS; AIndex: Integer);
 public
   constructor Create(ABoletoWS: TACBrBoleto); override;
   destructor  Destroy; Override;

   //function LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean; override; //2024-01-08 - TK5016 - 00215-23 - retirado

   function LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS; AIndex: Integer = 0): Boolean; reintroduce; //2024-01-08 - TK5016 - 00215-23
   function LerListaRetorno: Boolean; override;
   function RetornoEnvio(const AIndex: Integer): Boolean; override;

 end;

implementation

uses
  ACBrBoletoConversao;

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

constructor TRetornoEnvio_Inter_API.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);
end;

function TRetornoEnvio_Inter_API.DateInterToDateTime(
  const AValue: String): TDateTime;
var
  LData,
  LAno,
  LMes,
  LDia:String;
begin
  LAno := Copy( aValue, 0,4 );
  LMes := Copy( aValue, 6,2 );
  LDia := Copy( aValue, 9,2 );
  LData := Format( '%s/%s/%s' , [LDia,LMes,LAno]);
  Result := StrToDateDef( LData ,0 );
end;

destructor TRetornoEnvio_Inter_API.Destroy;
begin
  inherited Destroy;
end;

function TRetornoEnvio_Inter_API.LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS; AIndex: Integer = 0): Boolean;
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
    LJsonObject := TACBrJSONObject.Parse(RetWS);
    try
      try
        ARetornoWS.JSON := LJsonObject.ToJSON;

        if HttpResultCode >= 400 then
        begin
          if NaoEstaVazio(LJsonObject.asString['codigo']) then
          begin
            LRejeicaoMensagem            := ARetornoWS.CriarRejeicaoLista;
            LRejeicaoMensagem.Codigo     := LJsonObject.AsString['codigo'];
            LRejeicaoMensagem.Versao     := LJsonObject.AsString['parametro'];
            LRejeicaoMensagem.Mensagem   := LJsonObject.AsString['mensagem'];
          end;
          if LJsonObject.IsJSONArray('violacoes') then
          begin
            LJsonArray := LJsonObject.AsJSONArray['violacoes'];
            for X := 0 to LJsonArray.Count -1 do
            begin
              LRejeicaoMensagem            := ARetornoWS.CriarRejeicaoLista;
              LRejeicaoMensagem.Codigo     := LJsonArray.ItemAsJSONObject[X].AsString['codigo'];
              LRejeicaoMensagem.Versao     := LJsonArray.ItemAsJSONObject[X].AsString['parametro'];
              LRejeicaoMensagem.Mensagem   := LJsonArray.ItemAsJSONObject[X].AsString['razao']
                                                  + ' de '
                                                  + LJsonArray.ItemAsJSONObject[X].AsString['propriedade']
                                                  +' Valor :'
                                                  + LJsonArray.ItemAsJSONObject[X].AsString['valor'];
            end;
          end
          else
          begin
            if NaoEstaVazio(LJsonObject.asString['title']) or
               NaoEstaVazio(LJsonObject.asString['detail']) then
            begin
              LRejeicaoMensagem            := ARetornoWS.CriarRejeicaoLista;
              LRejeicaoMensagem.Codigo     := LJsonObject.AsString['title'];
              LRejeicaoMensagem.Versao     := ''; // LJsonObject.AsString['parametro'];
              LRejeicaoMensagem.Mensagem   := LJsonObject.AsString['detail'] +
                                               ' Data e Hora: '+ LJsonObject.AsString['timestamp'];
            end;
          end;
        end;

        //retorna quando tiver sucesso
        if (ARetornoWS.ListaRejeicao.Count = 0) then
        begin
          if ACBrBoleto.Cedente.CedenteWS.IndicadorPix then
            LerRetornoPix(ARetornoWS, AIndex)
          else

          if (LTipoOperacao = tpInclui) then
          begin

            ARetornoWS.DadosRet.IDBoleto.CodBarras      := LJsonObject.AsString['codigoBarras'];
            ARetornoWS.DadosRet.IDBoleto.LinhaDig       := LJsonObject.AsString['linhaDigitavel'];
            ARetornoWS.DadosRet.IDBoleto.NossoNum       := LJsonObject.AsString['nossoNumero'];

            ARetornoWS.DadosRet.TituloRet.CodBarras     := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig      := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
            ARetornoWS.DadosRet.TituloRet.NossoNumero   := ARetornoWS.DadosRet.IDBoleto.NossoNum;

            ARetornoWS.DadosRet.TituloRet.SeuNumero       := LJsonObject.AsString['seuNumero'];

          end else
          if (LTipoOperacao = tpConsultaDetalhe) then
          begin
            AJSonObjectItem := TACBrJSONObject.Create;

            ARetornoWS.DadosRet.IDBoleto.CodBarras       := LJsonObject.AsString['codigoBarras'];
            ARetornoWS.DadosRet.IDBoleto.LinhaDig        := LJsonObject.AsString['linhaDigitavel'];
            ARetornoWS.DadosRet.IDBoleto.NossoNum        := LJsonObject.AsString['nossoNumero'];
            ARetornoWS.indicadorContinuidade             := false;

            ARetornoWS.DadosRet.TituloRet.NossoNumero     := ARetornoWS.DadosRet.IDBoleto.NossoNum;
            ARetornoWS.DadosRet.TituloRet.Vencimento      := DateInterToDateTime(LJsonObject.AsString['dataVencimento']);
            ARetornoWS.DadosRet.TituloRet.ValorDocumento  := LJsonObject.AsCurrency['valorNominal'];
            ARetornoWS.DadosRet.TituloRet.ValorAtual      := LJsonObject.AsCurrency['valorNominal'];


            if LJsonObject.ValueExists('valorTotalRecebimento') then // V2
              begin
              ARetornoWS.DadosRet.TituloRet.ValorPago       := LJsonObject.AsCurrency['valorTotalRecebimento'];
              ARetornoWS.DadosRet.TituloRet.ValorRecebido   := LJsonObject.AsCurrency['valorTotalRecebimento'];
              end
            else
              begin // v3
              ARetornoWS.DadosRet.TituloRet.ValorPago       := LJsonObject.AsCurrency['valorTotalRecebido'];
              ARetornoWS.DadosRet.TituloRet.ValorRecebido   := LJsonObject.AsCurrency['valorTotalRecebido'];
              end;

            ARetornoWS.DadosRet.TituloRet.CodBarras       := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig        := ARetornoWS.DadosRet.IDBoleto.LinhaDig;

            ARetornoWS.DadosRet.TituloRet.SeuNumero       := LJsonObject.asString['seuNumero'];
            ARetornoWS.DadosRet.TituloRet.DataRegistro    := DateIntertoDateTime( LJsonObject.asString['dataEmissao'] );
            ARetornoWS.DadosRet.TituloRet.Vencimento      := DateIntertoDateTime( LJsonObject.asString['dataVencimento'] );

            ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca  := LJsonObject.asString['situacao'];
            ARetornoWS.DadosRet.TituloRet.DataMovimento         := DateIntertoDateTime( LJsonObject.asString['dataHoraSituacao'] );
            ARetornoWS.DadosRet.TituloRet.DataCredito           := DateIntertoDateTime( LJsonObject.asString['dataHoraSituacao'] );

            LSituacao := AnsiUpperCase(LJsonObject.asString['situacao']);

            if AJSonObjectItem.IsJSONObject('dataHoraSituacao') then
             ARetornoWS.DadosRet.TituloRet.DataBaixa  := DateIntertoDateTime( AJSonObjectItem.asString['dataHoraSituacao'] );

            {Mora}
            if LJsonObject.IsJSONObject('mora') then
            begin
              AJSonObjectItem := LJsonObject.AsJSONObject['mora'];
              if AJSonObjectItem.AsString['codigo'] = 'TAXAMENSAL' then
              begin
                ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros := cjTaxaMensal;
                ARetornoWS.DadosRet.TituloRet.ValorMoraJuros  := AJSonObjectItem.AsInteger['taxa'];
                ARetornoWS.DadosRet.TituloRet.DataMoraJuros   := DateInterToDateTime(AJSonObjectItem.AsString['data']);
              end
              else if AJSonObjectItem.AsString['codigo']= 'VALORDIA'then
              begin
                ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros :=  cjValorDia;
                ARetornoWS.DadosRet.TituloRet.ValorMoraJuros  := AJSonObjectItem.AsCurrency['valor'];
                ARetornoWS.DadosRet.TituloRet.DataMoraJuros   := DateInterToDateTime(AJSonObjectItem.AsString['data']);
              end
              else
              begin
                ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros :=  cjIsento;
                ARetornoWS.DadosRet.TituloRet.ValorMoraJuros   := 0;
              end;

            end;

            {Multa}
            if LJsonObject.IsJSONObject('multa') then
            begin
              AJSonObjectItem := LJsonObject.AsJSONObject['multa'];
              if AJSonObjectItem.AsString['codigo'] = 'PERCENTUAL' then
              begin
                ARetornoWS.DadosRet.TituloRet.PercentualMulta       := AJSonObjectItem.AsFloat['taxa'];
                ARetornoWS.DadosRet.TituloRet.MultaValorFixo        := False;
                ARetornoWS.DadosRet.TituloRet.DataMulta             := DateInterToDateTime(AJSonObjectItem.AsString['data']);
              end
              else if AJSonObjectItem.AsString['codigo'] = 'VALORFIXO' then
              begin
                ARetornoWS.DadosRet.TituloRet.PercentualMulta       := AJSonObjectItem.AsCurrency['valor'];
                ARetornoWS.DadosRet.TituloRet.MultaValorFixo        := True;
                ARetornoWS.DadosRet.TituloRet.ValorMulta            := ARetornoWS.DadosRet.TituloRet.PercentualMulta;
                ARetornoWS.DadosRet.TituloRet.DataMulta             := DateInterToDateTime(AJSonObjectItem.AsString['data']);
              end
              else
              begin
                ARetornoWS.DadosRet.TituloRet.PercentualMulta       := 0;
                ARetornoWS.DadosRet.TituloRet.ValorMulta := 0;
                ARetornoWS.DadosRet.TituloRet.MultaValorFixo        := False;
              end;

            end;

            {Desconto1}
            if LJsonObject.IsJSONObject('desconto1') then
            begin
              AJSonObjectItem := LJsonObject.AsJSONObject['desconto1'];
              if AJSonObjectItem.AsString['codigo'] = 'VALORFIXODATAINFORMADA' then
              begin
                ARetornoWS.DadosRet.TituloRet.ValorDesconto  := AJSonObjectItem.AsCurrency['valor'];
                ARetornoWS.DadosRet.TituloRet.CodigoDesconto := cdValorFixo;
                ARetornoWS.DadosRet.TituloRet.DataDesconto   := DateInterToDateTime(AJSonObjectItem.AsString['data']);
              end
              else if AJSonObjectItem.AsString['codigo'] = 'PERCENTUALDATAINFORMADA' then
              begin
                ARetornoWS.DadosRet.TituloRet.ValorDesconto := AJSonObjectItem.AsFloat['taxa'];
                ARetornoWS.DadosRet.TituloRet.CodigoDesconto := cdPercentual;
                ARetornoWS.DadosRet.TituloRet.DataDesconto   := DateInterToDateTime(AJSonObjectItem.AsString['data']);
              end
              else
              begin
                ARetornoWS.DadosRet.TituloRet.ValorDesconto := 0;
                ARetornoWS.DadosRet.TituloRet.CodigoDesconto := cdSemDesconto;
              end;

            end;
            {Desconto1}
            if LJsonObject.IsJSONObject('pagador') then
            begin
              AJSonObjectItem := LJsonObject.AsJSONObject['pagador'];
              ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado     := AJSonObjectItem.asString['nome'];
              ARetornoWS.DadosRet.TituloRet.Sacado.Cidade         := AJSonObjectItem.asString['cidade'];
              ARetornoWS.DadosRet.TituloRet.Sacado.UF             := AJSonObjectItem.asString['uf'];
              ARetornoWS.DadosRet.TituloRet.Sacado.Bairro         := AJSonObjectItem.asString['bairro'];
              ARetornoWS.DadosRet.TituloRet.Sacado.Cep            := AJSonObjectItem.asString['cep'];
              ARetornoWS.DadosRet.TituloRet.Sacado.Numero         := AJSonObjectItem.asString['numero'];
              ARetornoWS.DadosRet.TituloRet.Sacado.Logradouro     := AJSonObjectItem.asString['logradouro'];
              ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF        := AJSonObjectItem.asString['cpfCnpj'];
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

procedure TRetornoEnvio_Inter_API.LerRetornoPix(const ARetornoWS: TACBrBoletoRetornoWS; AIndex: Integer);
var
  LJsonViolacao, LJsonObject, LJsonObjectItem : TACBrJSONObject;
  LMensagemRejeicao : TACBrBoletoRejeicao;
  LJsonViolacoes: TACBrJSONArray;
  LSituacao : string;
  X: Integer;
begin
  ARetornoWS.HTTPResultCode := HTTPResultCode;
  ARetornoWS.JSONEnvio := EnvWs;
  ARetornoWS.Header.Operacao := ACBrBoleto.Configuracoes.WebService.Operacao;

  if RetWS <> '' then
  begin
    LJsonObject := TACBrJSONObject.Parse(RetWS);
    try
      ARetornoWS.JSON := LJsonObject.ToJSON;

      case HttpResultCode of
        400, 415:
          begin
            LJsonViolacoes := LJsonObject.AsJSONArray['violacoes'];
            for x := 0 to LJsonViolacoes.Count -1 do
            begin
              LJsonViolacao     := LJsonViolacoes.ItemAsJSONObject[x];
              LMensagemRejeicao         := ARetornoWS.CriarRejeicaoLista;
              LMensagemRejeicao.Codigo  := LJsonViolacao.AsString['codigo'];
              LMensagemRejeicao.Versao  := LJsonViolacao.AsString['parametro'];
              LMensagemRejeicao.Mensagem := LJsonViolacao.AsString['razao'] + ' de ' +
              LJsonViolacao.AsString['propriedade'] + ' Valor :' + LJsonViolacao.AsString['valor'];
            end;
          end;
        404:
          begin
            if NaoEstaVazio(LJsonObject.asString['codigo']) then
            begin
              LMensagemRejeicao := ARetornoWS.CriarRejeicaoLista;
              LMensagemRejeicao.Codigo    := LJsonObject.AsString['codigo'];
              LMensagemRejeicao.Versao    := LJsonObject.AsString['parametro'];
              LMensagemRejeicao.Mensagem  := LJsonObject.AsString['mensagem'];
            end;
          end;
      end;

      //retorna quando tiver sucesso
      if (ARetornoWS.ListaRejeicao.Count = 0) then
      begin
        LJsonObjectItem := TACBrJSONObject.Parse(RetWS);
        try
          if ((ARetornoWS.Header.Operacao = tpInclui) and (NaoEstaVazio(LJsonObjectItem.AsString['codigoSolicitacao']))) then
          begin
             ACBrBoleto.ListadeBoletos[AIndex].NossoNumeroCorrespondente := LJsonObjectItem.AsString['codigoSolicitacao'];
             ARetornoWS.DadosRet.IDBoleto.IDBoleto   := LJsonObjectItem.AsString['codigoSolicitacao'];
          end
          else if (ARetornoWS.Header.Operacao = tpConsultaDetalhe) or (ARetornoWS.Header.Operacao = tpInclui) then
          begin
            if LJsonObjectItem.IsJSONObject('boleto') then
            begin
              ARetornoWS.DadosRet.IDBoleto.CodBarras  := LJsonObjectItem.AsJSONObject['boleto'].AsString['codigoBarras'];
              ARetornoWS.DadosRet.IDBoleto.LinhaDig   := LJsonObjectItem.AsJSONObject['boleto'].AsString['linhaDigitavel'];
              ARetornoWS.DadosRet.IDBoleto.NossoNum   := LJsonObjectItem.AsJSONObject['boleto'].AsString['nossoNumero'];
            end;
            ARetornoWS.indicadorContinuidade        := False;

            if NaoEstaVazio(ARetornoWS.DadosRet.IDBoleto.IDBoleto) then
              ARetornoWS.DadosRet.TituloRet.NossoNumeroCorrespondente := ARetornoWS.DadosRet.IDBoleto.IDBoleto;

            if LJsonObjectItem.IsJSONObject('pix') then
            begin
              ARetornoWS.DadosRet.TituloRet.TxId          := LJsonObjectItem.AsJSONObject['pix'].AsString['txid'];
              ARetornoWS.DadosRet.TituloRet.EMV           := LJsonObjectItem.AsJSONObject['pix'].AsString['pixCopiaECola'];
            end;
            ARetornoWS.DadosRet.TituloRet.NossoNumero   := ARetornoWS.DadosRet.IDBoleto.NossoNum;

            if LJsonObjectItem.IsJSONObject('cobranca') then
            begin
              ARetornoWS.DadosRet.TituloRet.NossoNumeroCorrespondente := LJsonObjectItem.AsJSONObject['cobranca'].AsString['codigoSolicitacao'];
              ARetornoWS.DadosRet.TituloRet.SeuNumero     := LJsonObjectItem.AsJSONObject['cobranca'].AsString['seuNumero'];
              ARetornoWS.DadosRet.TituloRet.Vencimento    := DateInterToDateTime(LJsonObjectItem.AsJSONObject['cobranca'].AsString['dataVencimento']);
              ARetornoWS.DadosRet.TituloRet.DataDocumento := DateInterToDateTime(LJsonObjectItem.AsJSONObject['cobranca'].AsString['dataEmissao']);
              ARetornoWS.DadosRet.TituloRet.ValorDocumento:= LJsonObjectItem.AsJSONObject['cobranca'].AsCurrency['valorNominal'];
              ARetornoWS.DadosRet.TituloRet.ValorAtual    := LJsonObjectItem.AsJSONObject['cobranca'].AsCurrency['valorNominal'];
              ARetornoWS.DadosRet.TituloRet.ValorPago     := LJsonObjectItem.AsJSONObject['cobranca'].AsCurrency['valorTotalRecebido'];
              ARetornoWS.DadosRet.TituloRet.ValorRecebido := LJsonObjectItem.AsJSONObject['cobranca'].AsCurrency['valorTotalRecebido'];
            end;

            if LJsonObjectItem.IsJSONObject('mora') then
              if LJsonObjectItem.AsJSONObject['mora'].AsString['codigo'] = 'TAXAMENSAL' then
              begin
                ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros := cjTaxaMensal;
                ARetornoWS.DadosRet.TituloRet.ValorMoraJuros  := LJsonObjectItem.AsJSONObject['mora'].AsFloat['taxa'];
              end
              else if LJsonObjectItem.AsJSONObject['mora'].AsString['codigo'] = 'VALORDIA'then
              begin
                ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros := cjValorDia;
                ARetornoWS.DadosRet.TituloRet.ValorMoraJuros := LJsonObjectItem.AsJSONObject['mora'].AsCurrency['valor'];
              end else
              begin
                ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros :=  cjIsento;
                ARetornoWS.DadosRet.TituloRet.ValorMoraJuros := 0;
              end;

            if LJsonObjectItem.IsJSONObject('multa') then
              if LJsonObjectItem.AsJSONObject['multa'].AsString['codigo'] = 'PERCENTUAL' then
              begin
                ARetornoWS.DadosRet.TituloRet.PercentualMulta := LJsonObjectItem.AsJSONObject['multa'].AsCurrency['valor'];
                ARetornoWS.DadosRet.TituloRet.MultaValorFixo := False;
              end
              else if LJsonObjectItem.AsJSONObject['multa'].AsString['codigo'] = 'VALORFIXO' then
              begin
                ARetornoWS.DadosRet.TituloRet.PercentualMulta := LJsonObjectItem.AsJSONObject['multa'].AsCurrency['valor'];
                ARetornoWS.DadosRet.TituloRet.MultaValorFixo := True;
                ARetornoWS.DadosRet.TituloRet.ValorMulta := ARetornoWS.DadosRet.TituloRet.PercentualMulta;
              end
              else
              begin
                ARetornoWS.DadosRet.TituloRet.PercentualMulta := 0;
                ARetornoWS.DadosRet.TituloRet.ValorMulta := 0;
                ARetornoWS.DadosRet.TituloRet.MultaValorFixo := False;
              end;

            if LJsonObjectItem.AsJSONObject['descontos'] <> nil then
              if LJsonObjectItem.AsJSONObject['descontos'].AsString['codigo']= 'VALORFIXODATAINFORMADA' then
              begin
                ARetornoWS.DadosRet.TituloRet.ValorDesconto := LJsonObjectItem.AsJSONObject['descontos'].AsFloat['taxa'];
                ARetornoWS.DadosRet.TituloRet.CodigoDesconto := cdValorFixo;
              end
              else if LJsonObjectItem.AsJSONObject['descontos'].AsString['codigo'] = 'PERCENTUALDATAINFORMADA' then
              begin
                ARetornoWS.DadosRet.TituloRet.ValorDesconto := LJsonObjectItem.AsJSONObject['descontos'].AsFloat['taxa'];
                ARetornoWS.DadosRet.TituloRet.CodigoDesconto := cdPercentual;
              end
              else
              begin
                ARetornoWS.DadosRet.TituloRet.ValorDesconto := 0;
                ARetornoWS.DadosRet.TituloRet.CodigoDesconto := cdSemDesconto;
              end;

            ARetornoWS.DadosRet.TituloRet.CodBarras := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig := ARetornoWS.DadosRet.IDBoleto.LinhaDig;

            if LJsonObjectItem.IsJSONObject('cobranca') then
            begin
              ARetornoWS.DadosRet.TituloRet.SeuNumero     := LJsonObjectItem.AsJSONObject['cobranca'].AsString['seuNumero'];
              ARetornoWS.DadosRet.TituloRet.DataRegistro  := DateIntertoDateTime(LJsonObjectItem.AsJSONObject['cobranca'].AsString['dataEmissao']);
              ARetornoWS.DadosRet.TituloRet.Vencimento    := DateIntertoDateTime(LJsonObjectItem.AsJSONObject['cobranca'].AsString['dataVencimento']);
              ARetornoWS.DadosRet.TituloRet.DataDocumento := DateIntertoDateTime(LJsonObjectItem.AsJSONObject['cobranca'].AsString['dataEmissao']);

              ARetornoWS.DadosRet.TituloRet.DataMovimento         := DateIntertoDateTime(LJsonObjectItem.AsJSONObject['cobranca'].AsString['dataSituacao']);
              ARetornoWS.DadosRet.TituloRet.DataCredito           := DateIntertoDateTime(LJsonObjectItem.AsJSONObject['cobranca'].AsString['dataSituacao']);

              if LJsonObjectItem.AsJSONObject['cobranca'].isJSONObject('pagador') then
              begin
                ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado := LJsonObjectItem.AsJSONObject['cobranca'].AsJSONObject['pagador'].AsString['nome'];
                ARetornoWS.DadosRet.TituloRet.Sacado.Cidade     := LJsonObjectItem.AsJSONObject['cobranca'].AsJSONObject['pagador'].AsString['cidade'];
                ARetornoWS.DadosRet.TituloRet.Sacado.UF         := LJsonObjectItem.AsJSONObject['cobranca'].AsJSONObject['pagador'].AsString['uf'];
                ARetornoWS.DadosRet.TituloRet.Sacado.Bairro     := LJsonObjectItem.AsJSONObject['cobranca'].AsJSONObject['pagador'].AsString['bairro'];
                ARetornoWS.DadosRet.TituloRet.Sacado.Cep        := LJsonObjectItem.AsJSONObject['cobranca'].AsJSONObject['pagador'].AsString['cep'];
                ARetornoWS.DadosRet.TituloRet.Sacado.Numero     := LJsonObjectItem.AsJSONObject['cobranca'].AsJSONObject['pagador'].asString['numero'];
                ARetornoWS.DadosRet.TituloRet.Sacado.Logradouro := LJsonObjectItem.AsJSONObject['cobranca'].AsJSONObject['pagador'].AsString['endereco'];
                ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF    := LJsonObjectItem.AsJSONObject['cobranca'].AsJSONObject['pagador'].AsString['cpfCnpj'];
              end;
              if NaoEstaVazio(LJsonObjectItem.AsJSONObject['cobranca'].AsString['situacao']) then
                 LSituacao :=  LJsonObjectItem.AsJSONObject['cobranca'].AsString['situacao'];

              ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca  := LSituacao;

              if (LSituacao = C_CANCELADO) or (LSituacao = C_EXPIRADO) or
                (LSituacao = C_PAGO) or (LSituacao = C_EXPIRADO) or
                (LSituacao = C_MARCADO_RECEBIDO) or (LSituacao = C_RECEBIDO) then
              begin
                ARetornoWS.DadosRet.TituloRet.DataBaixa := DateIntertoDateTime(LJsonObjectItem.AsJSONObject['cobranca'].AsString['dataSituacao']);
              end;
            end;
          end;
        finally
          LJsonObjectItem.Free;
        end;


      end;
    finally
      LJsonObject.Free;
    end;
  end;
end;

function TRetornoEnvio_Inter_API.LerListaRetorno: Boolean;
var
  LListaRetorno: TACBrBoletoRetornoWS;
  LJsonObject, LJsonObjectItem: TACBrJSONObject;
  LMensagemRejeicao: TACBrBoletoRejeicao;
  LJsonArray: TACBrJSONArray;
  I, X: Integer;
  LSituacao : AnsiString;
begin
  Result := True;

  if RetWS <> '' then
  begin
    if ACBrBoleto.Cedente.CedenteWS.IndicadorPix then
    begin
      LerListaRetornoPix;
      Exit;
    end;


  LListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;
  LListaRetorno.HTTPResultCode := HTTPResultCode;
  LListaRetorno.JSONEnvio      := EnvWs;

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
            LListaRetorno.DadosRet.TituloRet.Vencimento           := DateInterToDateTime(LJsonObjectItem.AsString['dataVencimento']);

            LListaRetorno.DadosRet.TituloRet.ValorDocumento       := LJsonObjectItem.AsCurrency['valorNominal'];
            LListaRetorno.DadosRet.TituloRet.ValorAtual           := LJsonObjectItem.AsCurrency['valorNominal'];
            LListaRetorno.DadosRet.TituloRet.ValorPago            := LJsonObjectItem.AsCurrency['valorTotalRecebimento'];

            if LJsonObjectItem.AsJSONObject['mora'].AsString['codigo'] = 'TAXAMENSAL' then
            begin
              LListaRetorno.DadosRet.TituloRet.CodigoMoraJuros :=  cjTaxaMensal;
              LListaRetorno.DadosRet.TituloRet.ValorMoraJuros  := LJsonObjectItem.AsJSONObject['mora'].AsCurrency['taxa'];
              LListaRetorno.DadosRet.TituloRet.DataMoraJuros   := DateInterToDateTime(LJsonObjectItem.AsJSONObject['mora'].AsString['data']);
            end
            else if LJsonObjectItem.AsJSONObject['mora'].AsString['codigo'] = 'VALORDIA'then
            begin
              LListaRetorno.DadosRet.TituloRet.CodigoMoraJuros :=  cjValorDia;
              LListaRetorno.DadosRet.TituloRet.ValorMoraJuros  := LJsonObjectItem.AsJSONObject['mora'].AsCurrency['valor'];
              LListaRetorno.DadosRet.TituloRet.DataMoraJuros   := DateInterToDateTime(LJsonObjectItem.AsJSONObject['mora'].AsString['data']);
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
              LListaRetorno.DadosRet.TituloRet.DataMulta             := DateInterToDateTime(LJsonObjectItem.AsJSONObject['multa'].AsString['data']);
            end
            else if LJsonObjectItem.AsJSONObject['multa'].AsString['codigo'] = 'VALORFIXO' then
            begin
              LListaRetorno.DadosRet.TituloRet.PercentualMulta       := LJsonObjectItem.AsJSONObject['multa'].AsFloat['valor'];
              LListaRetorno.DadosRet.TituloRet.MultaValorFixo        := True;
              LListaRetorno.DadosRet.TituloRet.ValorMulta            := LListaRetorno.DadosRet.TituloRet.PercentualMulta;
              LListaRetorno.DadosRet.TituloRet.DataMulta             := DateInterToDateTime(LJsonObjectItem.AsJSONObject['multa'].AsString['data']);
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
              LListaRetorno.DadosRet.TituloRet.DataDesconto   := DateInterToDateTime(LJsonObjectItem.AsJSONObject['desconto1'].AsString['data']);
            end
            else if LJsonObjectItem.AsJSONObject['desconto1'].AsString['codigo'] = 'PERCENTUALDATAINFORMADA' then
            begin
              LListaRetorno.DadosRet.TituloRet.ValorDesconto := LJsonObjectItem.AsJSONObject['desconto1'].AsFloat['taxa'];
              LListaRetorno.DadosRet.TituloRet.CodigoDesconto := cdPercentual;
              LListaRetorno.DadosRet.TituloRet.DataDesconto   := DateInterToDateTime(LJsonObjectItem.AsJSONObject['desconto1'].AsString['data']);
            end
            else
            begin
              LListaRetorno.DadosRet.TituloRet.ValorDesconto := 0;
              LListaRetorno.DadosRet.TituloRet.CodigoDesconto := cdSemDesconto;
            end;

            
            LListaRetorno.DadosRet.TituloRet.SeuNumero            := LJsonObjectItem.asString['seuNumero'];
            LListaRetorno.DadosRet.TituloRet.DataRegistro         := DateIntertoDateTime( LJsonObjectItem.asString['dataEmissao'] );
            LListaRetorno.DadosRet.TituloRet.Vencimento           := DateIntertoDateTime( LJsonObjectItem.asString['dataVencimento'] );

            LListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca := LJsonObjectItem.asString['situacao'];
            LListaRetorno.DadosRet.TituloRet.DataMovimento        := DateIntertoDateTime( LJsonObjectItem.asString['dataHoraSituacao'] );
            LListaRetorno.DadosRet.TituloRet.DataCredito          := DateIntertoDateTime( LJsonObjectItem.asString['dataHoraSituacao'] );

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
                LListaRetorno.DadosRet.TituloRet.DataBaixa    := DateIntertoDateTime( LJsonObjectItem.asString['dataHoraSituacao'] )
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

function TRetornoEnvio_Inter_API.RetornoEnvio(const AIndex: Integer): Boolean;
begin
//  Result:=inherited RetornoEnvio(AIndex); // Pelo fato do reintroduce, nao chama a função base, uma vez que tirou o override da declaração
  if (ACBrBoleto.ListadeBoletos.Count > 0) and (ACBrBoleto.Configuracoes.WebService.Operacao <> tpConsulta) then
  begin
    Result := LerRetorno(ACBrBoleto.ListadeBoletos[ AIndex ].RetornoWeb, AIndex);
    ACBrBoleto.ListadeBoletos[ AIndex ].QrCode; //GetQRCode valida campos no titulo
  end
  else
    Result := LerListaRetorno;
end;

procedure TRetornoEnvio_Inter_API.LerListaRetornoPix;
var
  LListaRetorno: TACBrBoletoRetornoWS;
  LJsonObject, LJsonItemObject: TACBrJSONObject;
  LMensagemRejeicao: TACBrBoletoRejeicao;
  LJsonArray: TACBrJSONArray;
  I, X: Integer;
  LSituacao, LSituacaoBoleto : AnsiString;
begin
  LListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;
  LListaRetorno.HTTPResultCode := HTTPResultCode;
  LListaRetorno.JSONEnvio      := EnvWs;
  if RetWS <> '' then
  begin
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
        if not(LJsonObject.AsBoolean['ultimaPagina']) then
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

        LJsonArray := LJsonObject.AsJSONArray['cobrancas'];
        for I := 0 to Pred(LJsonArray.Count) do
        begin
          if I > 0 then
            LListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;

          {Cobrança}
          if LJsonArray.ItemAsJSONObject[I].IsJSONObject('cobranca') then
          begin
            LJsonItemObject := LJsonArray.ItemAsJSONObject[I].AsJSONObject['cobranca'];

            LListaRetorno.DadosRet.TituloRet.NossoNumeroCorrespondente := LJsonItemObject.AsString['codigoSolicitacao'];
            LListaRetorno.DadosRet.TituloRet.ValorDocumento        := LJsonItemObject.AsCurrency['valorNominal'];
            LListaRetorno.DadosRet.TituloRet.ValorAtual            := LJsonItemObject.AsCurrency['valorNominal'];
            LListaRetorno.DadosRet.TituloRet.ValorPago             := LJsonItemObject.AsCurrency['valorTotalRecebido'];
            LListaRetorno.DadosRet.TituloRet.Vencimento            := DateInterToDateTime(LJsonItemObject.AsString['dataVencimento']);
            LListaRetorno.DadosRet.TituloRet.SeuNumero             := LJsonItemObject.AsString['seuNumero'];
            LListaRetorno.DadosRet.TituloRet.DataRegistro          := DateIntertoDateTime(LJsonItemObject.AsString['dataEmissao']);
            LListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca  := LJsonItemObject.AsString['situacao'];
            LListaRetorno.DadosRet.TituloRet.DataMovimento         := DateIntertoDateTime(LJsonItemObject.AsString['dataSituacao']);
            LListaRetorno.DadosRet.TituloRet.DataCredito           := DateIntertoDateTime(LJsonItemObject.AsString['dataSituacao']);
            LSituacaoBoleto := LJsonItemObject.AsString['situacao'];
            LListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca := LSituacaoBoleto;
            LListaRetorno.DadosRet.TituloRet.DataMovimento        := DateIntertoDateTime(LJsonItemObject.AsString['dataSituacao']);
            if (LSituacaoBoleto = C_CANCELADO) or (LSituacaoBoleto = C_EXPIRADO) or
               (LSituacaoBoleto= C_PAGO) or (LSituacaoBoleto = C_EXPIRADO) or (LSituacaoBoleto = C_RECEBIDO) then
            begin
              if (LSituacaoBoleto = C_PAGO) or (LSituacaoBoleto = C_RECEBIDO) then
              begin
                LListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := LJsonItemObject.AsString['origemRecebimento'];
                LListaRetorno.DadosRet.TituloRet.ValorPago := LJsonItemObject.AsCurrency['valorTotalRecebido'];
                LListaRetorno.DadosRet.TituloRet.ValorRecebido := LJsonItemObject.AsCurrency['valorTotalRecebido'];
              end;
              LListaRetorno.DadosRet.TituloRet.DataBaixa := DateIntertoDateTime(LJsonItemObject.AsString['dataSituacao']);
            end;
            {Cobranca - Pagador}
            if LJsonItemObject.IsJSONObject('pagador') then
              begin
                LJsonItemObject := LJsonItemObject.AsJSONObject['pagador'];
                LListaRetorno.DadosRet.TituloRet.Sacado.NomeSacado   := LJsonItemObject.AsString['nome'];
                LListaRetorno.DadosRet.TituloRet.Sacado.CNPJCPF      := LJsonItemObject.AsString['cpfCnpj'];
              end;
          end;

          {Boleto}
          if LJsonArray.ItemAsJSONObject[I].IsJSONObject('boleto') then
          begin
            LJsonItemObject := LJsonArray.ItemAsJSONObject[I].AsJSONObject['boleto'];
            LListaRetorno.DadosRet.IDBoleto.CodBarras      := LJsonItemObject.AsString['codigoBarras'];
            LListaRetorno.DadosRet.IDBoleto.LinhaDig       := LJsonItemObject.AsString['linhaDigitavel'];
            LListaRetorno.DadosRet.IDBoleto.NossoNum       := LJsonItemObject.AsString['nossoNumero'];
            LListaRetorno.DadosRet.TituloRet.CodBarras     := LListaRetorno.DadosRet.IDBoleto.CodBarras;
            LListaRetorno.DadosRet.TituloRet.LinhaDig      := LListaRetorno.DadosRet.IDBoleto.LinhaDig;
            LListaRetorno.DadosRet.TituloRet.NossoNumero   := LListaRetorno.DadosRet.IDBoleto.NossoNum;
          end;

          {PIX}
          if LJsonArray.ItemAsJSONObject[I].IsJSONObject('pix') then
          begin
            LJsonItemObject := LJsonArray.ItemAsJSONObject[I].AsJSONObject['pix'];
            LListaRetorno.DadosRet.TituloRet.EMV      := LJsonItemObject.AsString['pixCopiaECola'];
            LListaRetorno.DadosRet.TituloRet.TxId     := LJsonItemObject.AsString['txid'];
          end;

        end;
      end;
    finally
      LJsonObject.free;
    end;
  end;
end;

end.

