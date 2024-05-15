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

unit ACBrBoletoRet_Sicredi_APIV2;

interface

uses
  SysUtils,
  ACBrBoleto,
  ACBrBoletoRetorno,
  ACBrBoletoWS,
  ACBrBoletoWS.Rest;

type

{ TRetornoEnvio_Sicredi_APIV2 }

 TRetornoEnvio_Sicredi_APIV2 = class(TRetornoEnvioREST)
 private
   function DateSicreditoDateTime(Const AValue : String) : TDateTime;
   function TimeSicreditoDateTime(Const AValue : String) : String;
 public
   constructor Create(ABoletoWS: TACBrBoleto); override;
   destructor  Destroy; Override;
   function LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean; override;
   function LerListaRetorno: Boolean; override;
   function RetornoEnvio(const AIndex: Integer): Boolean; override;

 end;

implementation

uses
  ACBrBoletoConversao,
  ACBrJSON;

{ TRetornoEnvio }

constructor TRetornoEnvio_Sicredi_APIV2.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);

end;

function TRetornoEnvio_Sicredi_APIV2.DateSicreditoDateTime(const AValue: String): TDateTime;
var
  LData, LAno, LMes, LDia : String;
begin
    LAno := Copy( aValue, 0,4 );
    LMes := Copy( aValue, 6,2 );
    LDia := Copy( aValue, 9,2 );
    LData := Format( '%s/%s/%s' , [LDia,LMes,LAno]);
    Result := StrToDateDef( LData ,0 );
end;

destructor TRetornoEnvio_Sicredi_APIV2.Destroy;
begin
  inherited Destroy;
end;

function TRetornoEnvio_Sicredi_APIV2.LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;
var
  LJsonObject: TACBrJSONObject;
  LJsonArray: TACBrJSONArray;
  LTipoOperacao : TOperacao;
  LMensagemRejeicao: TACBrBoletoRejeicao;
  I : Integer;
begin
  Result := True;
  LTipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;

  ARetornoWs.JSONEnvio       := EnvWs;
  ARetornoWS.HTTPResultCode  := HTTPResultCode;
  ARetornoWS.Header.Operacao := LTipoOperacao;

  if RetWS <> '' then
  begin

    try
      LJsonObject := TACBrJSONObject.Parse(RetWS);
      try
        ARetornoWS.JSON           := LJsonObject.ToJSON;
        if HTTPResultCode >= 400 then
        begin
          if (LJsonObject.AsString['error'] <> '') then
          begin
            LMensagemRejeicao            := ARetornoWS.CriarRejeicaoLista;
            LMensagemRejeicao.Codigo     := LJsonObject.AsString['status'];
            LMensagemRejeicao.Versao     := LJsonObject.AsString['error'];
            LMensagemRejeicao.Mensagem   := LJsonObject.AsString['message'];
          end;
          if LJsonObject.IsJSONArray('erros') then
          begin
            LJsonArray := LJsonObject.AsJSONArray['erros'];
            for I := 0 to Pred(LJsonArray.Count) do
            begin
              LMensagemRejeicao            := ARetornoWS.CriarRejeicaoLista;
              LMensagemRejeicao.Codigo     := LJsonArray.ItemAsJSONObject[I].AsString['status'];
              LMensagemRejeicao.Mensagem   := LJsonArray.ItemAsJSONObject[I].AsString['message'];
            end;
          end;
        end;

        case LTipoOperacao of
          tpBaixa,
          tpAltera,
          tpConsultaDetalhe :
            begin
              ARetornoWS.DadosRet.TituloRet.CodBarras      := LJsonObject.AsString['codigoBarras'];
              ARetornoWS.DadosRet.TituloRet.LinhaDig       := LJsonObject.AsString['linhaDigitavel'];
              ARetornoWS.DadosRet.TituloRet.Carteira       := LJsonObject.AsString['carteira'];
              ARetornoWS.DadosRet.TituloRet.SeuNumero      := LJsonObject.AsString['seuNumero'];
              ARetornoWS.DadosRet.TituloRet.NossoNumero    := LJsonObject.AsString['nossoNumero'];
              //Pagador
              //ARetornoWS.DadosRet.TituloRet.Sacado.codigo         := LJsonObject.AsJSONObject['pagador'].AsString['codigo'];
              ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado      := LJsonObject.AsJSONObject['pagador'].AsString['nome'];
              ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF         := LJsonObject.AsJSONObject['pagador'].AsString['documento'];
              //Datas
              ARetornoWS.DadosRet.TituloRet.DataRegistro           := DateSicreditoDateTime( LJsonObject.AsString['dataEmissao'] );
              ARetornoWS.DadosRet.TituloRet.Vencimento             := DateSicreditoDateTime( LJsonObject.AsString['dataVencimento'] );
              ARetornoWS.DadosRet.TituloRet.DataBaixa              := DateSicreditoDateTime(LJsonObject.AsString['dataPagamento']);
              ARetornoWS.DadosRet.TituloRet.HoraBaixa              := timeSicreditoDateTime(LJsonObject.AsString['dataPagamento']);
              //Valores
              ARetornoWS.DadosRet.TituloRet.ValorDocumento         := LJsonObject.AsFloat['valorNominal'];
              //Situação/Código da situação.
              ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca    := UpperCase(LJsonObject.AsString['situacao']);

              if (Pos('EM CARTEIRA',UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca)) > 0) or (Pos('VENCIDO',UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca)) > 0) then
                ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '1';

              if (Pos('BAIXADO POR SOLICITACAO',UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca)) > 0) then
              begin
                ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '7';
                ARetornoWS.DadosRet.TituloRet.DataBaixa                  := DateSicreditoDateTime(LJsonObject.AsString['dataBaixa']);
                ARetornoWS.DadosRet.TituloRet.HoraBaixa                  := TimeSicreditoDateTime(LJsonObject.AsString['dataBaixa']);

              end;

              if (Pos('LIQUIDADO',UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca)) > 0) then
                ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '6';

              //Multa/Juros
              ARetornoWS.DadosRet.TituloRet.PercentualMulta        := LJsonObject.AsFloat['valorMulta'];
              ARetornoWS.DadosRet.TituloRet.ValorAbatimento        := LJsonObject.AsFloat['abatimento'];
              ARetornoWS.DadosRet.TituloRet.ValorMoraJuros         := LJsonObject.AsFloat['juros'];
              ARetornoWS.DadosRet.TituloRet.DiasDeProtesto         := LJsonObject.AsInteger['diasProtesto'];
              ARetornoWS.DadosRet.TituloRet.TxId                   := LJsonObject.AsString['txId'];
              ARetornoWS.DadosRet.TituloRet.EMV                    := LJsonObject.AsString['codigoQrCode'];
              ARetornoWS.DadosRet.TituloRet.PercentualMulta        := LJsonObject.AsFloat['valorMulta'];
              if ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca = '6' then
              begin
                 ARetornoWS.DadosRet.TituloRet.ValorPago              := LJsonObject.AsJSONObject['dadosLiquidacao'].AsFloat['valor'];
                 ARetornoWS.DadosRet.TituloRet.ValorMoraJuros         := LJsonObject.AsJSONObject['dadosLiquidacao'].AsFloat['juros'];
                 ARetornoWS.DadosRet.TituloRet.ValorAbatimento        := LJsonObject.AsJSONObject['dadosLiquidacao'].AsFloat['abatimento'];
                 ARetornoWS.DadosRet.TituloRet.DataBaixa              := DateSicreditoDateTime(LJsonObject.AsJSONObject['dadosLiquidacao'].AsString['data']);
                 ARetornoWS.DadosRet.TituloRet.HoraBaixa              := TimeSicreditoDateTime(LJsonObject.AsJSONObject['dadosLiquidacao'].AsString['data']);
                 ARetornoWS.DadosRet.TituloRet.ValorDesconto          := LJsonObject.AsJSONObject['dadosLiquidacao'].AsFloat['desconto'];
                 ARetornoWS.DadosRet.TituloRet.ValorMulta             := LJsonObject.AsJSONObject['dadosLiquidacao'].AsFloat['multa'];
              end;

              if LJsonObject.IsJSONArray('descontos') then
              begin
                LJsonArray := LJsonObject.AsJSONArray['descontos'];
                for I := 0 to LJsonArray.Count - 1 do
                begin
                  if I = 0 then
                  begin
                    ARetornoWS.DadosRet.TituloRet.ValorDesconto:= LJsonArray.ItemAsJSONObject[i].AsFloat['ValorDesconto'];
                    ARetornoWS.DadosRet.TituloRet.DataDesconto := DateSicreditoDateTime(LJsonArray.ItemAsJSONObject[i].AsString['dataLimite']);
                  end;
                  if I = 1 then
                  begin
                    ARetornoWS.DadosRet.TituloRet.ValorDesconto2:= LJsonArray.ItemAsJSONObject[i].AsFloat['valorDesconto'];
                    ARetornoWS.DadosRet.TituloRet.DataDesconto2 := DateSicreditoDateTime(LJsonArray.ItemAsJSONObject[i].AsString['dataLimite']);
                  end;
                  if I = 2 then
                  begin
                    ARetornoWS.DadosRet.TituloRet.ValorDesconto3:= LJsonArray.ItemAsJSONObject[i].AsFloat['valorDesconto'];
                    ARetornoWS.DadosRet.TituloRet.DataDesconto3 := DateSicreditoDateTime(LJsonArray.ItemAsJSONObject[i].AsString['dataLimite']);
                  end;
                end;
              end;
            end;
        end;

        //retorna quando tiver sucesso
        if (ARetornoWS.ListaRejeicao.Count = 0) then
        begin
          if (LTipoOperacao = tpInclui) then
          begin
            ARetornoWS.DadosRet.IDBoleto.CodBarras      := LJsonObject.AsString['codigoBarras'];
            ARetornoWS.DadosRet.IDBoleto.LinhaDig       := LJsonObject.AsString['linhaDigitavel'];
            ARetornoWS.DadosRet.IDBoleto.NossoNum       := LJsonObject.AsString['nossoNumero'];
            ARetornoWS.DadosRet.TituloRet.UrlPix        := '';//LJsonObject.AsString['url'];
            ARetornoWS.DadosRet.TituloRet.TxId          := LJsonObject.AsString['txid'];
            ARetornoWS.DadosRet.TituloRet.EMV           := LJsonObject.AsString['qrCode'];

            ARetornoWS.DadosRet.TituloRet.CodBarras     := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig      := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
            ARetornoWS.DadosRet.TituloRet.NossoNumero   := ARetornoWS.DadosRet.IDBoleto.NossoNum;
          end else
          if (LTipoOperacao = tpBaixa) then
          begin
            ARetornoWS.DadosRet.TituloRet.NossoNumero   := LJsonObject.AsString['nossoNumero'];
            ARetornoWS.DadosRet.TituloRet.DataBaixa     := StrToDateDef(LJsonObject.AsString['dataMovimento'],0);
            ARetornoWS.DadosRet.TituloRet.HoraBaixa     := timeSicreditoDateTime(LJsonObject.AsString['dataMovimento']);
          end else
          if (LTipoOperacao in [tpAltera]) then
          begin
            ARetornoWS.DadosRet.TituloRet.NossoNumero      := LJsonObject.AsString['nossoNumero'];
            ARetornoWS.DadosRet.TituloRet.DataMovimento     := DateSicreditoDateTime( LJsonObject.AsString['dataMovimento'] );
          end;
        end;
      finally
        LJsonObject.free;
      end;
    except
      Result := False;
    end;
  end
  else
  begin
    case HTTPResultCode of
      404 :
        begin
          LMensagemRejeicao            := ARetornoWS.CriarRejeicaoLista;
          LMensagemRejeicao.Codigo     := '404';
          LMensagemRejeicao.Mensagem   := 'NÃO ENCONTRADO. O servidor não conseguiu encontrar o recurso solicitado.';
        end;
      503 :
        begin
          LMensagemRejeicao            := ARetornoWS.CriarRejeicaoLista;
          LMensagemRejeicao.Codigo     := '503';
          LMensagemRejeicao.Versao     := 'ERRO INTERNO SICREDI';
          LMensagemRejeicao.Mensagem   := 'SERVIÇO INDISPONÍVEL. O servidor está impossibilitado de lidar com a requisição no momento. Tente mais tarde.';
          LMensagemRejeicao.Ocorrencia := 'ERRO INTERNO nos servidores do Sicredi.';
        end;
    end;
  end;
end;

function TRetornoEnvio_Sicredi_APIV2.LerListaRetorno: Boolean;
var
  LJsonObject, LItemObject : TACBrJSONObject;
  LJsonArray: TACBrJSONArray;
  LListaRetorno: TACBrBoletoRetornoWS;
  LMensagemRejeicao: TACBrBoletoRejeicao;
  I: Integer;
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
        LListaRetorno.JSON           := LJsonObject.ToJSON;
        //retorna quando houver erro

        if HTTPResultCode >= 400 then
        begin
          if (LJsonObject.AsString['error'] <> '') then
          begin
            LMensagemRejeicao            := LListaRetorno.CriarRejeicaoLista;
            LMensagemRejeicao.Codigo     := LJsonObject.AsString['code'];
            LMensagemRejeicao.Versao     := LJsonObject.AsString['error'];
            LMensagemRejeicao.Mensagem   := LJsonObject.AsString['message'];
          end;
          if LJsonObject.IsJSONArray('erros') then
          begin
            LJsonArray := LJsonObject.AsJSONArray['erros'];
            for I := 0 to Pred(LJsonArray.Count) do
            begin
              LMensagemRejeicao            := LListaRetorno.CriarRejeicaoLista;
              LMensagemRejeicao.Codigo     := LJsonArray.ItemAsJSONObject[I].AsString['status'];
              LMensagemRejeicao.Mensagem   := LJsonArray.ItemAsJSONObject[I].AsString['message'];
            end;
          end;
        end;

        //retorna quando tiver sucesso
        if (LListaRetorno.ListaRejeicao.Count = 0) then
        begin
          LJsonArray := LJsonObject.AsJSONArray['items'];
          LListaRetorno.indicadorContinuidade := LJsonObject.AsBoolean['hasNext'];
          for I := 0 to Pred(LJsonArray.Count) do
          begin
            if I > 0 then
              LListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;

            LItemObject  := LJsonArray.ItemAsJSONObject[I];

            LListaRetorno.DadosRet.IDBoleto.CodBarras      := '';
            LListaRetorno.DadosRet.IDBoleto.LinhaDig       := '';
            LListaRetorno.DadosRet.IDBoleto.NossoNum       := LItemObject.AsString['nossoNumero'];

            LListaRetorno.DadosRet.TituloRet.CodBarras      := LListaRetorno.DadosRet.IDBoleto.CodBarras;
            LListaRetorno.DadosRet.TituloRet.LinhaDig       := LListaRetorno.DadosRet.IDBoleto.LinhaDig;


            LListaRetorno.DadosRet.TituloRet.NossoNumero                := LListaRetorno.DadosRet.IDBoleto.NossoNum;
            LListaRetorno.DadosRet.TituloRet.DataBaixa                  := DateSicreditoDateTime(LItemObject.AsString['dataPagamento']);
            LListaRetorno.DadosRet.TituloRet.HoraBaixa                  := TimeSicreditoDateTime(LItemObject.AsString['dataPagamento']);
            LListaRetorno.DadosRet.TituloRet.SeuNumero                  := LItemObject.AsString['seuNumero'];
            LListaRetorno.DadosRet.TituloRet.ValorDocumento             := LItemObject.AsFloat['valor'];
            LListaRetorno.DadosRet.TituloRet.ValorRecebido              := LItemObject.AsFloat['valorLiquidado'];
            LListaRetorno.DadosRet.TituloRet.ValorPago                  := LItemObject.AsFloat['valorLiquidado'];
            LListaRetorno.DadosRet.TituloRet.ValorRecebido              := LItemObject.AsFloat['valorLiquidado'];
            LListaRetorno.DadosRet.TituloRet.ValorMoraJuros             := LItemObject.AsFloat['jurosLiquido'];
            LListaRetorno.DadosRet.TituloRet.ValorDesconto              := LItemObject.AsFloat['descontoLiquido'];
            LListaRetorno.DadosRet.TituloRet.ValorOutrosCreditos        := LItemObject.AsFloat['multaLiquida'];
            LListaRetorno.DadosRet.TituloRet.ValorAbatimento            := LItemObject.AsFloat['abatimentoLiquido'];
            LListaRetorno.DadosRet.TituloRet.Mensagem.Text              := LItemObject.AsString['tipoLiquidacao'];
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
      400 :
        begin
          LMensagemRejeicao            := LListaRetorno.CriarRejeicaoLista;
          LMensagemRejeicao.Codigo     := '400';
          LMensagemRejeicao.Mensagem   := Msg;
        end;
      404 :
        begin
          LMensagemRejeicao            := LListaRetorno.CriarRejeicaoLista;
          LMensagemRejeicao.Codigo     := '404';
          LMensagemRejeicao.Mensagem   := 'NÃO ENCONTRADO. O servidor não conseguiu encontrar o recurso solicitado.';
        end;
      503 :
        begin
          LMensagemRejeicao            := LListaRetorno.CriarRejeicaoLista;
          LMensagemRejeicao.Codigo     := '503';
          LMensagemRejeicao.Versao     := 'ERRO INTERNO SICREDI';
          LMensagemRejeicao.Mensagem   := 'SERVIÇO INDISPONÍVEL. O servidor está impossibilitado de lidar com a requisição no momento. Tente mais tarde.';
          LMensagemRejeicao.Ocorrencia := 'ERRO INTERNO nos servidores do Sicredi.';
        end;
    end;
  end;
end;

function TRetornoEnvio_Sicredi_APIV2.RetornoEnvio(const AIndex: Integer): Boolean;
begin

  Result:=inherited RetornoEnvio(AIndex);

end;

function TRetornoEnvio_Sicredi_APIV2.TimeSicreditoDateTime(const AValue: String): String;
var
  LHora, LMinuto, LSegundos : String;
begin
  LHora     := Copy( AValue, 12,2 );
  LMinuto   := Copy( AValue, 15,2 );
  LSegundos := Copy( AValue, 18,2 );
  Result   := Format( '%s:%s:%s' , [LHora,LMinuto,LSegundos]);
end;

end.


