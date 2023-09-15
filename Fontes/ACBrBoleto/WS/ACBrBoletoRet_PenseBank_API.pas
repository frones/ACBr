{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Victor Hugo Gonzales - Panda                   }
{                               Leandro do Couto                               }
{                               Fernando Henrique                              }
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

unit ACBrBoletoRet_PenseBank_API;

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
  ACBrBoletoWS.Rest;

type

{ TRetornoEnvio_PenseBank_API }

 TRetornoEnvio_PenseBank_API = class(TRetornoEnvioREST)
 private

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
  C_LIQUIDADO = 'LIQUIDADO';
  C_BAIXADO_POS_SOLICITACAO = 'BAIXADO POR SOLICITACAO';

{ TRetornoEnvio }

constructor TRetornoEnvio_PenseBank_API.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);

end;

destructor TRetornoEnvio_PenseBank_API.Destroy;
begin
  inherited Destroy;
end;

function TRetornoEnvio_PenseBank_API.LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;

var
  AJson: TJson;
  AJSonObject: TJsonObject;
  ARejeicao: TACBrBoletoRejeicao;
  AJsonBoletos: TJsonArray;
  TipoOperacao : TOperacao;
begin
  Result := True;

  TipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;
  ARetornoWS.HTTPResultCode := HTTPResultCode;
  ARetornoWS.JSONEnvio      := EnvWs;
  ARetornoWS.Header.Operacao := TipoOperacao;

  if RetWS <> '' then
  begin
    try
      AJSon := TJson.Create;
      try
        AJSon.Parse(RetWS);
        if ( AJson.StructType = jsObject ) then
          if not AJson.Values['success'].AsBoolean then
          begin
            ARejeicao            := ARetornoWS.CriarRejeicaoLista;
            ARejeicao.Mensagem   := AJson.Values['message'].AsString;
          end;

        //retorna quando tiver sucesso
        if (ARetornoWS.ListaRejeicao.Count = 0) then
        begin
          if (TipoOperacao = tpInclui) then
          begin
            AJsonObject:=AJson.Values['message'].AsObject;
            ARetornoWS.DadosRet.IDBoleto.CodBarras      := AJsonObject.Values['codigoBarraNumerico'].AsString;
            ARetornoWS.DadosRet.IDBoleto.LinhaDig       := AJsonObject.Values['linhaDigitavel'].AsString;
            ARetornoWS.DadosRet.IDBoleto.NossoNum       := AJsonObject.Values['numeroTituloCliente'].AsString;

            ARetornoWS.DadosRet.IDBoleto.IDBoleto       := IntToStr(AJsonObject.Values['idboleto'].AsInteger);
            ARetornoWS.DadosRet.IDBoleto.LinhaDig       := AJsonObject.Values['linhaDigitavel'].AsString;
            ARetornoWS.DadosRet.IDBoleto.NossoNum       := AJsonObject.Values['numeroTituloCliente'].AsString;
            ARetornoWS.DadosRet.IDBoleto.URL            := AJsonObject.Values['url_boleto'].AsString;
            ARetornoWS.DadosRet.IDBoleto.URLPDF         := AJsonObject.Values['url_pdf'].AsString;

            ARetornoWS.DadosRet.TituloRet.TxId          := AJsonObject.Values['pixHash'].AsString;
            ARetornoWS.DadosRet.TituloRet.EMV           := AJsonObject.Values['pixQrCode'].AsString;

            ARetornoWS.DadosRet.TituloRet.CodBarras     := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig      := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
            ARetornoWS.DadosRet.TituloRet.NossoNumero   := ARetornoWS.DadosRet.IDBoleto.NossoNum;
            ARetornoWS.DadosRet.TituloRet.SeuNumero     := AJsonObject.Values['idexterno'].AsString;

          end
          else
          begin
            if (TipoOperacao = tpConsultaDetalhe) then
            begin
              AJsonBoletos := TJsonArray.Create;
              AJsonBoletos.Parse( AJson.Stringify );
              if (AJsonBoletos.Count > 0) then
              begin
                ARetornoWS.JSON:=AJson.Stringify;
                AJSonObject  := AJsonBoletos[0].AsObject;

                ARetornoWS.DadosRet.IDBoleto.IDBoleto        := IntToStr(AJsonObject.Values['idboleto'].AsInteger);
                ARetornoWS.DadosRet.IDBoleto.CodBarras       := '';
                ARetornoWS.DadosRet.IDBoleto.LinhaDig        := AJSonObject.Values['codigoLinhaDigitavel'].AsString;
                ARetornoWS.DadosRet.IDBoleto.NossoNum        := '';
                ARetornoWS.indicadorContinuidade             := false;
                ARetornoWS.DadosRet.TituloRet.CodBarras      := ARetornoWS.DadosRet.IDBoleto.CodBarras;
                ARetornoWS.DadosRet.TituloRet.LinhaDig       := ARetornoWS.DadosRet.IDBoleto.LinhaDig;

                ARetornoWS.DadosRet.TituloRet.NossoNumero                := ARetornoWS.DadosRet.IDBoleto.NossoNum;
                ARetornoWS.DadosRet.TituloRet.Vencimento                 := StrToDate(AJSonObject.Values['dataVencimentoTituloCobranca'].AsString);
                ARetornoWS.DadosRet.TituloRet.ValorDocumento             := AJSonObject.Values['valorOriginalTituloCobranca'].AsNumber;
                ARetornoWS.DadosRet.TituloRet.ValorAtual                 := AJSonObject.Values['valorOriginalTituloCobranca'].AsNumber;

                if( AJSonObject.Values['situacaoEstadoTituloCobranca'].asString = C_LIQUIDADO ) or
                   ( AJSonObject.Values['situacaoEstadoTituloCobranca'].asString = C_BAIXADO_POS_SOLICITACAO ) then
                ARetornoWS.DadosRet.TituloRet.ValorPago                  := AJSonObject.Values['valor'].AsNumber;

              end;
            end else
            if (TipoOperacao = tpBaixa) then
            begin
              // não possui dados de retorno..
            end else
            if (TipoOperacao = tpAltera) then
            begin
              // não possui dados de retorno..
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

function TRetornoEnvio_PenseBank_API.LerListaRetorno: Boolean;
var
  ListaRetorno: TACBrBoletoRetornoWS;
  AJson: TJson;
  AJSonObject: TJsonObject;
  ARejeicao: TACBrBoletoRejeicao;
  AJsonBoletos: TJsonArray;
  I: Integer;
begin
  Result := True;

  ListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;

  ListaRetorno.HTTPResultCode := HTTPResultCode;
  ListaRetorno.JSONEnvio      := EnvWs;

  if RetWS <> '' then
  begin
    ListaRetorno.JSON := RetWS;
    try
      AJSon := TJson.Create;
      try
        AJSon.Parse(RetWS);
        if ( AJson.StructType = jsObject ) then
          if not AJson.Values['success'].AsBoolean then
          begin
            ARejeicao            := ListaRetorno.CriarRejeicaoLista;
            ARejeicao.Mensagem   := AJson.Values['message'].AsString;
          end;

        //retorna quando tiver sucesso
        if (ListaRetorno.ListaRejeicao.Count = 0) then
        begin
          AJsonBoletos := TJsonArray.Create;
          AJsonBoletos.Parse( AJson.Stringify );
          for I := 0 to Pred(AJsonBoletos.Count) do
          begin
            if I > 0 then
              ListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;
            ListaRetorno.JSON:=AJson.Stringify;
            AJSonObject  := AJsonBoletos[I].AsObject;
            ListaRetorno.DadosRet.IDBoleto.IDBoleto        := IntToStr(AJsonObject.Values['idboleto'].AsInteger);
            ListaRetorno.DadosRet.IDBoleto.CodBarras       := '';
            ListaRetorno.DadosRet.IDBoleto.LinhaDig        := AJSonObject.Values['codigoLinhaDigitavel'].AsString;
            ListaRetorno.DadosRet.IDBoleto.NossoNum        := '';
            ListaRetorno.indicadorContinuidade             := false;
            ListaRetorno.DadosRet.TituloRet.CodBarras      := ListaRetorno.DadosRet.IDBoleto.CodBarras;
            ListaRetorno.DadosRet.TituloRet.LinhaDig       := ListaRetorno.DadosRet.IDBoleto.LinhaDig;
            ListaRetorno.DadosRet.TituloRet.NossoNumero                := ListaRetorno.DadosRet.IDBoleto.NossoNum;
            ListaRetorno.DadosRet.TituloRet.Vencimento                 := StrToDate(AJSonObject.Values['dataVencimentoTituloCobranca'].AsString);
            ListaRetorno.DadosRet.TituloRet.ValorDocumento             := AJSonObject.Values['valorOriginalTituloCobranca'].AsNumber;
            ListaRetorno.DadosRet.TituloRet.ValorAtual                 := AJSonObject.Values['valorOriginalTituloCobranca'].AsNumber;
            if( AJSonObject.Values['situacaoEstadoTituloCobranca'].asString = C_LIQUIDADO ) or
               ( AJSonObject.Values['situacaoEstadoTituloCobranca'].asString = C_BAIXADO_POS_SOLICITACAO ) then
              ListaRetorno.DadosRet.TituloRet.ValorPago                  := AJSonObject.Values['valor'].AsNumber;

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

function TRetornoEnvio_PenseBank_API.RetornoEnvio(const AIndex: Integer): Boolean;
begin

  Result:=inherited RetornoEnvio(AIndex);

end;

end.

