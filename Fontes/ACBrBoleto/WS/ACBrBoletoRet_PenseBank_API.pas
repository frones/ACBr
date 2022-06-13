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
  Classes, SysUtils, ACBrBoleto,ACBrBoletoWS, ACBrBoletoRetorno,
//  {$IfDef USE_JSONDATAOBJECTS_UNIT}
//    JsonDataObjects_ACBr,
//  {$Else}
    Jsons,
//  {$EndIf}
  ACBrUtil, DateUtils, pcnConversao;

type

{ TRetornoEnvio_PenseBank_API }

 TRetornoEnvio_PenseBank_API = class(TRetornoEnvioREST)
 private

 public
   constructor Create(ABoletoWS: TACBrBoleto); override;
   destructor  Destroy; Override;
   function LerRetorno: Boolean; override;
   function RetornoEnvio: Boolean; override;

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

function TRetornoEnvio_PenseBank_API.LerRetorno: Boolean;
var
  Retorno: TRetEnvio;
  AJson: TJson;
  AJSonObject: TJsonObject;
  ARejeicao: TRejeicao;
  AJsonBoletos: TJsonArray;
  I: Integer;
  TipoOperacao : TOperacao;
begin
  Result := True;
  TipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;
  if RetWS <> '' then
  begin
    Retorno := ACBrBoleto.CriarRetornoWebNaLista;

    Retorno.JSON := RetWS;
    try
      AJSon := TJson.Create;
      try
        AJSon.Parse(RetWS);
        if ( AJson.StructType = jsObject ) then
          if not AJson.Values['success'].AsBoolean then
          begin
            ARejeicao            := Retorno.CriarRejeicaoLista;
            ARejeicao.Mensagem   := AJson.Values['message'].AsString;
          end;

        //retorna quando tiver sucesso
        if (Retorno.ListaRejeicao.Count = 0) then
        begin
          if (TipoOperacao = tpInclui) then
          begin
            AJsonObject:=AJson.Values['message'].AsObject;
            Retorno.DadosRet.IDBoleto.CodBarras      := AJsonObject.Values['codigoBarraNumerico'].AsString;
            Retorno.DadosRet.IDBoleto.LinhaDig       := AJsonObject.Values['linhaDigitavel'].AsString;
            Retorno.DadosRet.IDBoleto.NossoNum       := AJsonObject.Values['numeroTituloCliente'].AsString;

            Retorno.DadosRet.IDBoleto.IDBoleto       := IntToStr(AJsonObject.Values['idboleto'].AsInteger);
            Retorno.DadosRet.IDBoleto.LinhaDig       := AJsonObject.Values['linhaDigitavel'].AsString;
            Retorno.DadosRet.IDBoleto.NossoNum       := AJsonObject.Values['numeroTituloCliente'].AsString;
            Retorno.DadosRet.IDBoleto.URL            := AJsonObject.Values['url_boleto'].AsString;
            Retorno.DadosRet.IDBoleto.URLPDF         := AJsonObject.Values['url_pdf'].AsString;

            QRCodeRet.txId                           := AJsonObject.Values['pixHash'].AsString;
            QRCodeRet.emv                            := AJsonObject.Values['pixQrCode'].AsString;
            Retorno.DadosRet.TituloRet.TxId          := QRCodeRet.txId;
            Retorno.DadosRet.TituloRet.EMV           := QRCodeRet.emv;

            Retorno.DadosRet.TituloRet.CodBarras     := Retorno.DadosRet.IDBoleto.CodBarras;
            Retorno.DadosRet.TituloRet.LinhaDig      := Retorno.DadosRet.IDBoleto.LinhaDig;
            Retorno.DadosRet.TituloRet.NossoNumero   := Retorno.DadosRet.IDBoleto.NossoNum;
            Retorno.DadosRet.TituloRet.SeuNumero     := AJsonObject.Values['idexterno'].AsString;

          end
          else
          begin
            if (TipoOperacao in [tpConsulta,tpConsultaDetalhe]) then
            begin
              AJsonBoletos := TJsonArray.Create;
              AJsonBoletos.Parse( AJson.Stringify );
              for I := 0 to Pred(AJsonBoletos.Count) do
              begin
                if I > 0 then
                  Retorno := ACBrBoleto.CriarRetornoWebNaLista;
                Retorno.JSON:=AJson.Stringify;
                AJSonObject  := AJsonBoletos[I].AsObject;

                Retorno.DadosRet.IDBoleto.IDBoleto        := IntToStr(AJsonObject.Values['idboleto'].AsInteger);
                Retorno.DadosRet.IDBoleto.CodBarras       := '';
                Retorno.DadosRet.IDBoleto.LinhaDig        := AJSonObject.Values['codigoLinhaDigitavel'].AsString;
                Retorno.DadosRet.IDBoleto.NossoNum        := '';
                Retorno.indicadorContinuidade             := false;
                Retorno.DadosRet.TituloRet.CodBarras      := Retorno.DadosRet.IDBoleto.CodBarras;
                Retorno.DadosRet.TituloRet.LinhaDig       := Retorno.DadosRet.IDBoleto.LinhaDig;

                Retorno.DadosRet.TituloRet.NossoNumero                := Retorno.DadosRet.IDBoleto.NossoNum;
                Retorno.DadosRet.TituloRet.Vencimento                 := StrToDate(AJSonObject.Values['dataVencimentoTituloCobranca'].AsString);
                Retorno.DadosRet.TituloRet.ValorDocumento             := AJSonObject.Values['valorOriginalTituloCobranca'].AsNumber;
                Retorno.DadosRet.TituloRet.ValorAtual                 := AJSonObject.Values['valorOriginalTituloCobranca'].AsNumber;

                if( AJSonObject.Values['situacaoEstadoTituloCobranca'].asString = C_LIQUIDADO ) or
                   ( AJSonObject.Values['situacaoEstadoTituloCobranca'].asString = C_BAIXADO_POS_SOLICITACAO ) then
                Retorno.DadosRet.TituloRet.ValorPago                  := AJSonObject.Values['valor'].AsNumber;

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

function TRetornoEnvio_PenseBank_API.RetornoEnvio: Boolean;
begin

  Result:=inherited RetornoEnvio;

end;

end.

