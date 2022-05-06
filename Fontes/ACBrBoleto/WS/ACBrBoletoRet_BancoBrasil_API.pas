{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

unit ACBrBoletoRet_BancoBrasil_API;

interface

uses
  Classes, SysUtils, ACBrBoleto,ACBrBoletoWS, ACBrBoletoRetorno,
//  {$IfDef USE_JSONDATAOBJECTS_UNIT}
//    JsonDataObjects_ACBr,
//  {$Else}
    Jsons,
//  {$EndIf}
   pcnConversao;

type

{ TRetornoEnvio_BancoBrasil_API }

 TRetornoEnvio_BancoBrasil_API = class(TRetornoEnvioREST)
 private
   function DateBBtoDateTime(Const AValue : String) : TDateTime;
 public
   constructor Create(ABoletoWS: TACBrBoleto); override;
   destructor  Destroy; Override;
   function LerRetorno: Boolean; override;
   function RetornoEnvio: Boolean; override;

 end;

implementation

uses
  ACBrBoletoConversao, ACBrUtil.Strings, ACBrUtil.DateTime;

{ TRetornoEnvio }

constructor TRetornoEnvio_BancoBrasil_API.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);

end;

function TRetornoEnvio_BancoBrasil_API.DateBBtoDateTime(
  const AValue: String): TDateTime;
begin
  Result := StrToDateDef( StringReplace( AValue,'.','/', [rfReplaceAll] ),0);
end;

destructor TRetornoEnvio_BancoBrasil_API.Destroy;
begin
  inherited Destroy;
end;

function TRetornoEnvio_BancoBrasil_API.LerRetorno: Boolean;
var
  Retorno: TRetEnvio;
  AJson: TJson;
  AJSonRejeicao, AJSonObject: TJsonObject;
  ARejeicao: TRejeicao;
  AJSonResp, AJsonBoletos: TJsonArray;
  I: Integer;
  TipoOperacao : TOperacao;
begin
  Result := True;
  TipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;

  Retorno := ACBrBoleto.CriarRetornoWebNaLista;
  Retorno.HTTPResultCode := HTTPResultCode;

  if RetWS <> '' then
  begin
    try
      AJSon := TJson.Create;
      try
        AJSon.Parse(RetWS);
        Retorno.JSON           := AJson.Stringify;
        //retorna quando houver erro
        case TipoOperacao of
          tpInclui,
          tpPIXCriar,
          tpPIXCancelar,
          tpPIXConsultar :
            begin
              case HTTPResultCode of
                400,
                403,
                500 :
                  begin
                    AJSonResp := AJson.Values['erros'].AsArray;
                    for I := 0 to Pred(AJSonResp.Count) do
                    begin
                      AJSonRejeicao        := AJSonResp[I].AsObject;
                      ARejeicao            := Retorno.CriarRejeicaoLista;
                      ARejeicao.Codigo     := AJSonRejeicao.Values['codigo'].AsString;
                      ARejeicao.Versao     := AJSonRejeicao.Values['versao'].AsString;
                      ARejeicao.Mensagem   := AJSonRejeicao.Values['mensagem'].AsString;
                      ARejeicao.Ocorrencia := AJSonRejeicao.Values['ocorrencia'].AsString;
                    end;
                  end;
                401 :
                  begin
                    if (AJson.Values['error'].AsString <> '') then
                    begin
                      ARejeicao            := Retorno.CriarRejeicaoLista;
                      ARejeicao.Codigo     := AJson.Values['statusCode'].AsString;
                      ARejeicao.Versao     := AJson.Values['error'].AsString;
                      ARejeicao.Mensagem   := AJson.Values['message'].AsString;
                    end;
                  end;
              end;
            end;
          tpBaixa,
          tpAltera,
          tpConsultaDetalhe :
            begin
              case HTTPResultCode of
                400,
                403,
                500 :
                  begin
                    AJSonResp := AJson.Values['errors'].AsArray;
                    for I := 0 to Pred(AJSonResp.Count) do
                    begin
                      AJSonRejeicao        := AJSonResp[I].AsObject;
                      ARejeicao            := Retorno.CriarRejeicaoLista;
                      ARejeicao.Codigo     := AJSonRejeicao.Values['code'].AsString;
                      ARejeicao.Mensagem   := AJSonRejeicao.Values['message'].AsString;
                    end;
                  end;
                401 :
                  begin
                    if (AJson.Values['error'].AsString <> '') then
                    begin
                      ARejeicao            := Retorno.CriarRejeicaoLista;
                      ARejeicao.Codigo     := AJson.Values['statusCode'].AsString;
                      ARejeicao.Versao     := AJson.Values['error'].AsString;
                      ARejeicao.Mensagem   := AJson.Values['message'].AsString;
                    end;
                  end;
              end;
            end;
          tpConsulta :
            begin
              case HTTPResultCode of
                400,
                500,
                503 :
                  begin
                    AJSonResp := AJson.Values['erros'].AsArray;
                    for I := 0 to Pred(AJSonResp.Count) do
                    begin
                      AJSonRejeicao        := AJSonResp[I].AsObject;
                      ARejeicao            := Retorno.CriarRejeicaoLista;
                      ARejeicao.Codigo     := AJSonRejeicao.Values['codigoMensagem'].AsString;
                      ARejeicao.Versao     := AJSonRejeicao.Values['versaoMensagem'].AsString;
                      ARejeicao.Mensagem   := AJSonRejeicao.Values['textoMensagem'].AsString;
                      ARejeicao.Ocorrencia := AJSonRejeicao.Values['codigoRetorno'].AsString;
                    end;
                  end;
                401 :
                  begin
                    if (AJson.Values['error'].AsString <> '') then
                    begin
                      ARejeicao            := Retorno.CriarRejeicaoLista;
                      ARejeicao.Codigo     := AJson.Values['statusCode'].AsString;
                      ARejeicao.Versao     := AJson.Values['error'].AsString;
                      ARejeicao.Mensagem   := AJson.Values['message'].AsString;
                    end;
                  end;
              end;
            end;
        end;

        //retorna quando tiver sucesso
        if (Retorno.ListaRejeicao.Count = 0) then
        begin
          if (TipoOperacao = tpInclui) then
          begin

            Retorno.DadosRet.IDBoleto.CodBarras      := AJson.Values['codigoBarraNumerico'].AsString;
            Retorno.DadosRet.IDBoleto.LinhaDig       := AJson.Values['linhaDigitavel'].AsString;
            Retorno.DadosRet.IDBoleto.NossoNum       := AJson.Values['numero'].AsString;

            Retorno.DadosRet.TituloRet.CodBarras     := Retorno.DadosRet.IDBoleto.CodBarras;
            Retorno.DadosRet.TituloRet.LinhaDig      := Retorno.DadosRet.IDBoleto.LinhaDig;
            Retorno.DadosRet.TituloRet.NossoNumero   := Retorno.DadosRet.IDBoleto.NossoNum;
            Retorno.DadosRet.TituloRet.Carteira      := AJson.Values['numeroCarteira'].AsString;
            Retorno.DadosRet.TituloRet.Modalidade    := AJson.Values['numeroVariacaoCarteira'].AsInteger;
            Retorno.DadosRet.TituloRet.CodigoCliente := AJson.Values['codigoCliente'].AsNumber;
            Retorno.DadosRet.TituloRet.Contrato      := AJson.Values['numeroContratoCobranca'].AsString;

            AJSonObject                              := AJson.Values['qrCode'].AsObject;
            QRCodeRet.url                            := AJSonObject.Values['url'].AsString;
            QRCodeRet.txId                           := AJSonObject.Values['txId'].AsString;
            QRCodeRet.emv                            := AJSonObject.Values['emv'].AsString;

            Retorno.DadosRet.TituloRet.UrlPix        := QRCodeRet.url;
            Retorno.DadosRet.TituloRet.TxId          := QRCodeRet.txId;
            Retorno.DadosRet.TituloRet.EMV           := QRCodeRet.emv;

          end else
          if (TipoOperacao = tpConsulta) then
          begin
            AJsonBoletos := AJson.Values['boletos'].AsArray;
            for I := 0 to Pred(AJsonBoletos.Count) do
            begin
              if I > 0 then
                Retorno := ACBrBoleto.CriarRetornoWebNaLista;
                
              AJSonObject  := AJsonBoletos[I].AsObject;

              Retorno.indicadorContinuidade := AJson.Values['indicadorContinuidade'].AsString = 'S';
              Retorno.proximoIndice         := AJson.Values['proximoIndice'].AsInteger;



              Retorno.DadosRet.IDBoleto.CodBarras      := '';
              Retorno.DadosRet.IDBoleto.LinhaDig       := '';
              Retorno.DadosRet.IDBoleto.NossoNum       := AJSonObject.Values['numeroBoletoBB'].AsString;

              Retorno.DadosRet.TituloRet.CodBarras      := Retorno.DadosRet.IDBoleto.CodBarras;
              Retorno.DadosRet.TituloRet.LinhaDig       := Retorno.DadosRet.IDBoleto.LinhaDig;


              Retorno.DadosRet.TituloRet.NossoNumero                := Retorno.DadosRet.IDBoleto.NossoNum;
              Retorno.DadosRet.TituloRet.DataRegistro               := DateBBtoDateTime(AJSonObject.Values['dataRegistro'].AsString);
              Retorno.DadosRet.TituloRet.Vencimento                 := DateBBtoDateTime(AJSonObject.Values['dataVencimento'].AsString);
              Retorno.DadosRet.TituloRet.ValorDocumento             := AJSonObject.Values['valorOriginal'].AsNumber;
              Retorno.DadosRet.TituloRet.Carteira                   := OnlyNumber(AJSonObject.Values['carteiraConvenio'].AsString);
              Retorno.DadosRet.TituloRet.Modalidade                 := AJSonObject.Values['variacaoCarteiraConvenio'].AsInteger;
              Retorno.DadosRet.TituloRet.codigoEstadoTituloCobranca := OnlyNumber(AJSonObject.Values['codigoEstadoTituloCobranca'].AsString);
              Retorno.DadosRet.TituloRet.estadoTituloCobranca       := AJSonObject.Values['estadoTituloCobranca'].AsString;
              Retorno.DadosRet.TituloRet.contrato                   := AJSonObject.Values['contrato'].AsString;
              Retorno.DadosRet.TituloRet.DataMovimento              := DateBBtoDateTime(AJSonObject.Values['dataMovimento'].AsString);
              Retorno.DadosRet.TituloRet.dataCredito                := DateBBtoDateTime(AJSonObject.Values['dataCredito'].AsString);
              Retorno.DadosRet.TituloRet.ValorAtual                 := AJSonObject.Values['valorAtual'].AsNumber;
              Retorno.DadosRet.TituloRet.ValorPago                  := AJSonObject.Values['valorPago'].AsNumber;

            end;
          end else
          if (TipoOperacao = tpConsultaDetalhe) then
          begin
            Retorno.DadosRet.IDBoleto.CodBarras      := AJson.Values['textoCodigoBarrasTituloCobranca'].AsString;
            Retorno.DadosRet.IDBoleto.LinhaDig       := AJson.Values['codigoLinhaDigitavel'].AsString;
            Retorno.DadosRet.IDBoleto.NossoNum       := AJson.Values['numeroTituloCedenteCobranca'].AsString;

            Retorno.DadosRet.TituloRet.CodBarras     := Retorno.DadosRet.IDBoleto.CodBarras;
            Retorno.DadosRet.TituloRet.LinhaDig      := Retorno.DadosRet.IDBoleto.LinhaDig;
            Retorno.DadosRet.TituloRet.NossoNumero   := Retorno.DadosRet.IDBoleto.NossoNum;
            Retorno.DadosRet.TituloRet.Carteira      := AJson.Values['numeroCarteiraCobranca'].AsString;
            Retorno.DadosRet.TituloRet.Modalidade    := AJson.Values['numeroVariacaoCarteiraCobranca'].AsInteger;
            Retorno.DadosRet.TituloRet.Contrato      := AJson.Values['numeroContratoCobranca'].AsString;

            // Dados Adicionais

            Retorno.DadosRet.TituloRet.NumeroDocumento            := AJson.Values['numeroTituloCedenteCobranca'].AsString;
            Retorno.DadosRet.TituloRet.DataRegistro               := DateBBtoDateTime( AJson.Values['dataRegistroTituloCobranca'].AsString );
            Retorno.DadosRet.TituloRet.Vencimento                 := DateBBtoDateTime( AJson.Values['dataVencimentoTituloCobranca'].AsString );
            Retorno.DadosRet.TituloRet.ValorDocumento             := AJson.Values['valorOriginalTituloCobranca'].AsNumber;
            Retorno.DadosRet.TituloRet.Carteira                   := AJson.Values['numeroCarteiraCobranca'].AsString;
            Retorno.DadosRet.TituloRet.Modalidade                 := StrToIntDef( AJson.Values['numeroVariacaoCarteiraCobranca'].asString ,0 );
            Retorno.DadosRet.TituloRet.codigoEstadoTituloCobranca := AJson.Values['codigoEstadoTituloCobranca'].AsString;
            Retorno.DadosRet.TituloRet.contrato                   := AJson.Values['numeroContratoCobranca'].AsString;
            Retorno.DadosRet.TituloRet.DataMovimento              := DateBBtoDateTime(AJson.Values['dataRegistroTituloCobranca'].AsString);
            Retorno.DadosRet.TituloRet.Vencimento                 := DateBBtoDateTime(AJson.Values['dataVencimentoTituloCobranca'].AsString);
            Retorno.DadosRet.TituloRet.DataDocumento              := DateBBtoDateTime(AJson.Values['dataEmissaoTituloCobranca'].AsString);
            Retorno.DadosRet.TituloRet.DataCredito                := DateBBtoDateTime(AJson.Values['dataCreditoLiquidacao'].AsString);
            Retorno.DadosRet.TituloRet.DataBaixa                  := DateBBtoDateTime(AJson.Values['dataRecebimentoTitulo'].AsString);
            Retorno.DadosRet.TituloRet.ValorAtual                 := AJson.Values['valorAtualTituloCobranca'].AsNumber;
            Retorno.DadosRet.TituloRet.ValorPago                  := AJson.Values['valorPagoSacado'].AsNumber;


            Retorno.DadosRet.TituloRet.Sacado.NomeSacado          := AJson.Values['nomeSacadoCobranca'].AsString;
            Retorno.DadosRet.TituloRet.Sacado.Logradouro          := AJson.Values['textoEnderecoSacadoCobranca'].AsString;
            Retorno.DadosRet.TituloRet.Sacado.Bairro              := AJson.Values['nomeBairroSacadoCobranca'].AsString;
            Retorno.DadosRet.TituloRet.Sacado.Cidade              := AJson.Values['nomeMunicipioSacadoCobranca'].AsString;


          end else
          if (TipoOperacao = tpBaixa) then
          begin
            Retorno.DadosRet.TituloRet.contrato      := AJson.Values['numeroContratoCobranca'].AsString;
            Retorno.DadosRet.TituloRet.DataBaixa     := DateBBtoDateTime( AJson.Values['dataBaixa'].AsString);
            Retorno.DadosRet.TituloRet.HoraBaixa     := AJson.Values['horarioBaixa'].AsString;

          end else
          if (TipoOperacao = tpAltera) then
          begin
            Retorno.DadosRet.TituloRet.contrato      := AJson.Values['numeroContratoCobranca'].AsString;
            Retorno.DadosRet.Comprovante.Data        := DateBBtoDateTime( AJson.Values['dataAtualizacao'].AsString);
            Retorno.DadosRet.Comprovante.Hora        := AJson.Values['horarioAtualizacao'].AsString;

          end else
          if (TipoOperacao = tpPIXCriar) or (TipoOperacao = tpPIXCancelar) then
          begin;
            QRCodeRet.url  := AJson.Values['qrCode.url'].AsString;
            QRCodeRet.txId := AJson.Values['qrCode.txId'].AsString;
            QRCodeRet.emv  := AJson.Values['qrCode.emv'].AsString;

            Retorno.DadosRet.TituloRet.UrlPix        := QRCodeRet.url;
            Retorno.DadosRet.TituloRet.TxId          := QRCodeRet.txId;
            Retorno.DadosRet.TituloRet.EMV           := QRCodeRet.emv;
          end else
          if (TipoOperacao = tpPIXConsultar) then
          begin;
            Retorno.DadosRet.IDBoleto.NossoNum         := AJson.Values['id'].AsString;
            Retorno.DadosRet.TituloRet.NossoNumero     := Retorno.DadosRet.IDBoleto.NossoNum;
            Retorno.DadosRet.TituloRet.ValorDocumento  := AJson.Values['valorOriginalTituloCobranca'].AsNumber;
            Retorno.DadosRet.TituloRet.DataRegistro    := DateBBtoDateTime( AJson.Values['dataRegistroTituloCobranca'].AsString );

            QRCodeRet.url  := AJson.Values['qrCode.url'].AsString;
            QRCodeRet.txId := AJson.Values['qrCode.txId'].AsString;
            QRCodeRet.emv  := AJson.Values['qrCode.emv'].AsString;

            Retorno.DadosRet.TituloRet.UrlPix        := QRCodeRet.url;
            Retorno.DadosRet.TituloRet.TxId          := QRCodeRet.txId;
            Retorno.DadosRet.TituloRet.EMV           := QRCodeRet.emv;;
          end;
        end;

      finally
        AJson.free;
      end;


    except
      Result := False;
    end;

  end else
  begin
    case TipoOperacao of
      tpInclui,
      tpBaixa,
      tpAltera,
      tpConsulta,
      tpConsultaDetalhe,
      tpPIXCriar,
      tpPIXCancelar,
      tpPIXConsultar :
        begin
          case HTTPResultCode of
            404 :
              begin
                ARejeicao            := Retorno.CriarRejeicaoLista;
                ARejeicao.Codigo     := '404';
                ARejeicao.Mensagem   := 'NÃO ENCONTRADO. O servidor não conseguiu encontrar o recurso solicitado.';
              end;
            503 :
              begin
                ARejeicao            := Retorno.CriarRejeicaoLista;
                ARejeicao.Codigo     := '503';
                ARejeicao.Versao     := 'ERRO INTERNO BB';
                ARejeicao.Mensagem   := 'SERVIÇO INDISPONÍVEL. O servidor está impossibilitado de lidar com a requisição no momento. Tente mais tarde.';
                ARejeicao.Ocorrencia := 'ERRO INTERNO nos servidores do Banco do Brasil.';
              end;
          end;
        end;
    end;
  end;

end;

function TRetornoEnvio_BancoBrasil_API.RetornoEnvio: Boolean;
begin

  Result:=inherited RetornoEnvio;

end;

end.

