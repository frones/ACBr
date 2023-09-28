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
  Classes,
  SysUtils,
  ACBrBoleto,
  ACBrBoletoWS,
  ACBrBoletoRetorno,
  ACBrBoletoWS.Rest,
  Jsons,
  DateUtils,
  pcnConversao;

type

{ TRetornoEnvio_BancoBrasil_API }

 TRetornoEnvio_BancoBrasil_API = class(TRetornoEnvioREST)
 private
   function DateBBtoDateTime(Const AValue : String) : TDateTime;
 public
   constructor Create(ABoletoWS: TACBrBoleto); override;
   destructor  Destroy; Override;
   function LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean; override;
   function LerListaRetorno: Boolean; override;
   function RetornoEnvio(const AIndex: Integer): Boolean; override;

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

function TRetornoEnvio_BancoBrasil_API.LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;
var
  AJson: TJson;
  AJSonRejeicao, AJSonObject: TJsonObject;
  ARejeicao: TACBrBoletoRejeicao;
  AJSonResp: TJsonArray;
  I: Integer;
  TipoOperacao : TOperacao;
begin
  Result := True;
  TipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;
  ARetornoWS.HTTPResultCode  := HTTPResultCode;
  ARetornoWS.JSONEnvio       := EnvWs;
  ARetornoWS.Header.Operacao := TipoOperacao;
  
  if RetWS <> '' then
  begin
    try
      AJSon := TJson.Create;
      try
        AJSon.Parse(RetWS);
        ARetornoWS.JSON           := AJson.Stringify;
		
        //retorna quando houver erro
        case TipoOperacao of
          tpInclui,
          tpPIXCriar,
          tpPIXCancelar:
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
                      ARejeicao            := ARetornoWS.CriarRejeicaoLista;
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
                      ARejeicao            := ARetornoWS.CriarRejeicaoLista;
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
                      ARejeicao            := ARetornoWS.CriarRejeicaoLista;
                      ARejeicao.Codigo     := AJSonRejeicao.Values['code'].AsString;
                      ARejeicao.Mensagem   := AJSonRejeicao.Values['message'].AsString;
                    end;
                  end;
                401 :
                  begin
                    if (AJson.Values['error'].AsString <> '') then
                    begin
                      ARejeicao            := ARetornoWS.CriarRejeicaoLista;
                      ARejeicao.Codigo     := AJson.Values['statusCode'].AsString;
                      ARejeicao.Versao     := AJson.Values['error'].AsString;
                      ARejeicao.Mensagem   := AJson.Values['message'].AsString;
                    end;
                  end;

              end;
            end;
        end;

        //retorna quando tiver sucesso
        if (ARetornoWS.ListaRejeicao.Count = 0) then
        begin
          if (TipoOperacao = tpInclui) then
          begin

            ARetornoWS.DadosRet.IDBoleto.CodBarras      := AJson.Values['codigoBarraNumerico'].AsString;
            ARetornoWS.DadosRet.IDBoleto.LinhaDig       := AJson.Values['linhaDigitavel'].AsString;
            ARetornoWS.DadosRet.IDBoleto.NossoNum       := AJson.Values['numero'].AsString;

            ARetornoWS.DadosRet.TituloRet.CodBarras     := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig      := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
            ARetornoWS.DadosRet.TituloRet.NossoNumero   := ARetornoWS.DadosRet.IDBoleto.NossoNum;
            ARetornoWS.DadosRet.TituloRet.Carteira      := AJson.Values['numeroCarteira'].AsString;
            ARetornoWS.DadosRet.TituloRet.Modalidade    := AJson.Values['numeroVariacaoCarteira'].AsInteger;
            ARetornoWS.DadosRet.TituloRet.CodigoCliente := AJson.Values['codigoCliente'].AsNumber;
            ARetornoWS.DadosRet.TituloRet.Contrato      := AJson.Values['numeroContratoCobranca'].AsString;

            AJSonObject                              := AJson.Values['qrCode'].AsObject;
            {QRCodeRet.url                            := AJSonObject.Values['url'].AsString;
            QRCodeRet.txId                           := AJSonObject.Values['txId'].AsString;
            QRCodeRet.emv                            := AJSonObject.Values['emv'].AsString;}

            ARetornoWS.DadosRet.TituloRet.UrlPix        := AJSonObject.Values['url'].AsString;
            ARetornoWS.DadosRet.TituloRet.TxId          := AJSonObject.Values['txId'].AsString;
            ARetornoWS.DadosRet.TituloRet.EMV           := AJSonObject.Values['emv'].AsString;

          end else
          if (TipoOperacao = tpConsultaDetalhe) then
          begin
            ARetornoWS.DadosRet.IDBoleto.CodBarras      := AJson.Values['textoCodigoBarrasTituloCobranca'].AsString;
            ARetornoWS.DadosRet.IDBoleto.LinhaDig       := AJson.Values['codigoLinhaDigitavel'].AsString;
            ARetornoWS.DadosRet.IDBoleto.NossoNum       := AJson.Values['id'].AsString;

            ARetornoWS.DadosRet.TituloRet.CodBarras     := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig      := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
            ARetornoWS.DadosRet.TituloRet.NossoNumero   := ARetornoWS.DadosRet.IDBoleto.NossoNum;
            ARetornoWS.DadosRet.TituloRet.SeuNumero     := AJson.Values['numeroTituloCedenteCobranca'].AsString;
            ARetornoWS.DadosRet.TituloRet.Carteira      := AJson.Values['numeroCarteiraCobranca'].AsString;
            ARetornoWS.DadosRet.TituloRet.Modalidade    := AJson.Values['numeroVariacaoCarteiraCobranca'].AsInteger;
            ARetornoWS.DadosRet.TituloRet.Contrato      := AJson.Values['numeroContratoCobranca'].AsString;

            // Dados Adicionais

            ARetornoWS.DadosRet.TituloRet.NumeroDocumento            := AJson.Values['numeroTituloCedenteCobranca'].AsString;
            ARetornoWS.DadosRet.TituloRet.DataRegistro               := DateBBtoDateTime( AJson.Values['dataRegistroTituloCobranca'].AsString );
            ARetornoWS.DadosRet.TituloRet.Vencimento                 := DateBBtoDateTime( AJson.Values['dataVencimentoTituloCobranca'].AsString );
            ARetornoWS.DadosRet.TituloRet.ValorDocumento             := AJson.Values['valorOriginalTituloCobranca'].AsNumber;
            ARetornoWS.DadosRet.TituloRet.Carteira                   := AJson.Values['numeroCarteiraCobranca'].AsString;
            ARetornoWS.DadosRet.TituloRet.Modalidade                 := StrToIntDef( AJson.Values['numeroVariacaoCarteiraCobranca'].asString ,0 );
            ARetornoWS.DadosRet.TituloRet.codigoEstadoTituloCobranca := AJson.Values['codigoEstadoTituloCobranca'].AsString;
            ARetornoWS.DadosRet.TituloRet.CodigoCanalTituloCobranca  := AJson.Values['codigoCanalPagamento'].AsString;
            ARetornoWS.DadosRet.TituloRet.contrato                   := AJson.Values['numeroContratoCobranca'].AsString;
            ARetornoWS.DadosRet.TituloRet.DataMovimento              := DateBBtoDateTime(AJson.Values['dataRegistroTituloCobranca'].AsString);
            ARetornoWS.DadosRet.TituloRet.Vencimento                 := DateBBtoDateTime(AJson.Values['dataVencimentoTituloCobranca'].AsString);
            ARetornoWS.DadosRet.TituloRet.DataDocumento              := DateBBtoDateTime(AJson.Values['dataEmissaoTituloCobranca'].AsString);
            ARetornoWS.DadosRet.TituloRet.DataCredito                := DateBBtoDateTime(AJson.Values['dataCreditoLiquidacao'].AsString);
            ARetornoWS.DadosRet.TituloRet.DataBaixa                  := DateBBtoDateTime(AJson.Values['dataRecebimentoTitulo'].AsString);
            ARetornoWS.DadosRet.TituloRet.ValorAtual                 := AJson.Values['valorAtualTituloCobranca'].AsNumber;
            ARetornoWS.DadosRet.TituloRet.ValorPago                  := AJson.Values['valorPagoSacado'].AsNumber;

            ARetornoWS.DadosRet.TituloRet.ValorRecebido              := AJson.Values['valorCreditoCedente'].AsNumber;
            ARetornoWS.DadosRet.TituloRet.ValorMoraJuros             := AJson.Values['valorJuroMoraRecebido'].AsNumber;
            ARetornoWS.DadosRet.TituloRet.ValorDesconto              := AJson.Values['valorDescontoUtilizado'].AsNumber;
            ARetornoWS.DadosRet.TituloRet.ValorOutrosCreditos        := AJson.Values['valorOutroRecebido'].AsNumber;
            ARetornoWS.DadosRet.TituloRet.ValorIOF                   := AJson.Values['valorImpostoSobreOprFinanceirasRecebidoTitulo'].AsNumber;
            ARetornoWS.DadosRet.TituloRet.ValorAbatimento            := AJson.Values['valorAbatimentoTotal'].AsNumber;
            ARetornoWS.DadosRet.TituloRet.MultaValorFixo             := true;
            ARetornoWS.DadosRet.TituloRet.PercentualMulta            := AJson.Values['valorMultaRecebido'].AsNumber;
            //ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := IntToStr(AJson.Values['codigoTipoBaixaTitulo'].AsInteger);
            case AJson.Values['codigoTipoBaixaTitulo'].AsInteger of
              1  : ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := 'BAIXADO POR SOLICITACAO';
              2  : ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := 'ENTREGA FRANCO PAGAMENTO';
              9  : ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := 'COMANDADA BANCO';
              10 : ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := 'COMANDADA CLIENTE - ARQUIVO';
              11 : ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := 'COMANDADA CLIENTE - ON-LINE';
              12 : ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := 'DECURSO PRAZO - CLIENTE';
              13 : ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := 'DECURSO PRAZO - BANCO';
              15 : ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := 'PROTESTADO';
              31 : ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := 'LIQUIDADO ANTERIORMENTE';
              32 : ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := 'HABILITADO EM PROCESSO';
              35 : ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := 'TRANSFERIDO PARA PERDAS';
              51 : ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := 'REGISTRADO INDEVIDAMENTE';
              90 : ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := 'BAIXA AUTOMATICA';
            end;

            ARetornoWS.DadosRet.TituloRet.CodigoOcorrenciaCartorio   := IntToStr(AJson.Values['codigoOcorrenciaCartorio'].AsInteger);

            ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado          := AJson.Values['nomeSacadoCobranca'].AsString;
            ARetornoWS.DadosRet.TituloRet.Sacado.Logradouro          := AJson.Values['textoEnderecoSacadoCobranca'].AsString;
            ARetornoWS.DadosRet.TituloRet.Sacado.Bairro              := AJson.Values['nomeBairroSacadoCobranca'].AsString;
            ARetornoWS.DadosRet.TituloRet.Sacado.Cidade              := AJson.Values['nomeMunicipioSacadoCobranca'].AsString;


          end else
          if (TipoOperacao = tpBaixa) then
          begin
            ARetornoWS.DadosRet.TituloRet.contrato      := AJson.Values['numeroContratoCobranca'].AsString;
            ARetornoWS.DadosRet.TituloRet.DataBaixa     := DateBBtoDateTime( AJson.Values['dataBaixa'].AsString);
            ARetornoWS.DadosRet.TituloRet.HoraBaixa     := AJson.Values['horarioBaixa'].AsString;

          end else
          if (TipoOperacao = tpAltera) then
          begin
            ARetornoWS.DadosRet.TituloRet.contrato      := AJson.Values['numeroContratoCobranca'].AsString;
            ARetornoWS.DadosRet.Comprovante.Data        := DateBBtoDateTime( AJson.Values['dataAtualizacao'].AsString);
            ARetornoWS.DadosRet.Comprovante.Hora        := AJson.Values['horarioAtualizacao'].AsString;

          end else
          if (TipoOperacao = tpPIXCriar) or (TipoOperacao = tpPIXCancelar) then
          begin;
            {QRCodeRet.url  := AJson.Values['qrCode.url'].AsString;
            QRCodeRet.txId := AJson.Values['qrCode.txId'].AsString;
            QRCodeRet.emv  := AJson.Values['qrCode.emv'].AsString;}

            ARetornoWS.DadosRet.TituloRet.UrlPix        := AJson.Values['qrCode.url'].AsString;
            ARetornoWS.DadosRet.TituloRet.TxId          := AJson.Values['qrCode.txId'].AsString;
            ARetornoWS.DadosRet.TituloRet.EMV           := AJson.Values['qrCode.emv'].AsString;
          end else
          if (TipoOperacao = tpPIXConsultar) then
          begin;
            ARetornoWS.DadosRet.IDBoleto.NossoNum         := AJson.Values['id'].AsString;
            ARetornoWS.DadosRet.TituloRet.NossoNumero     := ARetornoWS.DadosRet.IDBoleto.NossoNum;
            ARetornoWS.DadosRet.TituloRet.ValorDocumento  := AJson.Values['valorOriginalTituloCobranca'].AsNumber;
            ARetornoWS.DadosRet.TituloRet.DataRegistro    := DateBBtoDateTime( AJson.Values['dataRegistroTituloCobranca'].AsString );

            {QRCodeRet.url  := AJson.Values['qrCode.url'].AsString;
            QRCodeRet.txId := AJson.Values['qrCode.txId'].AsString;
            QRCodeRet.emv  := AJson.Values['qrCode.emv'].AsString;}

            ARetornoWS.DadosRet.TituloRet.UrlPix        := AJson.Values['qrCode.url'].AsString;
            ARetornoWS.DadosRet.TituloRet.TxId          := AJson.Values['qrCode.txId'].AsString;
            ARetornoWS.DadosRet.TituloRet.EMV           := AJson.Values['qrCode.emv'].AsString;
          end;
        end;

      finally
        AJson.free;
      end;

    except
      Result := False;
    end;

  end
  else
  begin
    case TipoOperacao of
      tpInclui,
      tpBaixa,
      tpAltera,
      tpConsulta,
      tpConsultaDetalhe,
      tpPIXCriar,
      tpPIXCancelar:
        begin
          case HTTPResultCode of
            404 :
              begin
                ARejeicao            := ARetornoWS.CriarRejeicaoLista;
                ARejeicao.Codigo     := '404';
                ARejeicao.Mensagem   := 'NÃO ENCONTRADO. O servidor não conseguiu encontrar o recurso solicitado.';
              end;
            503 :
              begin
                ARejeicao            := ARetornoWS.CriarRejeicaoLista;
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

function TRetornoEnvio_BancoBrasil_API.LerListaRetorno: Boolean;
var
  ListaRetorno: TACBrBoletoRetornoWS;
  AJson: TJson;
  AJSonRejeicao, AJSonObject: TJsonObject;
  ARejeicao: TACBrBoletoRejeicao;
  AJSonResp, AJsonBoletos: TJsonArray;
  I: Integer;
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

        ListaRetorno.JSON           := AJson.Stringify;
		

        //retorna quando houver erro
        case HTTPResultCode of
          400,
          403,
          500,
          503 :
            begin
              AJSonResp := AJson.Values['erros'].AsArray;
              for I := 0 to Pred(AJSonResp.Count) do
              begin
                AJSonRejeicao        := AJSonResp[I].AsObject;
                ARejeicao            := ListaRetorno.CriarRejeicaoLista;
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
                ARejeicao            := ListaRetorno.CriarRejeicaoLista;
                ARejeicao.Codigo     := AJson.Values['statusCode'].AsString;
                ARejeicao.Versao     := AJson.Values['error'].AsString;
                ARejeicao.Mensagem   := AJson.Values['message'].AsString;
              end;
            end;

        end;

        //retorna quando tiver sucesso
        if (ListaRetorno.ListaRejeicao.Count = 0) then
        begin
          AJsonBoletos := AJson.Values['boletos'].AsArray;
          for I := 0 to Pred(AJsonBoletos.Count) do
          begin
            if I > 0 then
              ListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;

            AJSonObject  := AJsonBoletos[I].AsObject;

            ListaRetorno.indicadorContinuidade := AJson.Values['indicadorContinuidade'].AsString = 'S';
            ListaRetorno.proximoIndice         := AJson.Values['proximoIndice'].AsInteger;

            ListaRetorno.DadosRet.IDBoleto.CodBarras      := '';
            ListaRetorno.DadosRet.IDBoleto.LinhaDig       := '';
            ListaRetorno.DadosRet.IDBoleto.NossoNum       := AJSonObject.Values['numeroBoletoBB'].AsString;

            ListaRetorno.DadosRet.TituloRet.CodBarras      := ListaRetorno.DadosRet.IDBoleto.CodBarras;
            ListaRetorno.DadosRet.TituloRet.LinhaDig       := ListaRetorno.DadosRet.IDBoleto.LinhaDig;


            ListaRetorno.DadosRet.TituloRet.NossoNumero                := ListaRetorno.DadosRet.IDBoleto.NossoNum;
            ListaRetorno.DadosRet.TituloRet.DataRegistro               := DateBBtoDateTime(AJSonObject.Values['dataRegistro'].AsString);
            ListaRetorno.DadosRet.TituloRet.Vencimento                 := DateBBtoDateTime(AJSonObject.Values['dataVencimento'].AsString);
            ListaRetorno.DadosRet.TituloRet.ValorDocumento             := AJSonObject.Values['valorOriginal'].AsNumber;
            ListaRetorno.DadosRet.TituloRet.Carteira                   := OnlyNumber(AJSonObject.Values['carteiraConvenio'].AsString);
            ListaRetorno.DadosRet.TituloRet.Modalidade                 := AJSonObject.Values['variacaoCarteiraConvenio'].AsInteger;
            ListaRetorno.DadosRet.TituloRet.codigoEstadoTituloCobranca := OnlyNumber(AJSonObject.Values['codigoEstadoTituloCobranca'].AsString);
            ListaRetorno.DadosRet.TituloRet.estadoTituloCobranca       := AJSonObject.Values['estadoTituloCobranca'].AsString;
            ListaRetorno.DadosRet.TituloRet.contrato                   := AJSonObject.Values['contrato'].AsString;
            ListaRetorno.DadosRet.TituloRet.DataMovimento              := DateBBtoDateTime(AJSonObject.Values['dataMovimento'].AsString);
            ListaRetorno.DadosRet.TituloRet.dataCredito                := DateBBtoDateTime(AJSonObject.Values['dataCredito'].AsString);
            ListaRetorno.DadosRet.TituloRet.ValorAtual                 := AJSonObject.Values['valorAtual'].AsNumber;
            ListaRetorno.DadosRet.TituloRet.ValorPago                  := AJSonObject.Values['valorPago'].AsNumber;

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
    case HTTPResultCode of
      404 :
        begin
          ARejeicao            := ListaRetorno.CriarRejeicaoLista;
          ARejeicao.Codigo     := '404';
          ARejeicao.Mensagem   := 'NÃO ENCONTRADO. O servidor não conseguiu encontrar o recurso solicitado.';
        end;
      503 :
        begin
          ARejeicao            := ListaRetorno.CriarRejeicaoLista;
          ARejeicao.Codigo     := '503';
          ARejeicao.Versao     := 'ERRO INTERNO BB';
          ARejeicao.Mensagem   := 'SERVIÇO INDISPONÍVEL. O servidor está impossibilitado de lidar com a requisição no momento. Tente mais tarde.';
          ARejeicao.Ocorrencia := 'ERRO INTERNO nos servidores do Banco do Brasil.';
        end;
    end;
  end;

end;

function TRetornoEnvio_BancoBrasil_API.RetornoEnvio(const AIndex: Integer): Boolean;
begin

  Result:=inherited RetornoEnvio(AIndex);

end;

end.

