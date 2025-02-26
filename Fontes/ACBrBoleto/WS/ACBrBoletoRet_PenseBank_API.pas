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
  ACBrBoleto,
  ACBrBoletoWS,
  ACBrBoletoRetorno,
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
  SysUtils,
  StrUtils,
  ACBrJSON,
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
  LJsonObject, LItemObject, LJsonViolacao: TACBrJSONObject;
  LJsonArray, LJsonViolacoes: TACBrJSONArray;
  LMensagemRejeicao: TACBrBoletoRejeicao;
  LTipoOperacao : TOperacao;
  X: Integer;
begin
  Result := True;

  LTipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;

  ARetornoWS.HTTPResultCode  := HTTPResultCode;
  ARetornoWS.JSONEnvio       := EnvWs;
  ARetornoWS.Header.Operacao := LTipoOperacao;

  if RetWS <> '' then
  begin
    try
      if Copy(Trim(RetWS),0,5) = 'ERRO:' then
        RetWS := Copy(Trim(RetWS),6,Length(RetWS));

      LJsonObject := TACBrJSONObject.Parse(RetWS);
      ARetornoWS.MsgRetorno := RetWS;
      try

        ARetornoWS.JSON := LJsonObject.ToJSON;
        if HTTPResultCode >= 400 then
        begin
          LMensagemRejeicao            := ARetornoWS.CriarRejeicaoLista;
          LMensagemRejeicao.Codigo     := IntToStr(HTTPResultCode);
          LMensagemRejeicao.Mensagem   := LJsonObject.AsString['message'];

          if LMensagemRejeicao.Mensagem = '' then
          begin
            ARetornoWS.ListaRejeicao.Clear;

            LJsonViolacoes := LJsonObject.AsJSONArray['erros'];

            if LJsonViolacoes.Count > 0 then
            begin
              for X := 0 to LJsonViolacoes.Count - 1 do
              begin
                LJsonViolacao := LJsonViolacoes.ItemAsJSONObject[X];

                LMensagemRejeicao          := ARetornoWS.CriarRejeicaoLista;
                LMensagemRejeicao.Codigo   := LJsonViolacao.AsString['codigo'];
                LMensagemRejeicao.Mensagem := LJsonViolacao.AsString['mensagem'];
              end;
            end
            else
            begin
              LJsonViolacoes := LJsonObject.AsJSONArray['errors'];
              for X := 0 to LJsonViolacoes.Count - 1 do
              begin
                LJsonViolacao              := LJsonViolacoes.ItemAsJSONObject[X];
                LMensagemRejeicao          := ARetornoWS.CriarRejeicaoLista;
                LMensagemRejeicao.Codigo   := LJsonViolacao.AsString['code'];
                LMensagemRejeicao.Mensagem := LJsonViolacao.AsString['message'];
              end;

               if (LMensagemRejeicao.Mensagem = '') and (LMensagemRejeicao.Codigo = '') then
               begin
                 LMensagemRejeicao          := ARetornoWS.CriarRejeicaoLista;
                 LMensagemRejeicao.Codigo   := LJsonObject.AsString['statusCode'];
                 LMensagemRejeicao.Mensagem := LJsonObject.AsString['error'];
               end

            end;
          end;
        end;

        //retorna quando tiver sucesso
        if (ARetornoWS.ListaRejeicao.Count = 0) then
        begin
          if (LTipoOperacao = tpInclui) then
          begin
            LItemObject := LJsonObject.AsJSONObject['message'];
            ARetornoWS.DadosRet.IDBoleto.CodBarras      := LItemObject.AsString['codigoBarraNumerico'];
            ARetornoWS.DadosRet.IDBoleto.LinhaDig       := LItemObject.AsString['linhaDigitavel'];
            ARetornoWS.DadosRet.IDBoleto.NossoNum       := RightStr(LItemObject.AsString['numeroTituloCliente'], 10);

            ARetornoWS.DadosRet.IDBoleto.IDBoleto       := IntToStr(LItemObject.AsInteger['idboleto']);
            ARetornoWS.DadosRet.IDBoleto.LinhaDig       := LItemObject.AsString['linhaDigitavel'];
            ARetornoWS.DadosRet.IDBoleto.URL            := LItemObject.AsString['url_boleto'];
            ARetornoWS.DadosRet.IDBoleto.URLPDF         := LItemObject.AsString['url_pdf'];

            ARetornoWS.DadosRet.TituloRet.TxId          := LItemObject.AsString['pixHash'];
            ARetornoWS.DadosRet.TituloRet.EMV           := LItemObject.AsString['pixQrCode'];

            ARetornoWS.DadosRet.TituloRet.CodBarras     := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig      := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
            ARetornoWS.DadosRet.TituloRet.NossoNumero   := ARetornoWS.DadosRet.IDBoleto.NossoNum;
            ARetornoWS.DadosRet.TituloRet.SeuNumero     := LItemObject.AsString['idexterno'];

          end
          else if (LTipoOperacao = tpConsultaDetalhe) then
            begin
              LItemObject := LJsonObject.AsJSONObject['message'];
              ARetornoWS.DadosRet.IDBoleto.IDBoleto              := IntToStr(LItemObject.AsInteger['idboleto']);
              ARetornoWS.DadosRet.IDBoleto.CodBarras             := LItemObject.AsString['codigoBarraNumerico'];
              ARetornoWS.DadosRet.IDBoleto.LinhaDig              := LItemObject.AsString['codigoLinhaDigitavel'];
              ARetornoWS.DadosRet.IDBoleto.NossoNum              := RightStr(LItemObject.AsString['numeroTituloCliente'], 10);
              ARetornoWS.DadosRet.TituloRet.NumeroDocumento      := LItemObject.AsString['numeroTituloBeneficiario'];
              ARetornoWS.DadosRet.TituloRet.ValorDocumento       := LItemObject.AsCurrency['valorOriginalTituloCobranca'];
              ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := LItemObject.AsString['situacaoEstadoTituloCobranca'];
              ARetornoWS.DadosRet.TituloRet.ValorPago            := LItemObject.AsCurrency['valorPagoSacado'];
              ARetornoWS.DadosRet.TituloRet.ValorMoraJuros       := LItemObject.AsCurrency['valorJuroMoraRecebido'];
              ARetornoWS.DadosRet.TituloRet.CodigoMulta          := CmPercentual;
              ARetornoWS.DadosRet.TituloRet.DataMulta            := StrToDateDef(LItemObject.AsString['dataMultaTitulo'], 0);
              ARetornoWS.DadosRet.TituloRet.ValorMulta           := LItemObject.AsCurrency['valorMultaRecebido'];
              ARetornoWS.DadosRet.TituloRet.PercentualMulta      := LItemObject.AsFloat['percentualMultaTitulo'];
              ARetornoWS.DadosRet.TituloRet.ValorDesconto        := LItemObject.AsCurrency['valorDescontoUtilizado'];
              ARetornoWS.DadosRet.TituloRet.DataBaixa            := StrToDateDef(LItemObject.AsString['dataRecebimentoTitulo'], 0);
              ARetornoWS.DadosRet.IDBoleto.IDBoleto              := IntToStr(LItemObject.AsInteger['idboleto']);
              ARetornoWS.DadosRet.IDBoleto.URL                   := LItemObject.AsString['url_boleto'];
              ARetornoWS.DadosRet.TituloRet.TxId                 := LItemObject.AsString['pix_hash'];
              ARetornoWS.DadosRet.TituloRet.SeuNumero            := LItemObject.AsString['idExterno'];
              ARetornoWS.indicadorContinuidade                   := false;
              ARetornoWS.DadosRet.TituloRet.CodBarras            := ARetornoWS.DadosRet.IDBoleto.CodBarras;
              ARetornoWS.DadosRet.TituloRet.LinhaDig             := ARetornoWS.DadosRet.IDBoleto.LinhaDig;

              ARetornoWS.DadosRet.TituloRet.NossoNumero          := ARetornoWS.DadosRet.IDBoleto.NossoNum;
              ARetornoWS.DadosRet.TituloRet.Vencimento           := StrToDate(LItemObject.AsString['dataVencimentoTituloCobranca']);
              ARetornoWS.DadosRet.TituloRet.ValorDocumento       := LItemObject.AsFloat['valorOriginalTituloCobranca'];
              ARetornoWS.DadosRet.TituloRet.ValorAtual           := LItemObject.AsFloat['valorOriginalTituloCobranca'];

            end
          else if (LTipoOperacao in [TpConsulta]) then
            begin

              LItemObject := LJsonObject.AsJSONObject['message'];
              ARetornoWS.DadosRet.IDBoleto.IDBoleto              := IntToStr(LItemObject.AsInteger['idboleto']);
              ARetornoWS.DadosRet.IDBoleto.CodBarras             := LItemObject.AsString['codigoBarraNumerico'];
              ARetornoWS.DadosRet.IDBoleto.LinhaDig              := LItemObject.AsString['codigoLinhaDigitavel'];
              ARetornoWS.DadosRet.IDBoleto.NossoNum              := RightStr(LItemObject.AsString['numeroTituloCliente'], 10);
              ARetornoWS.DadosRet.TituloRet.NumeroDocumento      := LItemObject.AsString['numeroTituloBeneficiario'];
              ARetornoWS.DadosRet.TituloRet.ValorDocumento       := LItemObject.AsCurrency['valorOriginalTituloCobranca'];
              ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := LItemObject.AsString['situacaoEstadoTituloCobranca'];
              ARetornoWS.DadosRet.TituloRet.ValorPago            := LItemObject.AsCurrency['valorPagoSacado'];
              ARetornoWS.DadosRet.TituloRet.ValorMoraJuros       := LItemObject.AsCurrency['valorJuroMoraRecebido'];
              ARetornoWS.DadosRet.TituloRet.CodigoMulta          := CmPercentual;
              ARetornoWS.DadosRet.TituloRet.DataMulta            := StrToDateDef(LItemObject.AsString['dataMultaTitulo'], 0);
              ARetornoWS.DadosRet.TituloRet.ValorMulta           := LItemObject.AsCurrency['valorMultaRecebido'];
              ARetornoWS.DadosRet.TituloRet.PercentualMulta      := LItemObject.AsFloat['percentualMultaTitulo'];
              ARetornoWS.DadosRet.TituloRet.ValorDesconto        := LItemObject.AsCurrency['valorDescontoUtilizado'];
              ARetornoWS.DadosRet.TituloRet.DataBaixa            := StrToDateDef(LItemObject.AsString['dataRecebimentoTitulo'], 0);
              ARetornoWS.DadosRet.IDBoleto.IDBoleto              := IntToStr(LItemObject.AsInteger['idboleto']);
              ARetornoWS.DadosRet.IDBoleto.URL                   := LItemObject.AsString['url_boleto'];
              ARetornoWS.DadosRet.TituloRet.TxId                 := LItemObject.AsString['pix_hash'];
              ARetornoWS.DadosRet.TituloRet.SeuNumero            := LItemObject.AsString['idExterno'];
              ARetornoWS.indicadorContinuidade                   := false;
              ARetornoWS.DadosRet.TituloRet.CodBarras            := ARetornoWS.DadosRet.IDBoleto.CodBarras;
              ARetornoWS.DadosRet.TituloRet.LinhaDig             := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
              ARetornoWS.DadosRet.TituloRet.NossoNumero          := ARetornoWS.DadosRet.IDBoleto.NossoNum;
              ARetornoWS.DadosRet.TituloRet.Vencimento           := StrToDate(LItemObject.AsString['dataVencimentoTituloCobranca']);
              ARetornoWS.DadosRet.TituloRet.ValorDocumento       := LItemObject.AsFloat['valorOriginalTituloCobranca'];
              ARetornoWS.DadosRet.TituloRet.ValorAtual           := LItemObject.AsFloat['valorOriginalTituloCobranca'];
            end
          else if (LTipoOperacao = TpBaixa) then
            begin
            // não possui dados de retorno..
            end
          else if (LTipoOperacao = TpAltera) then
            begin

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

function TRetornoEnvio_PenseBank_API.LerListaRetorno: Boolean;
var
  LListaRetorno: TACBrBoletoRetornoWS;
  LJsonObject, LItemObject, LJsonViolacao: TACBrJSONObject;
  LJsonArray, LJsonViolacoes: TACBrJSONArray;
  LMensagemRejeicao: TACBrBoletoRejeicao;
  I: Integer;
  LTipoOperacao: TOperacao;
  X: Integer;
begin
  Result := True;

  LListaRetorno                := ACBrBoleto.CriarRetornoWebNaLista;
  LListaRetorno.HTTPResultCode := HTTPResultCode;
  LListaRetorno.JSONEnvio      := EnvWs;

  if RetWS <> '' then
  begin
    LListaRetorno.JSON := RetWS;
    try
      if Copy(Trim(RetWS),0,5) = 'ERRO:' then
        RetWS := Copy(Trim(RetWS),6,Length(RetWS));

      LJsonObject := TACBrJSONObject.Parse(RetWS);
      try
        if (HTTPResultCode >= 400) and (HTTPResultCode <> 404) then
        begin
          LMensagemRejeicao            := LListaRetorno.CriarRejeicaoLista;
          LMensagemRejeicao.Codigo     := IntToStr(HTTPResultCode);
          LMensagemRejeicao.Mensagem   := LJsonObject.AsString['message'];

          if LMensagemRejeicao.Mensagem = '' then
          begin
            LListaRetorno.ListaRejeicao.Clear;

            LJsonViolacoes := LJsonObject.AsJSONArray['erros'];

            if LJsonViolacoes.Count > 0 then
            begin
              for X := 0 to LJsonViolacoes.Count - 1 do
              begin
                LJsonViolacao              := LJsonViolacoes.ItemAsJSONObject[X];
                LMensagemRejeicao          := LListaRetorno.CriarRejeicaoLista;
                LMensagemRejeicao.Codigo   := LJsonViolacao.AsString['codigo'];
                LMensagemRejeicao.Mensagem := LJsonViolacao.AsString['mensagem'];
              end;
            end
            else
            begin
              LJsonViolacoes := LJsonObject.AsJSONArray['erros'];
              for X := 0 to LJsonViolacoes.Count - 1 do
              begin
                LJsonViolacao              := LJsonViolacoes.ItemAsJSONObject[X];
                LMensagemRejeicao          := LListaRetorno.CriarRejeicaoLista;
                LMensagemRejeicao.Codigo   := LJsonViolacao.AsString['code'];
                LMensagemRejeicao.Mensagem := LJsonViolacao.AsString['message'];
              end;
            end;
          end;
        end;

        //retorna quando tiver sucesso
        if (LListaRetorno.ListaRejeicao.Count = 0) then
        begin
          LJsonArray := LJsonObject.AsJSONArray['boletos'];

          for I := 0 to Pred(LJsonArray.Count) do
          begin
            if I > 0 then
              LListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;

            LItemObject  := LJsonArray.ItemAsJSONObject[I];

            LListaRetorno.DadosRet.IDBoleto.IDBoleto        := IntToStr(LItemObject.AsInteger['idboleto']);
            LListaRetorno.DadosRet.IDBoleto.CodBarras       := '';
            LListaRetorno.DadosRet.IDBoleto.LinhaDig        := LItemObject.AsString['codigoLinhaDigitavel'];
            LListaRetorno.DadosRet.IDBoleto.NossoNum        := '';
            LListaRetorno.indicadorContinuidade             := false;
            LListaRetorno.DadosRet.TituloRet.CodBarras      := LListaRetorno.DadosRet.IDBoleto.CodBarras;
            LListaRetorno.DadosRet.TituloRet.LinhaDig       := LListaRetorno.DadosRet.IDBoleto.LinhaDig;
            LListaRetorno.DadosRet.TituloRet.NossoNumero    := LListaRetorno.DadosRet.IDBoleto.NossoNum;
            LListaRetorno.DadosRet.TituloRet.Vencimento     := StrToDate(LItemObject.AsString['dataVencimentoTituloCobranca']);
            LListaRetorno.DadosRet.TituloRet.ValorDocumento := LItemObject.AsFloat['valorOriginalTituloCobranca'];
            LListaRetorno.DadosRet.TituloRet.ValorAtual     := LItemObject.AsFloat['valorOriginalTituloCobranca'];
            if( LItemObject.AsString['situacaoEstadoTituloCobranca'] = C_LIQUIDADO ) or
               ( LItemObject.AsString['situacaoEstadoTituloCobranca'] = C_BAIXADO_POS_SOLICITACAO ) then
              LListaRetorno.DadosRet.TituloRet.ValorPago                  := LItemObject.AsFloat['valor'];
          end;
        end;
      except
        Result := False;
      end;
    finally
      LJsonObject.free;
    end;
  end else
  begin
    case HTTPResultCode of
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
          LMensagemRejeicao.Versao     := 'ERRO INTERNO BB';
          LMensagemRejeicao.Mensagem   := 'SERVIÇO INDISPONÍVEL. O servidor está impossibilitado de lidar com a requisição no momento. Tente mais tarde.';
          LMensagemRejeicao.Ocorrencia := 'ERRO INTERNO nos servidores do Banco.';
        end;
    end;
  end;
end;

function TRetornoEnvio_PenseBank_API.RetornoEnvio(const AIndex: Integer): Boolean;
begin
//  Result:=inherited RetornoEnvio(AIndex);
  if (ACBrBoleto.ListadeBoletos.Count > 0) then
  begin
    Result := LerRetorno(ACBrBoleto.ListadeBoletos[AIndex].RetornoWeb);
    ACBrBoleto.ListadeBoletos[AIndex].QrCode; // GetQRCode valida campos no titulo
  end
  else
    Result := LerListaRetorno;
end;

end.

