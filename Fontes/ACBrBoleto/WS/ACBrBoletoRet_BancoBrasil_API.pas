{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  José M S Junior, Victor Hugo Gonzales - Pandaaa}
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
  SysUtils,
  ACBrBoleto,
  ACBrBoletoWS,
  ACBrBoletoRetorno,
  ACBrBoletoWS.Rest;
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
  ACBrBoletoConversao,
  ACBrUtil.Strings,
  ACBrUtil.Base,
  ACBrJSON;

{ TRetornoEnvio }

constructor TRetornoEnvio_BancoBrasil_API.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);
end;

function TRetornoEnvio_BancoBrasil_API.DateBBtoDateTime(const AValue: String): TDateTime;
begin
  Result := StrToDateDef( StringReplace( AValue,'.','/', [rfReplaceAll] ),0);
end;

destructor TRetornoEnvio_BancoBrasil_API.Destroy;
begin
  inherited Destroy;
end;

function TRetornoEnvio_BancoBrasil_API.LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;
var
  LJsonObject, LItemObject: TACBrJSONObject;
  LJsonArray: TACBrJSONArray;
  LMensagemRejeicao: TACBrBoletoRejeicao;
  LTipoOperacao : TOperacao;
  I: Integer;
begin
  Result := True;

  LTipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;

  ARetornoWS.HTTPResultCode  := HTTPResultCode;
  ARetornoWS.JSONEnvio       := EnvWs;
  ARetornoWS.Header.Operacao := LTipoOperacao;

  if RetWS <> '' then
  begin
    try
      LJsonObject := TACBrJSONObject.Parse(RetWS);
      try
        ARetornoWS.JSON := LJsonObject.ToJSON;

        if HTTPResultCode >= 400 then
        begin
          LJsonArray := LJsonObject.AsJSONArray['erros'];

          if LJsonArray.Count > 0 then
          begin
            for I := 0 to Pred(LJsonArray.Count) do
            begin
              LItemObject        := LJsonArray.ItemAsJSONObject[I];
              LMensagemRejeicao            := ARetornoWS.CriarRejeicaoLista;

              if NaoEstaVazio(LItemObject.AsString['codigo']) or
                 NaoEstaVazio(LItemObject.AsString['mensagem']) or
                 NaoEstaVazio(LItemObject.AsString['ocorrencia']) then
              begin
                LMensagemRejeicao.Codigo     := LItemObject.AsString['codigo'];
                LMensagemRejeicao.Versao     := LItemObject.AsString['versao'];
                LMensagemRejeicao.Mensagem   := LItemObject.AsString['mensagem'];
                LMensagemRejeicao.Ocorrencia := LItemObject.AsString['ocorrencia'];
              end
              else
              begin
                LMensagemRejeicao.Codigo     := LItemObject.AsString['codigoMensagem'];
                LMensagemRejeicao.Versao     := LItemObject.AsString['versaoMensagem'];
                LMensagemRejeicao.Mensagem   := LItemObject.AsString['textoMensagem'];
              end;
            end;
          end
          else if LJsonObject.AsString['error'] <> '' then
          begin
            LMensagemRejeicao            := ARetornoWS.CriarRejeicaoLista;
            LMensagemRejeicao.Codigo     := LJsonObject.AsString['statusCode'];
            LMensagemRejeicao.Versao     := LJsonObject.AsString['error'];
            LMensagemRejeicao.Mensagem   := LJsonObject.AsString['message'];
          end;
        end;

        //retorna quando tiver sucesso
        if (ARetornoWS.ListaRejeicao.Count = 0) then
        begin
          if (LTipoOperacao = tpInclui) then
          begin

            ARetornoWS.DadosRet.IDBoleto.CodBarras      := LJsonObject.AsString['codigoBarraNumerico'];
            ARetornoWS.DadosRet.IDBoleto.LinhaDig       := LJsonObject.AsString['linhaDigitavel'];
            ARetornoWS.DadosRet.IDBoleto.NossoNum       := LJsonObject.AsString['numero'];

            ARetornoWS.DadosRet.TituloRet.CodBarras     := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig      := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
            ARetornoWS.DadosRet.TituloRet.NossoNumero   := ARetornoWS.DadosRet.IDBoleto.NossoNum;
            ARetornoWS.DadosRet.TituloRet.Carteira      := LJsonObject.AsString['numeroCarteira'];
            ARetornoWS.DadosRet.TituloRet.Modalidade    := LJsonObject.AsInteger['numeroVariacaoCarteira'];
            ARetornoWS.DadosRet.TituloRet.CodigoCliente := LJsonObject.AsFloat['codigoCliente'];
            ARetornoWS.DadosRet.TituloRet.Contrato      := LJsonObject.AsString['numeroContratoCobranca'];

            LItemObject                                 := LJsonObject.AsJSONObject['qrCode'];

            ARetornoWS.DadosRet.TituloRet.UrlPix        := LItemObject.AsString['url'];
            ARetornoWS.DadosRet.TituloRet.TxId          := LItemObject.AsString['txId'];
            ARetornoWS.DadosRet.TituloRet.EMV           := LItemObject.AsString['emv'];

          end else
          if (LTipoOperacao = tpConsultaDetalhe) then
          begin
            ARetornoWS.DadosRet.IDBoleto.CodBarras      := LJsonObject.AsString['textoCodigoBarrasTituloCobranca'];
            ARetornoWS.DadosRet.IDBoleto.LinhaDig       := LJsonObject.AsString['codigoLinhaDigitavel'];
            ARetornoWS.DadosRet.IDBoleto.NossoNum       := LJsonObject.AsString['id'];

            ARetornoWS.DadosRet.TituloRet.CodBarras     := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig      := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
            ARetornoWS.DadosRet.TituloRet.NossoNumero   := ARetornoWS.DadosRet.IDBoleto.NossoNum;
            ARetornoWS.DadosRet.TituloRet.SeuNumero     := LJsonObject.AsString['numeroTituloCedenteCobranca'];
            ARetornoWS.DadosRet.TituloRet.Carteira      := LJsonObject.AsString['numeroCarteiraCobranca'];
            ARetornoWS.DadosRet.TituloRet.Modalidade    := LJsonObject.AsInteger['numeroVariacaoCarteiraCobranca'];
            ARetornoWS.DadosRet.TituloRet.Contrato      := LJsonObject.AsString['numeroContratoCobranca'];

            // Dados Adicionais

            ARetornoWS.DadosRet.TituloRet.NumeroDocumento            := LJsonObject.AsString['numeroTituloCedenteCobranca'];
            ARetornoWS.DadosRet.TituloRet.DataRegistro               := DateBBtoDateTime( LJsonObject.AsString['dataRegistroTituloCobranca'] );
            ARetornoWS.DadosRet.TituloRet.Vencimento                 := DateBBtoDateTime( LJsonObject.AsString['dataVencimentoTituloCobranca'] );
            ARetornoWS.DadosRet.TituloRet.ValorDocumento             := LJsonObject.AsFloat['valorOriginalTituloCobranca'];
            ARetornoWS.DadosRet.TituloRet.Carteira                   := LJsonObject.AsString['numeroCarteiraCobranca'];
            ARetornoWS.DadosRet.TituloRet.Modalidade                 := StrToIntDef( LJsonObject.AsString['numeroVariacaoCarteiraCobranca'] ,0 );
            ARetornoWS.DadosRet.TituloRet.codigoEstadoTituloCobranca := LJsonObject.AsString['codigoEstadoTituloCobranca'];
            ARetornoWS.DadosRet.TituloRet.CodigoCanalTituloCobranca  := LJsonObject.AsString['codigoCanalPagamento'];
            ARetornoWS.DadosRet.TituloRet.contrato                   := LJsonObject.AsString['numeroContratoCobranca'];
            ARetornoWS.DadosRet.TituloRet.DataMovimento              := DateBBtoDateTime(LJsonObject.AsString['dataRegistroTituloCobranca']);
            ARetornoWS.DadosRet.TituloRet.Vencimento                 := DateBBtoDateTime(LJsonObject.AsString['dataVencimentoTituloCobranca']);
            ARetornoWS.DadosRet.TituloRet.DataDocumento              := DateBBtoDateTime(LJsonObject.AsString['dataEmissaoTituloCobranca']);
            ARetornoWS.DadosRet.TituloRet.DataCredito                := DateBBtoDateTime(LJsonObject.AsString['dataCreditoLiquidacao']);
            ARetornoWS.DadosRet.TituloRet.DataBaixa                  := DateBBtoDateTime(LJsonObject.AsString['dataRecebimentoTitulo']);
            ARetornoWS.DadosRet.TituloRet.ValorAtual                 := LJsonObject.AsFloat['valorAtualTituloCobranca'];
            ARetornoWS.DadosRet.TituloRet.ValorPago                  := LJsonObject.AsFloat['valorPagoSacado'];

            ARetornoWS.DadosRet.TituloRet.ValorRecebido              := LJsonObject.AsFloat['valorCreditoCedente'];
            ARetornoWS.DadosRet.TituloRet.ValorMoraJuros             := LJsonObject.AsFloat['valorJuroMoraRecebido'];
            ARetornoWS.DadosRet.TituloRet.ValorDesconto              := LJsonObject.AsFloat['valorDescontoUtilizado'];
            ARetornoWS.DadosRet.TituloRet.ValorOutrosCreditos        := LJsonObject.AsFloat['valorOutroRecebido'];
            ARetornoWS.DadosRet.TituloRet.ValorIOF                   := LJsonObject.AsFloat['valorImpostoSobreOprFinanceirasRecebidoTitulo'];
            ARetornoWS.DadosRet.TituloRet.ValorAbatimento            := LJsonObject.AsFloat['valorAbatimentoTotal'];
            ARetornoWS.DadosRet.TituloRet.MultaValorFixo             := true;
            ARetornoWS.DadosRet.TituloRet.PercentualMulta            := LJsonObject.AsFloat['valorMultaRecebido'];

            case LJsonObject.AsInteger['codigoTipoBaixaTitulo'] of
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

            ARetornoWS.DadosRet.TituloRet.CodigoOcorrenciaCartorio   := IntToStr(LJsonObject.AsInteger['codigoOcorrenciaCartorio']);

            ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado          := LJsonObject.AsString['nomeSacadoCobranca'];
            ARetornoWS.DadosRet.TituloRet.Sacado.Logradouro          := LJsonObject.AsString['textoEnderecoSacadoCobranca'];
            ARetornoWS.DadosRet.TituloRet.Sacado.Bairro              := LJsonObject.AsString['nomeBairroSacadoCobranca'];
            ARetornoWS.DadosRet.TituloRet.Sacado.Cidade              := LJsonObject.AsString['nomeMunicipioSacadoCobranca'];


          end else
          if (LTipoOperacao = tpBaixa) then
          begin
            ARetornoWS.DadosRet.TituloRet.contrato      := LJsonObject.AsString['numeroContratoCobranca'];
            ARetornoWS.DadosRet.TituloRet.DataBaixa     := DateBBtoDateTime( LJsonObject.AsString['dataBaixa']);
            ARetornoWS.DadosRet.TituloRet.HoraBaixa     := LJsonObject.AsString['horarioBaixa'];

          end else
          if (LTipoOperacao = tpAltera) then
          begin
            ARetornoWS.DadosRet.TituloRet.contrato      := LJsonObject.AsString['numeroContratoCobranca'];
            ARetornoWS.DadosRet.Comprovante.Data        := DateBBtoDateTime( LJsonObject.AsString['dataAtualizacao']);
            ARetornoWS.DadosRet.Comprovante.Hora        := LJsonObject.AsString['horarioAtualizacao'];

          end else
          if (LTipoOperacao = tpPIXCriar) or (LTipoOperacao = tpPIXCancelar) then
          begin
            ARetornoWS.DadosRet.TituloRet.UrlPix        := LJsonObject.AsString['qrCode.url'];
            ARetornoWS.DadosRet.TituloRet.TxId          := LJsonObject.AsString['qrCode.txId'];
            ARetornoWS.DadosRet.TituloRet.EMV           := LJsonObject.AsString['qrCode.emv'];
          end else
          if (LTipoOperacao = tpPIXConsultar) then
          begin;
            ARetornoWS.DadosRet.IDBoleto.NossoNum         := LJsonObject.AsString['id'];
            ARetornoWS.DadosRet.TituloRet.NossoNumero     := ARetornoWS.DadosRet.IDBoleto.NossoNum;
            ARetornoWS.DadosRet.TituloRet.ValorDocumento  := LJsonObject.AsFloat['valorOriginalTituloCobranca'];
            ARetornoWS.DadosRet.TituloRet.DataRegistro    := DateBBtoDateTime( LJsonObject.AsString['dataRegistroTituloCobranca'] );

            ARetornoWS.DadosRet.TituloRet.UrlPix        := LJsonObject.AsString['qrCode.url'];
            ARetornoWS.DadosRet.TituloRet.TxId          := LJsonObject.AsString['qrCode.txId'];
            ARetornoWS.DadosRet.TituloRet.EMV           := LJsonObject.AsString['qrCode.emv'];
          end;
        end;
      except
        Result := False;
      end;
    finally
      LJsonObject.free;
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
          LMensagemRejeicao.Versao     := 'ERRO INTERNO BB';
          LMensagemRejeicao.Mensagem   := 'SERVIÇO INDISPONÍVEL. O servidor está impossibilitado de lidar com a requisição no momento. Tente mais tarde.';
          LMensagemRejeicao.Ocorrencia := 'ERRO INTERNO nos servidores do Banco do Brasil.';
        end;
    end;
  end;
end;

function TRetornoEnvio_BancoBrasil_API.LerListaRetorno: Boolean;
var
  LJsonObject, LItemObject: TACBrJSONObject;
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

        if LJsonObject.IsJSONArray('erros') then
        begin
          LJsonArray := LJsonObject.AsJSONArray['erros'];
          for I := 0 to Pred(LJsonArray.Count) do
          begin
            LItemObject                  := LJsonArray.ItemAsJSONObject[I];
            LMensagemRejeicao            := LListaRetorno.CriarRejeicaoLista;
            LMensagemRejeicao.Codigo     := LItemObject.AsString['codigoMensagem'];
            LMensagemRejeicao.Versao     := LItemObject.AsString['versaoMensagem'];
            LMensagemRejeicao.Mensagem   := LItemObject.AsString['textoMensagem'];
            LMensagemRejeicao.Ocorrencia := LItemObject.AsString['codigoRetorno'];
          end;
        end;

        if NaoEstaVazio(LJsonObject.AsString['error']) then
        begin
          LMensagemRejeicao            := LListaRetorno.CriarRejeicaoLista;
          LMensagemRejeicao.Codigo     := LJsonObject.AsString['statusCode'];
          LMensagemRejeicao.Versao     := LJsonObject.AsString['error'];
          LMensagemRejeicao.Mensagem   := LJsonObject.AsString['message'];
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

            LListaRetorno.indicadorContinuidade := LJsonObject.AsString['indicadorContinuidade'] = 'S';
            LListaRetorno.proximoIndice         := LJsonObject.AsInteger['proximoIndice'];

            LListaRetorno.DadosRet.IDBoleto.CodBarras      := '';
            LListaRetorno.DadosRet.IDBoleto.LinhaDig       := '';
            LListaRetorno.DadosRet.IDBoleto.NossoNum       := LItemObject.AsString['numeroBoletoBB'];

            LListaRetorno.DadosRet.TituloRet.CodBarras      := LListaRetorno.DadosRet.IDBoleto.CodBarras;
            LListaRetorno.DadosRet.TituloRet.LinhaDig       := LListaRetorno.DadosRet.IDBoleto.LinhaDig;


            LListaRetorno.DadosRet.TituloRet.NossoNumero                := LListaRetorno.DadosRet.IDBoleto.NossoNum;
            LListaRetorno.DadosRet.TituloRet.DataRegistro               := DateBBtoDateTime(LItemObject.AsString['dataRegistro']);
            LListaRetorno.DadosRet.TituloRet.Vencimento                 := DateBBtoDateTime(LItemObject.AsString['dataVencimento']);
            LListaRetorno.DadosRet.TituloRet.ValorDocumento             := LItemObject.AsFloat['valorOriginal'];
            LListaRetorno.DadosRet.TituloRet.Carteira                   := OnlyNumber(LItemObject.AsString['carteiraConvenio']);
            LListaRetorno.DadosRet.TituloRet.Modalidade                 := LItemObject.AsInteger['variacaoCarteiraConvenio'];
            LListaRetorno.DadosRet.TituloRet.codigoEstadoTituloCobranca := OnlyNumber(LItemObject.AsString['codigoEstadoTituloCobranca']);
            LListaRetorno.DadosRet.TituloRet.estadoTituloCobranca       := LItemObject.AsString['estadoTituloCobranca'];
            LListaRetorno.DadosRet.TituloRet.contrato                   := LItemObject.AsString['contrato'];
            LListaRetorno.DadosRet.TituloRet.DataMovimento              := DateBBtoDateTime(LItemObject.AsString['dataMovimento']);
            LListaRetorno.DadosRet.TituloRet.dataCredito                := DateBBtoDateTime(LItemObject.AsString['dataCredito']);
            LListaRetorno.DadosRet.TituloRet.ValorAtual                 := LItemObject.AsFloat['valorAtual'];
            LListaRetorno.DadosRet.TituloRet.ValorPago                  := LItemObject.AsFloat['valorPago'];
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
          LMensagemRejeicao.Ocorrencia := 'ERRO INTERNO nos servidores do Banco do Brasil.';
        end;
    end;
  end;
end;

function TRetornoEnvio_BancoBrasil_API.RetornoEnvio(const AIndex: Integer): Boolean;
begin

  Result:=inherited RetornoEnvio(AIndex);

end;

end.

