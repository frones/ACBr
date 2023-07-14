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

unit ACBrBoletoRet_Sicredi_APIV2;

interface

uses
  Classes,
  SysUtils,
  DateUtils,
  Jsons,
  ACBrBoleto,
  ACBrBoletoWS,
  ACBrBoletoRetorno,
  ACBrBoletoWS.Rest,
  pcnConversao;

type

{ TRetornoEnvio_Sicredi_APIV2 }

 TRetornoEnvio_Sicredi_APIV2 = class(TRetornoEnvioREST)
 private
   function DateSicreditoDateTime(Const AValue : String) : TDateTime;
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

constructor TRetornoEnvio_Sicredi_APIV2.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);

end;

function TRetornoEnvio_Sicredi_APIV2.DateSicreditoDateTime(const AValue: String): TDateTime;
begin
  Result := ACBrUtil.DateTime.StringToDateTimeDef(AValue, 0,'yyyy-mm-dd');
end;

destructor TRetornoEnvio_Sicredi_APIV2.Destroy;
begin
  inherited Destroy;
end;

function TRetornoEnvio_Sicredi_APIV2.LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;
var
  AJson: TJson;
  AJSonRejeicao: TJsonObject;
  ARejeicao: TACBrBoletoRejeicao;
  AJSonResp: TJsonArray;
  Descontos: TJSONArray;
  Desconto: TJSONObject;
  I: Integer;
  TipoOperacao : TOperacao;
  Var_File : TFileStream;
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
        //if (TipoOperacao <> tpConsultarPDF) then
        //Begin //Não faz o parse pois vem o PDF em binario
          AJSon.Parse(RetWS);
          ARetornoWS.JSON           := AJson.Stringify;
        //End;//if (TipoOperacao <> tpConsultarPDF) then

        //retorna quando houver erro
        case TipoOperacao of
          tpInclui:
            begin
              case HTTPResultCode of
                400,
                403,
                500,
                401 :
                  begin
                    if (AJson.Values['error'].AsString <> '') then
                    begin
                      ARejeicao            := ARetornoWS.CriarRejeicaoLista;
                      ARejeicao.Codigo     := AJson.Values['status'].AsString;
                      ARejeicao.Versao     := AJson.Values['error'].AsString;
                      ARejeicao.Mensagem   := AJson.Values['message'].AsString;
                    end;
                  end;

              end;
            end;
          tpBaixa,
          tpAltera,
          tpConsultaDetalhe : begin
            case HTTPResultCode of
              200 : begin
                ARetornoWS.DadosRet.TituloRet.CodBarras      := aJson.Values['codigoBarras'].AsString;
                ARetornoWS.DadosRet.TituloRet.LinhaDig       := aJson.Values['linhaDigitavel'].AsString;
                ARetornoWS.DadosRet.TituloRet.Carteira       := aJson.Values['carteira'].AsString;
                ARetornoWS.DadosRet.TituloRet.SeuNumero      := aJson.Values['seuNumero'].asString;
                ARetornoWS.DadosRet.TituloRet.NossoNumero    := aJson.Values['nossoNumero'].AsString;
                //Pagador
                //ARetornoWS.DadosRet.TituloRet.Sacado.codigo         := aJson.Values['pagador'].asObject.Values['codigo'].asString;
                ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado      := aJson.Values['pagador'].asObject.Values['nome'].asString;
                ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF         := aJson.Values['pagador'].asObject.Values['documento'].asString;
                //Datas
                ARetornoWS.DadosRet.TituloRet.DataRegistro           := DateSicreditoDateTime( aJson.Values['dataEmissao'].asString );
                ARetornoWS.DadosRet.TituloRet.Vencimento             := DateSicreditoDateTime( aJson.Values['dataVencimento'].asString );
                ARetornoWS.DadosRet.TituloRet.DataBaixa              := DateSicreditoDateTime(aJson.Values['dataPagamento'].AsString);
                //Valores
                ARetornoWS.DadosRet.TituloRet.ValorDocumento         := aJson.Values['valorNominal'].AsNumber;
                //Situação/Código da situação.
                ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca    := UpperCase(aJson.Values['situacao'].asString);
                if (UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca) = 'EM CARTEIRA') or
                   (UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca) = 'VENCIDO') then
                  ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '1';
                if UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca) ='BAIXADO POR SOLICITACAO' then
                  ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '7';
                if UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca) = ('LIQUIDADO') then
                  ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '6';
                //Multa/Juros
                ARetornoWS.DadosRet.TituloRet.PercentualMulta        := aJson.Values['valorMulta'].AsNumber;
                ARetornoWS.DadosRet.TituloRet.ValorAbatimento        := aJson.Values['abatimento'].AsNumber;
                ARetornoWS.DadosRet.TituloRet.ValorMoraJuros         := aJson.Values['juros'].AsNumber;
                ARetornoWS.DadosRet.TituloRet.DiasDeProtesto         := aJson.Values['diasProtesto'].AsInteger;
                ARetornoWS.DadosRet.TituloRet.TxId                   := aJson.Values['txId'].AsString;
                ARetornoWS.DadosRet.TituloRet.EMV                    := aJson.Values['codigoQrCode'].AsString;
                ARetornoWS.DadosRet.TituloRet.PercentualMulta        := aJson.Values['valorMulta'].AsNumber;
                if ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca = '6' then
                begin
                   ARetornoWS.DadosRet.TituloRet.ValorPago              := AJson.Values['dadosLiquidacao'].AsObject.Values['valor'].AsNumber;
                   ARetornoWS.DadosRet.TituloRet.ValorMoraJuros         := AJson.Values['dadosLiquidacao'].AsObject.Values['juros'].AsNumber;
                   ARetornoWS.DadosRet.TituloRet.ValorAbatimento        := AJson.Values['dadosLiquidacao'].AsObject.Values['abatimento'].AsNumber;
                   ARetornoWS.DadosRet.TituloRet.DataBaixa              := DateSicreditoDateTime(AJson.Values['dadosLiquidacao'].AsObject.Values['data'].AsString);
                   ARetornoWS.DadosRet.TituloRet.ValorDesconto          := AJson.Values['dadosLiquidacao'].AsObject.Values['desconto'].AsNumber;
                end;
                Descontos := AJson.Values['descontos'].AsArray;
                if Assigned(Descontos) then
                begin
                  for I := 0 to Descontos.Count - 1 do
                  begin
                    if I = 0 then
                    begin
                      ARetornoWS.DadosRet.TituloRet.ValorDesconto:= Desconto.Values['valorDesconto'].AsNumber;
                      ARetornoWS.DadosRet.TituloRet.DataDesconto := DateSicreditoDateTime(Desconto.Values['dataLimite'].AsString);
                    end;
                    if I = 1 then
                    begin
                      ARetornoWS.DadosRet.TituloRet.ValorDesconto2:= Desconto.Values['valorDesconto'].AsNumber;
                      ARetornoWS.DadosRet.TituloRet.DataDesconto2 := DateSicreditoDateTime(Desconto.Values['dataLimite'].AsString);
                    end;

                  end;

                end;

              end;
              400,
              403,
              500 :
              begin
                AJSonResp := AJson.Values['errors'].AsArray;
                for I := 0 to Pred(AJSonResp.Count) do
                begin
                  AJSonRejeicao        := AJSonResp[I].AsObject;
                  ARejeicao            := ARetornoWS.CriarRejeicaoLista;
                  ARejeicao.Codigo     := AJSonRejeicao.Values['status'].AsString;
                  ARejeicao.Mensagem   := AJSonRejeicao.Values['message'].AsString;
                end;
              end;
              401 :
              begin
                if (AJson.Values['error'].AsString <> '') then
                begin
                  ARejeicao            := ARetornoWS.CriarRejeicaoLista;
                  ARejeicao.Codigo     := AJson.Values['status'].AsString;
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

            ARetornoWS.DadosRet.IDBoleto.CodBarras      := AJson.Values['codigoBarras'].AsString;
            ARetornoWS.DadosRet.IDBoleto.LinhaDig       := AJson.Values['linhaDigitavel'].AsString;
            ARetornoWS.DadosRet.IDBoleto.NossoNum       := AJson.Values['nossoNumero'].AsString;
            ARetornoWS.DadosRet.TituloRet.UrlPix        := '';//AJson.Values['url'].AsString;
            ARetornoWS.DadosRet.TituloRet.TxId          := AJson.Values['txid'].AsString;
            ARetornoWS.DadosRet.TituloRet.EMV           := AJson.Values['QrCode'].AsString;

            ARetornoWS.DadosRet.TituloRet.CodBarras     := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig      := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
            ARetornoWS.DadosRet.TituloRet.NossoNumero   := ARetornoWS.DadosRet.IDBoleto.NossoNum;
          end else
          if (TipoOperacao = tpBaixa) then
          begin
            ARetornoWS.DadosRet.TituloRet.NossoNumero      := AJson.Values['nossoNumero'].AsString;
            ARetornoWS.DadosRet.TituloRet.DataBaixa     := DateSicreditoDateTime( AJson.Values['dataMovimento'].AsString);

          end else
          if (TipoOperacao in [tpAltera]) then//,tpAlteraSeuNumero
          begin
            ARetornoWS.DadosRet.TituloRet.NossoNumero      := AJson.Values['nossoNumero'].AsString;
            ARetornoWS.DadosRet.TituloRet.DataMovimento     := DateSicreditoDateTime( AJson.Values['dataMovimento'].AsString);

          end else
          {if (TipoOperacao = tpConsultarPDF) then
          begin
            if Assigned(StreamRetorno) and (StreamRetorno.Size > 0) then
            Begin
              Try
                Var_File := TFileStream.Create(ACBrBoleto.ACBrBoletoFC.NomeArquivo,fmCreate);
                Var_File.CopyFrom(StreamRetorno,0);
              Finally
                Var_File.Free;
              End;
            End;
          end ;}
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
                ARejeicao.Versao     := 'ERRO INTERNO SICREDI';
                ARejeicao.Mensagem   := 'SERVIÇO INDISPONÍVEL. O servidor está impossibilitado de lidar com a requisição no momento. Tente mais tarde.';
                ARejeicao.Ocorrencia := 'ERRO INTERNO nos servidores do Sicredi.';
              end;
          end;
        end;
    end;

  end;

end;

function TRetornoEnvio_Sicredi_APIV2.LerListaRetorno: Boolean;
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
          503,
          401 :
            begin
              if (AJson.Values['error'].AsString <> '') then
              begin
                ARejeicao            := ListaRetorno.CriarRejeicaoLista;
                ARejeicao.Codigo     := AJson.Values['code'].AsString;
                ARejeicao.Versao     := AJson.Values['error'].AsString;
                ARejeicao.Mensagem   := AJson.Values['message'].AsString;
              end;
            end;

        end;

        //retorna quando tiver sucesso
        if (ListaRetorno.ListaRejeicao.Count = 0) then
        begin
          AJsonBoletos := AJson.Values['items'].AsArray;
          ListaRetorno.indicadorContinuidade := AJson.Values['hasNext'].AsBoolean;
          for I := 0 to Pred(AJsonBoletos.Count) do
          begin
            if I > 0 then
              ListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;

            AJSonObject  := AJsonBoletos[I].AsObject;


            ListaRetorno.DadosRet.IDBoleto.CodBarras      := '';
            ListaRetorno.DadosRet.IDBoleto.LinhaDig       := '';
            ListaRetorno.DadosRet.IDBoleto.NossoNum       := AJSonObject.Values['nossoNumero'].AsString;

            ListaRetorno.DadosRet.TituloRet.CodBarras      := ListaRetorno.DadosRet.IDBoleto.CodBarras;
            ListaRetorno.DadosRet.TituloRet.LinhaDig       := ListaRetorno.DadosRet.IDBoleto.LinhaDig;


            ListaRetorno.DadosRet.TituloRet.NossoNumero                := ListaRetorno.DadosRet.IDBoleto.NossoNum;
            ListaRetorno.DadosRet.TituloRet.DataBaixa                  := DateSicreditoDateTime(AJSonObject.Values['dataPagamento'].AsString);
            ListaRetorno.DadosRet.TituloRet.SeuNumero                  := AJSonObject.Values['seuNumero'].AsString;
            ListaRetorno.DadosRet.TituloRet.ValorDocumento             := AJSonObject.Values['valor'].AsNumber;
            ListaRetorno.DadosRet.TituloRet.ValorRecebido              := AJSonObject.Values['valorLiquidado'].AsNumber;
            ListaRetorno.DadosRet.TituloRet.ValorPago                  := AJSonObject.Values['valorLiquidado'].AsNumber;
            ListaRetorno.DadosRet.TituloRet.ValorRecebido              := AJSonObject.Values['valorLiquidado'].AsNumber;
            ListaRetorno.DadosRet.TituloRet.ValorMoraJuros             := AJSonObject.Values['jurosLiquido'].AsNumber;
            ListaRetorno.DadosRet.TituloRet.ValorDesconto              := AJSonObject.Values['descontoLiquido'].AsNumber;
            ListaRetorno.DadosRet.TituloRet.ValorOutrosCreditos        := AJSonObject.Values['multaLiquida'].AsNumber;
            ListaRetorno.DadosRet.TituloRet.ValorAbatimento            := AJSonObject.Values['abatimentoLiquido'].AsNumber;
            ListaRetorno.DadosRet.TituloRet.Mensagem.Text              := AJSonObject.Values['tipoLiquidacao'].AsString;
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
      400 :
        begin
          ARejeicao            := ListaRetorno.CriarRejeicaoLista;
          ARejeicao.Codigo     := '400';
          ARejeicao.Mensagem   := Msg;
        end;
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
          ARejeicao.Versao     := 'ERRO INTERNO SICREDI';
          ARejeicao.Mensagem   := 'SERVIÇO INDISPONÍVEL. O servidor está impossibilitado de lidar com a requisição no momento. Tente mais tarde.';
          ARejeicao.Ocorrencia := 'ERRO INTERNO nos servidores do Sicredi.';
        end;
    end;
  end;

end;

function TRetornoEnvio_Sicredi_APIV2.RetornoEnvio(const AIndex: Integer): Boolean;
begin

  Result:=inherited RetornoEnvio(AIndex);

end;

end.


