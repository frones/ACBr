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
//  {$IfDef USE_JSONDATAOBJECTS_UNIT}
//    JsonDataObjects_ACBr,
//  {$Else}
    Jsons,
//  {$EndIf}
  DateUtils, pcnConversao,ACBrBoletoWS.Rest;

type

{ TRetornoEnvio_Inter_API }

 TRetornoEnvio_Inter_API = class(TRetornoEnvioREST)
 private
   function DateInterToDateTime(Const AValue : String) : TDateTime;
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
  C_EXPIRADO = 'EXPIRADO';
  C_VENCIDO = 'VENCIDO';
  C_EMABERTO = 'EMABERTO';
  C_PAGO = 'PAGO';


{ TRetornoEnvio }

constructor TRetornoEnvio_Inter_API.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);

end;

function TRetornoEnvio_Inter_API.DateInterToDateTime(
  const AValue: String): TDateTime;
var
data,
ano,
mes,
dia:String;
begin

  ano := Copy( aValue, 0,4 );
  mes := Copy( aValue, 6,2 );
  dia := Copy( aValue, 9,2 );
  data := Format( '%s/%s/%s' , [dia,mes,ano]);
  Result := StrToDateDef( data ,0 );
end;

destructor TRetornoEnvio_Inter_API.Destroy;
begin
  inherited Destroy;
end;

function TRetornoEnvio_Inter_API.LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;
var
  //Retorno: TRetEnvio;
  AJson: TJson;
  AJSonObject: TJsonObject;
  ARejeicao: TACBrBoletoRejeicao;
  aJsonViolacao:TJsonObject;
  aJsonViolacoes,
  AJsonBoletos: TJsonArray;
  TipoOperacao : TOperacao;
  x:Integer;
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
        AJSon.Parse(RetWS);
        ARetornoWS.JSON := AJson.Stringify;

        case HttpResultCode of
          400,415 : begin
                  aJsonViolacoes := aJson.Values['violacoes'].AsArray;
                  for x := 0 to aJsonViolacoes.Count -1 do
                    begin
                        aJsonViolacao        := aJsonViolacoes[x].AsObject;
                        ARejeicao            := ARetornoWS.CriarRejeicaoLista;
                        ARejeicao.Codigo     := AJsonViolacao.Values['codigo'].AsString;
                        ARejeicao.Versao     := AJsonViolacao.Values['parametro'].AsString;
                        ARejeicao.Mensagem   := AJsonViolacao.Values['razao'].AsString
                                                + ' de '
                                                + AJsonViolacao.Values['propriedade'].AsString
                                                +' Valor :'
                                                + AJsonViolacao.Values['valor'].AsString;
                    end;
                end;


          404 : begin

            if ( AJson.StructType = jsObject ) then
              if( AJson.Values['codigo'].asString <> '' ) then
              begin
                ARejeicao            := ARetornoWS.CriarRejeicaoLista;
                ARejeicao.Codigo     := AJson.Values['codigo'].AsString;
                ARejeicao.Versao     := AJson.Values['parametro'].AsString;
                ARejeicao.Mensagem   := AJson.Values['mensagem'].AsString;
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

            ARetornoWS.DadosRet.TituloRet.CodBarras     := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig      := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
            ARetornoWS.DadosRet.TituloRet.NossoNumero   := ARetornoWS.DadosRet.IDBoleto.NossoNum;

            ARetornoWS.DadosRet.TituloRet.SeuNumero       := AJson.Values['seuNumero'].AsString;

          end else
          if (TipoOperacao = tpConsultaDetalhe) then
          begin
            aJsonObject := TJsonObject.Create;
            aJsonObject.Parse( AJson.Stringify );
            ARetornoWS.DadosRet.IDBoleto.CodBarras       := AJSonObject.Values['codigoBarras'].AsString;;;
            ARetornoWS.DadosRet.IDBoleto.LinhaDig        := AJSonObject.Values['linhaDigitavel'].AsString;;
            ARetornoWS.DadosRet.IDBoleto.NossoNum        := AJSonObject.Values['nossoNumero'].AsString;
            ARetornoWS.indicadorContinuidade             := false;

            ARetornoWS.DadosRet.TituloRet.NossoNumero     := ARetornoWS.DadosRet.IDBoleto.NossoNum;
            ARetornoWS.DadosRet.TituloRet.Vencimento      := DateInterToDateTime(AJSonObject.Values['dataVencimento'].AsString);
            ARetornoWS.DadosRet.TituloRet.ValorDocumento  := AJSonObject.Values['valorNominal'].AsNumber;
            ARetornoWS.DadosRet.TituloRet.ValorAtual      := AJSonObject.Values['valorNominal'].AsNumber;
            ARetornoWS.DadosRet.TituloRet.ValorPago       := AJSonObject.Values['valorTotalRecebimento'].AsNumber;
            ARetornoWS.DadosRet.TituloRet.ValorMoraJuros  := AJSonObject.Values['mora'].asObject.Values['valor'].asNumber;
            ARetornoWS.DadosRet.TituloRet.PercentualMulta := AJSonObject.Values['multa'].asObject.Values['taxa'].asNumber;
            ARetornoWS.DadosRet.TituloRet.CodBarras       := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig        := ARetornoWS.DadosRet.IDBoleto.LinhaDig;

            ARetornoWS.DadosRet.TituloRet.SeuNumero       := AJSonObject.Values['seuNumero'].asString;
            ARetornoWS.DadosRet.TituloRet.DataRegistro    := DateIntertoDateTime( AJSonObject.Values['dataEmissao'].asString );
            ARetornoWS.DadosRet.TituloRet.Vencimento      := DateIntertoDateTime( AJSonObject.Values['dataVencimento'].asString );

            ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca  := AJSonObject.Values['situacao'].asString;
            ARetornoWS.DadosRet.TituloRet.DataMovimento         := DateIntertoDateTime( AJSonObject.Values['dataHoraSituacao'].asString );
            ARetornoWS.DadosRet.TituloRet.DataCredito           := DateIntertoDateTime( AJSonObject.Values['dataHoraSituacao'].asString );

            ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado     := AJSonObject.Values['pagador'].asObject.Values['nome'].asString;
            ARetornoWS.DadosRet.TituloRet.Sacado.Cidade         := AJSonObject.Values['pagador'].asObject.Values['cidade'].asString;
            ARetornoWS.DadosRet.TituloRet.Sacado.UF             := AJSonObject.Values['pagador'].asObject.Values['uf'].asString;;
            ARetornoWS.DadosRet.TituloRet.Sacado.Bairro         := AJSonObject.Values['pagador'].asObject.Values['bairro'].asString;;
            ARetornoWS.DadosRet.TituloRet.Sacado.Cep            := AJSonObject.Values['pagador'].asObject.Values['cep'].asString;;
            ARetornoWS.DadosRet.TituloRet.Sacado.Numero         := AJSonObject.Values['pagador'].asObject.Values['numero'].asString;;
            ARetornoWS.DadosRet.TituloRet.Sacado.Logradouro     := AJSonObject.Values['pagador'].asObject.Values['logradouro'].asString;;
            ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF        := AJSonObject.Values['pagador'].asObject.Values['cpfCnpj'].asString;

            if( AJSonObject.Values['situacao'].asString = C_CANCELADO ) or
               ( AJSonObject.Values['situacao'].asString = C_EXPIRADO ) or
               ( AJSonObject.Values['situacao'].asString = C_PAGO ) or
               ( AJSonObject.Values['situacao'].asString = C_EXPIRADO ) then
               begin
                 //   ARetornoWS.DadosRet.TituloRet.ValorPago                   := AJSonObject.Values['valorNominal'].AsNumber;
                 ARetornoWS.DadosRet.TituloRet.DataBaixa                   := DateIntertoDateTime( AJSonObject.Values['dataHoraSituacao'].asString )
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

      finally
        AJson.free;
      end;

    except
      Result := False;
    end;

  end;

end;

function TRetornoEnvio_Inter_API.LerListaRetorno: Boolean;
var
  ListaRetorno: TACBrBoletoRetornoWS;
  AJson: TJson;
  AJSonObject: TJsonObject;
  ARejeicao: TACBrBoletoRejeicao;
  AJsonBoletos: TJsonArray;
  I: Integer;
  aJsonString:String;
  vIndice:Integer;
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

        case HTTPResultCode of
          400, 404 : begin
            if ( AJson.StructType = jsObject ) then
              if( AJson.Values['codigo'].asString <> '' ) then
              begin
                ARejeicao            := ListaRetorno.CriarRejeicaoLista;
                ARejeicao.Codigo     := AJson.Values['codigo'].AsString;
                ARejeicao.Versao     := AJson.Values['parametro'].AsString;
                ARejeicao.Mensagem   := AJson.Values['mensagem'].AsString;
              end;
          end;
        end;

        //retorna quando tiver sucesso
        if (ListaRetorno.ListaRejeicao.Count = 0) then
        begin
          aJsonString := aJson.Stringify;

          if not(AJson.Values['last'].AsBoolean) then
          begin
            ListaRetorno.indicadorContinuidade := True;
		    vIndice := Trunc(ACBrBoleto.Configuracoes.WebService.Filtro.indiceContinuidade);
            ListaRetorno.proximoIndice := (vIndice+1)
          end
          else
          begin
            ListaRetorno.indicadorContinuidade := False;
            ListaRetorno.proximoIndice := 0;
          end;

          aJsonString := AJson.Values['content'].Stringify;
          AJsonBoletos := AJson.Values['content'].AsArray;
          for I := 0 to Pred(AJsonBoletos.Count) do
          begin
            if I > 0 then
              ListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;

            AJSonObject  := AJsonBoletos[I].AsObject;

            ListaRetorno.DadosRet.IDBoleto.CodBarras             := AJSonObject.Values['codigoBarras'].AsString;;;
            ListaRetorno.DadosRet.IDBoleto.LinhaDig              := AJSonObject.Values['linhaDigitavel'].AsString;;
            ListaRetorno.DadosRet.IDBoleto.NossoNum              := AJSonObject.Values['nossoNumero'].AsString;
            ListaRetorno.indicadorContinuidade                   := false;
            ListaRetorno.DadosRet.TituloRet.CodBarras            := ListaRetorno.DadosRet.IDBoleto.CodBarras;
            ListaRetorno.DadosRet.TituloRet.LinhaDig             := ListaRetorno.DadosRet.IDBoleto.LinhaDig;

            ListaRetorno.DadosRet.TituloRet.NossoNumero          := ListaRetorno.DadosRet.IDBoleto.NossoNum;
            ListaRetorno.DadosRet.TituloRet.Vencimento           := DateInterToDateTime(AJSonObject.Values['dataVencimento'].AsString);

            ListaRetorno.DadosRet.TituloRet.ValorDocumento       := AJSonObject.Values['valorNominal'].AsNumber;
            ListaRetorno.DadosRet.TituloRet.ValorAtual           := AJSonObject.Values['valorNominal'].AsNumber;
            ListaRetorno.DadosRet.TituloRet.ValorPago            := AJSonObject.Values['valorTotalRecebimento'].AsNumber;
            ListaRetorno.DadosRet.TituloRet.ValorMoraJuros       := AJSonObject.Values['mora'].asObject.Values['valor'].asNumber;
            ListaRetorno.DadosRet.TituloRet.PercentualMulta      := AJSonObject.Values['multa'].asObject.Values['taxa'].asNumber;

            ListaRetorno.DadosRet.TituloRet.SeuNumero            := AJSonObject.Values['seuNumero'].asString;
            ListaRetorno.DadosRet.TituloRet.DataRegistro         := DateIntertoDateTime( AJSonObject.Values['dataEmissao'].asString );
            ListaRetorno.DadosRet.TituloRet.Vencimento           := DateIntertoDateTime( AJSonObject.Values['dataVencimento'].asString );

            ListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca := AJSonObject.Values['situacao'].asString;
            ListaRetorno.DadosRet.TituloRet.DataMovimento        := DateIntertoDateTime( AJSonObject.Values['dataHoraSituacao'].asString );
            ListaRetorno.DadosRet.TituloRet.DataCredito          := DateIntertoDateTime( AJSonObject.Values['dataHoraSituacao'].asString );

            ListaRetorno.DadosRet.TituloRet.Sacado.NomeSacado    := AJSonObject.Values['pagador'].asObject.Values['nome'].asString;
            ListaRetorno.DadosRet.TituloRet.Sacado.Cidade        := AJSonObject.Values['pagador'].asObject.Values['cidade'].asString;
            ListaRetorno.DadosRet.TituloRet.Sacado.UF            := AJSonObject.Values['pagador'].asObject.Values['uf'].asString;
            ListaRetorno.DadosRet.TituloRet.Sacado.Bairro        := AJSonObject.Values['pagador'].asObject.Values['bairro'].asString;
            ListaRetorno.DadosRet.TituloRet.Sacado.Cep           := AJSonObject.Values['pagador'].asObject.Values['cep'].asString;
            ListaRetorno.DadosRet.TituloRet.Sacado.Numero        := AJSonObject.Values['pagador'].asObject.Values['numero'].asString;
            ListaRetorno.DadosRet.TituloRet.Sacado.Logradouro    := AJSonObject.Values['pagador'].asObject.Values['logradouro'].asString;
            ListaRetorno.DadosRet.TituloRet.Sacado.CNPJCPF       := AJSonObject.Values['pagador'].asObject.Values['cpfCnpj'].asString;



              if( AJSonObject.Values['situacao'].asString = C_CANCELADO ) or
                 ( AJSonObject.Values['situacao'].asString = C_EXPIRADO ) or
                 ( AJSonObject.Values['situacao'].asString = C_PAGO ) or
                 ( AJSonObject.Values['situacao'].asString = C_EXPIRADO ) then
                 begin
                    ListaRetorno.DadosRet.TituloRet.ValorPago                   := AJSonObject.Values['valorNominal'].AsNumber;
                    ListaRetorno.DadosRet.TituloRet.DataBaixa                   := DateIntertoDateTime( AJSonObject.Values['dataHoraSituacao'].asString )
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

function TRetornoEnvio_Inter_API.RetornoEnvio(const AIndex: Integer): Boolean;
begin
  Result:=inherited RetornoEnvio(AIndex);
end;

end.

