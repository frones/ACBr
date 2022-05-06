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

unit ACBrBoletoRet_Sicredi_API;

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

{ TRetornoEnvio_Sicredi_API }

 TRetornoEnvio_Sicredi_API = class(TRetornoEnvioREST)
 private
   function DateSicrediToDateTime(Const AValue : String) : TDateTime;
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

constructor TRetornoEnvio_Sicredi_API.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);

end;

function TRetornoEnvio_Sicredi_API.DateSicrediToDateTime(
  const AValue: String): TDateTime;
begin
  Result := StrToDateDef( StringReplace( AValue,'-','/', [rfReplaceAll] ),0);
end;

destructor TRetornoEnvio_Sicredi_API.Destroy;
begin
  inherited Destroy;
end;

function TRetornoEnvio_Sicredi_API.LerRetorno: Boolean;
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
    try
      AJSon := TJson.Create;
      try
        AJSon.Parse(RetWS);
        if ( AJson.StructType = jsObject ) then
          if( AJson.Values['codigo'].asString <> '' ) then
          begin
            ARejeicao            := Retorno.CriarRejeicaoLista;
            ARejeicao.Codigo     := AJson.Values['codigo'].AsString;
            ARejeicao.Versao     := AJson.Values['parametro'].AsString;
            ARejeicao.Mensagem   := AJson.Values['mensagem'].AsString;
          end;

        //retorna quando tiver sucesso
        if (Retorno.ListaRejeicao.Count = 0) then
        begin
          if (TipoOperacao = tpInclui) then
          begin

            Retorno.DadosRet.IDBoleto.CodBarras      := AJson.Values['codigoBarra'].AsString;
            Retorno.DadosRet.IDBoleto.LinhaDig       := AJson.Values['linhaDigitavel'].AsString;
            Retorno.DadosRet.IDBoleto.NossoNum       := AJson.Values['nossoNumero'].AsString;

            Retorno.DadosRet.TituloRet.CodBarras     := Retorno.DadosRet.IDBoleto.CodBarras;
            Retorno.DadosRet.TituloRet.LinhaDig      := Retorno.DadosRet.IDBoleto.LinhaDig;
            Retorno.DadosRet.TituloRet.NossoNumero   := Retorno.DadosRet.IDBoleto.NossoNum;

          end else
          if (TipoOperacao in [tpConsulta,tpConsultaDetalhe]) then
          begin
            AJsonBoletos := TJsonArray.Create;
            AJsonBoletos.Parse( AJson.Stringify );
            for I := 0 to Pred(AJsonBoletos.Count) do
            begin
              if I > 0 then
                Retorno := ACBrBoleto.CriarRetornoWebNaLista;
                
              AJSonObject  := AJsonBoletos[I].AsObject;

              Retorno.DadosRet.IDBoleto.CodBarras       := '';
              Retorno.DadosRet.IDBoleto.LinhaDig        := '';
              Retorno.DadosRet.IDBoleto.NossoNum        := AJSonObject.Values['nossoNumero'].AsString;
              Retorno.indicadorContinuidade             := false;
              Retorno.DadosRet.TituloRet.CodBarras      := Retorno.DadosRet.IDBoleto.CodBarras;
              Retorno.DadosRet.TituloRet.LinhaDig       := Retorno.DadosRet.IDBoleto.LinhaDig;


              Retorno.DadosRet.TituloRet.NossoNumero                := Retorno.DadosRet.IDBoleto.NossoNum;
              Retorno.DadosRet.TituloRet.Vencimento                 := DateSicrediToDateTime(AJSonObject.Values['dataVencimento'].AsString);
              Retorno.DadosRet.TituloRet.ValorDocumento             := AJSonObject.Values['valor'].AsNumber;
              Retorno.DadosRet.TituloRet.ValorAtual                 := AJSonObject.Values['valor'].AsNumber;

              if( AJSonObject.Values['situacao'].asString = C_LIQUIDADO ) or
                 ( AJSonObject.Values['situacao'].asString = C_BAIXADO_POS_SOLICITACAO ) then
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

      finally
        AJson.free;
      end;

    except
      Result := False;
    end;

  end;

end;

function TRetornoEnvio_Sicredi_API.RetornoEnvio: Boolean;
begin

  Result:=inherited RetornoEnvio;

end;

end.

