{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Victor H Gonzales - Panda                       }
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

//incluido em::
{$I ACBr.inc}

unit ACBrBoletoRet_BTGPactual;

interface

uses
  Classes,
  SysUtils,
  ACBrBoleto,
  ACBrBoletoWS,
  ACBrBoletoRetorno,
  ACBrJSON,
  DateUtils,
  ACBrBoletoWS.Rest;

type
  { TRetornoEnvio_BTGPactual }

  TRetornoEnvio_BTGPactual = class(TRetornoEnvioREST)
  private

  public
    constructor Create(ABoletoWS: TACBrBoleto); override;
    destructor  Destroy; Override;
    function LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean; override;
    function RetornoEnvio(const AIndex: Integer): Boolean; override;
  end;


implementation

uses
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrBoletoConversao,
  StrUtils,
  ACBrUtil.Base;

{ TRetornoEnvio_BTGPactual }

constructor TRetornoEnvio_BTGPactual.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);
end;

destructor TRetornoEnvio_BTGPactual.Destroy;
begin
  inherited Destroy;
end;

function TRetornoEnvio_BTGPactual.LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;
var
  LJsonArray, LJsonErrorArray: TACBrJSONArray;
  LJsonObject : TACBrJSONObject;
  LRejeicao: TACBrBoletoRejeicao;
  I: Integer;
  TipoOperacao : TOperacao;
  LTotalObjetos, nIndiceOBJ : integer;
  LExisteBankSlip : boolean;

begin
  Result := True;
  LExisteBankSlip := false;

  TipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;

  ARetornoWs.JSONEnvio       := EnvWs;
  ARetornoWS.HTTPResultCode  := HTTPResultCode;
  ARetornoWS.Header.Operacao := TipoOperacao;

  if RetWS <> '' then
  begin
    LJsonArray := TACBrJSONArray.Parse(RetWS);
    try
      try
        ARetornoWS.JSON := LJsonArray.ToJSON;
        if HTTPResultCode < 300 then
        begin
          case TipoOperacao of
            tpInclui:
              begin
                case HTTPResultCode of
                  202 :
                  begin
                    LTotalObjetos := LJsonArray.Count;
                    LJSONObject := LJsonArray.ItemAsJSONObject[0];
                    ARetornoWS.DadosRet.TituloRet.NossoNumero          := LJSONObject.AsString['ourNumber'] + LJSONObject.AsString['ourNumberDigit'];
                    ARetornoWS.DadosRet.TituloRet.SeuNumero            := LJSONObject.AsString['bankSlipId'];
                    ARetornoWS.DadosRet.TituloRet.Vencimento           := StringToDateTimeDef(LJSONObject.AsString['dueDate'], 0, 'yyyy-mm-dd');
                    ARetornoWS.DadosRet.TituloRet.DataDocumento        := StringToDateTimeDef(LJSONObject.AsString['createdAt'], 0, 'yyyy-mm-dd');
                    ARetornoWS.DadosRet.TituloRet.ValorDocumento       := LJSONObject.AsFloat['amount'];
                    ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := LJSONObject.AsString['status'];

                    ARetornoWS.DadosRet.TituloRet.LinhaDig             := LJSONObject.AsString['digitableLine'];
                    ARetornoWS.DadosRet.TituloRet.CodBarras            := LJSONObject.AsString['barCode'];

                    if LJSONObject.AsString['correlationId'] <> '' then
                      ARetornoWS.DadosRet.TituloRet.NossoNumeroCorrespondente  := LJSONObject.AsString['correlationId'];

                    ARetornoWS.DadosRet.TituloRet.EMV                  := LJSONObject.AsString['pixInfo'];
                    ARetornoWS.DadosRet.IDBoleto.IDBoleto              := ARetornoWS.DadosRet.TituloRet.NossoNumeroCorrespondente;
                    ARetornoWS.DadosRet.IDBoleto.CodBarras             := ARetornoWS.DadosRet.TituloRet.CodBarras;
                    ARetornoWS.DadosRet.IDBoleto.LinhaDig              := ARetornoWS.DadosRet.TituloRet.LinhaDig;
                    ARetornoWS.DadosRet.IDBoleto.NossoNum              := ARetornoWS.DadosRet.TituloRet.NossoNumero;
                  end;
                end;
              end;
            tpConsultaDetalhe,
            tpConsulta :
              begin
                case HTTPResultCode of
                  200 :
                    begin
                      LTotalObjetos := LJsonArray.Count;
                      LJSONObject := LJsonArray.ItemAsJSONObject[0];
                      ARetornoWS.DadosRet.TituloRet.NossoNumero          := LJSONObject.AsString['ourNumber'];
                      ARetornoWS.DadosRet.TituloRet.SeuNumero            := LJSONObject.AsString['bankSlipId'];
                      ARetornoWS.DadosRet.TituloRet.Vencimento           := StringToDateTimeDef(LJSONObject.AsString['dueDate'], 0, 'yyyy-mm-dd');
                      ARetornoWS.DadosRet.TituloRet.DataDocumento        := StringToDateTimeDef(LJSONObject.AsString['createdAt'], 0, 'yyyy-mm-dd');
                      ARetornoWS.DadosRet.TituloRet.ValorDocumento       := LJSONObject.AsFloat['amount'];
                      ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := LJSONObject.AsString['status'];

                      ARetornoWS.DadosRet.TituloRet.LinhaDig             := LJSONObject.AsString['digitableLine'];
                      ARetornoWS.DadosRet.TituloRet.CodBarras            := LJSONObject.AsString['barCode'];

                      if LJSONObject.AsString['correlationId'] <> '' then
                        ARetornoWS.DadosRet.TituloRet.NossoNumeroCorrespondente  := LJSONObject.AsString['correlationId'];

                      ARetornoWS.DadosRet.IDBoleto.IDBoleto              := ARetornoWS.DadosRet.TituloRet.NossoNumeroCorrespondente;
                      ARetornoWS.DadosRet.IDBoleto.CodBarras             := ARetornoWS.DadosRet.TituloRet.CodBarras;
                      ARetornoWS.DadosRet.IDBoleto.LinhaDig              := ARetornoWS.DadosRet.TituloRet.LinhaDig;
                      ARetornoWS.DadosRet.IDBoleto.NossoNum              := ARetornoWS.DadosRet.TituloRet.NossoNumero;
                    end;
                end;
              end;
          end;
        end else
        begin
          LJsonErrorArray := LJsonArray.ItemAsJSONObject[0].AsJSONArray['_errors'];
          if LJsonErrorArray.Count > 0  then
          begin
            ARetornoWS.CodRetorno := LJsonArray.ItemAsJSONObject[0].AsString['_errorCode'];
            ARetornoWS.MsgRetorno := LJsonArray.ItemAsJSONObject[0].AsString['_message'];
            for I := 0 to Pred(LJsonErrorArray.Count) do
            begin
              LRejeicao          := ARetornoWS.CriarRejeicaoLista;

              LRejeicao.Codigo   := LJsonErrorArray.ItemAsJSONObject[I].AsString['_code'];
              LRejeicao.Campo    := LJsonErrorArray.ItemAsJSONObject[I].AsString['_field'];
              LRejeicao.Mensagem := LJsonErrorArray.ItemAsJSONObject[I].AsString['_message'];
              LRejeicao.Valor    := '';
            end;
          end;
        end;
      except
        Result := False;
      end;
    finally
     LJsonArray.free;
    end;
  end
  else
  begin
    case HTTPResultCode of
      401 :
      begin
        LRejeicao            := ARetornoWS.CriarRejeicaoLista;
        LRejeicao.Codigo     := '401';
        LRejeicao.Mensagem   := 'Não autorizado/Autenticado';
        LRejeicao.Ocorrencia := '401 - Não autorizado/Autenticado';
      end;
      403 :
      begin
        LRejeicao            := ARetornoWS.CriarRejeicaoLista;
        LRejeicao.Codigo     := '403';
        LRejeicao.Mensagem   := 'Não Autorizado';
        LRejeicao.Ocorrencia := '403 - Não Autorizado';
      end;
      404 :
      begin
        LRejeicao            := ARetornoWS.CriarRejeicaoLista;
        LRejeicao.Codigo     := '404';
        LRejeicao.Mensagem   := 'Informação não encontrada';
        LRejeicao.Ocorrencia := '404 - Informação não encontrada';
      end;
      406 :
      begin
        LRejeicao            := ARetornoWS.CriarRejeicaoLista;
        LRejeicao.Codigo     := '406';
        LRejeicao.Mensagem   := 'O recurso de destino não possui uma representação atual que seria aceitável';
        LRejeicao.Ocorrencia := '406 - O recurso de destino não possui uma representação atual que seria aceitável';
      end;
      500 :
      begin
        LRejeicao            := ARetornoWS.CriarRejeicaoLista;
        LRejeicao.Codigo     := '500';
        LRejeicao.Mensagem   := 'Erro de Servidor, Aplicação está fora';
        LRejeicao.Ocorrencia := '500 - Erro de Servidor, Aplicação está fora';
      end;
      501 :
      begin
        LRejeicao            := ARetornoWS.CriarRejeicaoLista;
        LRejeicao.Codigo     := '501';
        LRejeicao.Mensagem   := 'Erro de Servidor, Aplicação está fora';
        LRejeicao.Ocorrencia := '501 - O servidor não oferece suporte à funcionalidade necessária para atender à solicitação';
      end;
    end;
  end;
end;
function TRetornoEnvio_BTGPactual.RetornoEnvio(const AIndex: Integer): Boolean;
begin
  Result:=inherited RetornoEnvio(AIndex);
end;

end.

