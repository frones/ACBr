{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{ Colaboradores nesse arquivo:  Willian Delan                                  }
{ Delmar de Lima                                                               }
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
//incluido em COLOCAR A DATA

{$I ACBr.inc}

unit ACBrBoletoRet_Bradesco;

interface

uses
  Classes,
  SysUtils,
  DateUtils,
  StrUtils,


  ACBrBoleto,
  ACBrBoletoWS,
  ACBrBoletoRetorno,
  ACBrBoletoWS.Rest,
  pcnConversao ;

type

{ TRetornoEnvio_Sicoob_API }

 TRetornoEnvio_Bradesco = class(TRetornoEnvioREST)
 private
   function DateBradescoToDateTime(Const AValue : String) : TDateTime;
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

resourcestring
  C_CANCELADO = 'CANCELADO';
  C_BAIXADO   = 'BAIXADO';
  C_EXPIRADO  = 'EXPIRADO';
  C_VENCIDO   = 'VENCIDO';
  C_EMABERTO  = 'EM ABERTO';
  C_PAGO      = 'Liquidado';

{ TRetornoEnvio }

constructor TRetornoEnvio_Bradesco.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);
end;

function TRetornoEnvio_Bradesco.DateBradescoToDateTime(const AValue: String): TDateTime;
var
  LData, LAno, LMes, LDia : String;
begin
  LAno := Copy(AValue, 0, 4);
  LMes := Copy(AValue, 6, 2);
  LDia := Copy(AValue, 9, 2);
  LData := Format('%s/%s/%s', [LDia, LMes, LAno]);
  Result := StrToDateDef(LData, 0);
end;

destructor TRetornoEnvio_Bradesco.Destroy;
begin
  inherited Destroy;
end;

function TRetornoEnvio_Bradesco.LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;
var
  LJsonObject: TACBrJSONObject;
  LMensagemRejeicao: TACBrBoletoRejeicao;
  LJsonViolacao, LjsonDetalhe:TACBrJSONObject;
  LJsonViolacoes, LJsonDetalhes: TACBrJSONArray;
  LTipoOperacao : TOperacao;
  i :Integer;
begin
  Result := True;
  LTipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;
  ARetornoWs.JSONEnvio      := EnvWs;
  ARetornoWS.HTTPResultCode := HTTPResultCode;
  if RetWS <> '' then
  begin
    try
      LJsonObject := TACBrJSONObject.Create;
      try
        LJsonObject.Parse(RetWS);
        ARetornoWS.JSON           := LJsonObject.ToJSON;
        case HttpResultCode of
          207, 400, 406, 500:
          begin
            LJsonViolacoes := LJsonObject.AsJSONArray['details'];
            if LJsonViolacoes.Count > 0 then
            begin
               for i := 0 to Pred(LJsonViolacoes.Count) do
               begin
                 LJsonViolacao        := LJsonViolacoes.ItemAsJSONObject[i];
                 LMensagemRejeicao            := ARetornoWS.CriarRejeicaoLista;

                 LMensagemRejeicao.Codigo     := LJsonViolacao.AsString['name'];
                 LMensagemRejeicao.mensagem   := LJsonViolacao.AsString['value'];
               end;
            end
            else
            begin
               LMensagemRejeicao            := ARetornoWS.CriarRejeicaoLista;

               LMensagemRejeicao.Codigo     := LJsonObject.AsString['code'];
               LMensagemRejeicao.mensagem   := LJsonObject.AsString['message'];
            end;
          end;
        end;
        //retorna quando tiver sucesso
        if (ARetornoWS.ListaRejeicao.Count = 0) then
        begin
          if (LTipoOperacao = tpInclui) then
          begin
            LJsonDetalhes := LJsonObject.AsJSONArray['details'];
            LjsonDetalhe := LJsonDetalhes.ItemAsJSONObject[0];
            ///IntToStr(LJsonObject.JsonArray[0].AsObject.Values['ctitloCobrCdent'].AsInteger);

            ARetornoWS.DadosRet.TituloRet.NossoNumero                 := LjsonDetalhe.AsString['ctitloCobrCdent'];
            ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca        := LjsonDetalhe.AsString['codStatus10'];//Ex. A Vencer/Vencido
            ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca  := LjsonDetalhe.AsString['codStatus10'];//Ex 01.
            ARetornoWS.DadosRet.TituloRet.SeuNumero                   := LjsonDetalhe.AsString['snumero10'];
            ARetornoWS.DadosRet.TituloRet.DataRegistro                := DateBradescoToDateTime(LjsonDetalhe.AsString['dataReg10']);
            ARetornoWS.DadosRet.TituloRet.DataDocumento               := DateBradescoToDateTime(LjsonDetalhe.AsString['dataEmis10']);
            ARetornoWS.DadosRet.TituloRet.ValorDocumento              := LjsonDetalhe.AsCurrency['valMoeda10'];
            ARetornoWS.DadosRet.TituloRet.CodBarras                   := LjsonDetalhe.AsString['codBarras10'];
            ARetornoWS.DadosRet.TituloRet.LinhaDig                    := LjsonDetalhe.AsString['linhaDig10'];
            ARetornoWS.DadosRet.TituloRet.Vencimento                  := DateBradescoToDateTime(LjsonDetalhe.AsString['dataVenctoBol10']);
            ARetornoWS.DadosRet.TituloRet.TxId                        := LjsonDetalhe.AsString['iconcPgtoSpi'];
            ARetornoWS.DadosRet.TituloRet.EMV                         := LjsonDetalhe.AsString['wqrcdPdraoMercd'];
          end
          else
          if (LTipoOperacao = tpConsultaDetalhe) then
          begin
            //Implementar.
          end;
        end;
      finally
        LJsonObject.free;
      end;
    except
      Result := False;
    end;
  end;
end;

function TRetornoEnvio_Bradesco.LerListaRetorno: Boolean;
var
  LListaRetorno: TACBrBoletoRetornoWS;
  LJsonObject: TACBrJSONObject;
  LJSonObjectItem: TACBrJSONObject;
  LMensagemRejeicao: TACBrBoletoRejeicao;
  LJsonBoletos: TACBrJSONArray;
  LMora, LMulta, LPagador : TACBrJSONObject;
  LSituacao:string;
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
        case HTTPResultCode of
          400, 404 :
          begin
            if( LJsonObject.AsString['codigo'] <> '' ) then
            begin
              LMensagemRejeicao            := LListaRetorno.CriarRejeicaoLista;

              LMensagemRejeicao.Codigo     := LJsonObject.AsString['codigo'];
              LMensagemRejeicao.Versao     := LJsonObject.AsString['parametro'];
              LMensagemRejeicao.Mensagem   := LJsonObject.AsString['mensagem'];
            end;
          end;
        end;
        //retorna quando tiver sucesso
        if (LListaRetorno.ListaRejeicao.Count = 0) then
        begin

          LJsonBoletos := LJsonObject.AsJSONArray['content'];
          for I := 0 to Pred(LJsonBoletos.Count) do
          begin

            if I > 0 then
              LListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;

            LJSonObjectItem  := LJsonBoletos.ItemAsJSONObject[I];

            LListaRetorno.indicadorContinuidade                   := false;

            LListaRetorno.DadosRet.TituloRet.CodBarras            := LListaRetorno.DadosRet.IDBoleto.CodBarras;
            LListaRetorno.DadosRet.TituloRet.LinhaDig             := LListaRetorno.DadosRet.IDBoleto.LinhaDig;
            LListaRetorno.DadosRet.TituloRet.NossoNumero          := LListaRetorno.DadosRet.IDBoleto.NossoNum;

            LListaRetorno.DadosRet.IDBoleto.CodBarras             := LJSonObjectItem.AsString['codigoBarras'];
            LListaRetorno.DadosRet.IDBoleto.LinhaDig              := LJSonObjectItem.AsString['linhaDigitavel'];
            LListaRetorno.DadosRet.IDBoleto.NossoNum              := LJSonObjectItem.AsString['nossoNumero'];
            LListaRetorno.DadosRet.TituloRet.Vencimento           := DateBradescoToDateTime(LJSonObjectItem.AsString['dataVencimento']);
            LListaRetorno.DadosRet.TituloRet.ValorDocumento       := LJSonObjectItem.AsCurrency['valorNominal'];
            LListaRetorno.DadosRet.TituloRet.ValorAtual           := LJSonObjectItem.AsCurrency['valorNominal'];
            LListaRetorno.DadosRet.TituloRet.ValorPago            := LJSonObjectItem.AsCurrency['valorTotalRecebimento'];

            LMora := LJSonObjectItem.AsJSONObject['mora'];
            if LMora <>nil then
              LListaRetorno.DadosRet.TituloRet.ValorMoraJuros       := LMora.AsCurrency['valor'];

            LMulta := LJSonObjectItem.AsJSONObject['multa'];
            if LMulta <> nil then
              LListaRetorno.DadosRet.TituloRet.PercentualMulta      := LMulta.AsCurrency['taxa'];

            LListaRetorno.DadosRet.TituloRet.SeuNumero            := LJSonObjectItem.AsString['seuNumero'];
            LListaRetorno.DadosRet.TituloRet.DataRegistro         := DateBradescoToDateTime( LJSonObjectItem.AsString['dataEmissao']);
            LListaRetorno.DadosRet.TituloRet.Vencimento           := DateBradescoToDateTime( LJSonObjectItem.AsString['dataVencimento'] );
            LListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca := LJSonObjectItem.AsString['situacao'];
            LListaRetorno.DadosRet.TituloRet.DataMovimento        := DateBradescoToDateTime( LJSonObjectItem.AsString['dataHoraSituacao']);
            LListaRetorno.DadosRet.TituloRet.DataCredito          := DateBradescoToDateTime( LJSonObjectItem.AsString['dataHoraSituacao']);

            LPagador := LJSonObjectItem.AsJSONObject['pagador'];
            if LPagador <> nil then
            begin
              LListaRetorno.DadosRet.TituloRet.Sacado.NomeSacado    := LPagador.AsString['nome'];
              LListaRetorno.DadosRet.TituloRet.Sacado.Cidade        := LPagador.AsString['cidade'];
              LListaRetorno.DadosRet.TituloRet.Sacado.UF            := LPagador.AsString['uf'];
              LListaRetorno.DadosRet.TituloRet.Sacado.Bairro        := LPagador.AsString['bairro'];
              LListaRetorno.DadosRet.TituloRet.Sacado.Cep           := LPagador.AsString['cep'];
              LListaRetorno.DadosRet.TituloRet.Sacado.Numero        := LPagador.AsString['numero'];
              LListaRetorno.DadosRet.TituloRet.Sacado.Logradouro    := LPagador.AsString['logradouro'];
              LListaRetorno.DadosRet.TituloRet.Sacado.CNPJCPF       := LPagador.AsString['cpfCnpj'];
            end;

            LSituacao:= LJSonObjectItem.AsString['situacao'];
            if( LSituacao = C_CANCELADO ) or
              ( LSituacao = C_EXPIRADO ) or
              ( LSituacao = C_PAGO ) or
              ( LSituacao = C_EXPIRADO ) then
            begin
              LListaRetorno.DadosRet.TituloRet.ValorPago                   := LJSonObjectItem.AsCurrency['valorNominal'];
              LListaRetorno.DadosRet.TituloRet.DataBaixa                   := DateBradescoToDateTime( LJSonObjectItem.AsString['dataHoraSituacao'])
            end;
          end;
        end;
      finally
        LJsonObject.free;
      end;
    except
      Result := False;
    end;
  end;
end;

function TRetornoEnvio_Bradesco.RetornoEnvio(const AIndex: Integer): Boolean;
begin
  Result:=inherited RetornoEnvio(AIndex);
end;

end.

