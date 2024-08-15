{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{ Colaboradores nesse arquivo: Willian Delan de Oliveira                       }
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
//incluido em 15/08/2023

{$I ACBr.inc}

unit ACBrBoletoRet_Cresol;

interface

uses
   SysUtils,
   StrUtils,
   ACBrBoleto,
   ACBrBoletoRetorno,
   ACBrBoletoWS.Rest,
   ACBrJSON;

type
   { TRetornoEnvio_Cresol_API }
   TRetornoEnvio_Cresol = class(TRetornoEnvioREST)
   private
      function DateCresolToDateTime(const AValue: String): TDateTime;
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
   ACBrUtil.Strings;

{ TRetornoEnvio }

constructor TRetornoEnvio_Cresol.Create(ABoletoWS: TACBrBoleto);
begin
   inherited Create(ABoletoWS);
end;

function TRetornoEnvio_Cresol.DateCresolToDateTime(const AValue: String): TDateTime;
var
   LDia, LMes, LAno: String;
begin
   LAno := Copy(AValue, 0, 4);
   LMes := Copy(AValue, 6, 2);
   LDia := Copy(AValue, 9, 2);
   Result := StrToDate(LDia+'/'+LMes+'/'+LAno);
end;

destructor TRetornoEnvio_Cresol.Destroy;
begin
   inherited Destroy;
end;

function TRetornoEnvio_Cresol.LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;
var
   LJsonObject: TACBrJSONObject;
   ARejeicao: TACBrBoletoRejeicao;
   LJsonBoleto: TACBrJSONObject;
   ATipoOperacao : TOperacao;
begin
   Result := True;
   ATipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;
   ARetornoWs.JSONEnvio      := EnvWs;
   ARetornoWS.HTTPResultCode := HTTPResultCode;
   if Trim(RetWS) <> '' then
   begin
      Try
         LJSonObject := TACBrJSONObject.Create;
         try
            LJSonObject.Parse(RetWS);
            ARetornoWS.JSON           := LJsonObject.ToJSON;
            case HttpResultCode of
               207, 400, 406, 500 :
               begin
                  ARejeicao            := ARetornoWS.CriarRejeicaoLista;
                  ARejeicao.Codigo     := LJsonObject.AsString['code'];
                  ARejeicao.mensagem   := LJsonObject.AsString['message'];
               end;
               200 :
               //retorna quando tiver sucesso
               if (ARetornoWS.ListaRejeicao.Count = 0) then
               begin
                  if (ATipoOperacao = tpInclui) then
                  begin
                     LJsonBoleto := LJsonObject.AsJSONObject['content'];
                     ///LJsonObject.AsJSONArray[0].AsObject.Values['id'].AsString;
                     // ele tinha assim enão entendi se o banco retorna mais de um
                     /// Necessário testar e revisar

                     ARetornoWS.DadosRet.IDBoleto.IDBoleto        := LJsonBoleto.AsString['id'];
                     ARetornoWS.DadosRet.IDBoleto.CodBarras       := LJsonBoleto.AsString['codigoBarras'];
                     ARetornoWS.DadosRet.IDBoleto.LinhaDig        := LJsonBoleto.AsString['linhaDigitavel'];
                     ARetornoWS.DadosRet.IDBoleto.NossoNum        := LJsonBoleto.AsString['nossoNumero'];
                     ARetornoWS.DadosRet.TituloRet.CodBarras      := ARetornoWS.DadosRet.IDBoleto.CodBarras;
                     ARetornoWS.DadosRet.TituloRet.LinhaDig       := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
                     ARetornoWS.DadosRet.TituloRet.NossoNumero    := ARetornoWS.DadosRet.IDBoleto.NossoNum;
                     ARetornoWS.DadosRet.TituloRet.Vencimento     := DateCresolToDateTime(LJsonBoleto.AsString['dtvencimento']);
                     ARetornoWS.DadosRet.TituloRet.NossoNumero    := LJsonBoleto.AsString['nossoNumero'];
                     ARetornoWS.DadosRet.TituloRet.SeuNumero      := StrUtils.IfThen(LJsonBoleto.AsString['NumeroDocumento'] <> '',
                                                                                 LJsonBoleto.AsString['NumeroDocumento'],
                                                                                 OnlyNumber(LJsonBoleto.AsString['nossoNumero'])
                                                                              );
                     ARetornoWS.DadosRet.TituloRet.EspecieDoc     := LJsonBoleto.AsString['idEspecie'];
                     ARetornoWS.DadosRet.TituloRet.DataDocumento  := DateCresolToDateTime(LJsonBoleto.AsString['dtDocumento']);
                     ARetornoWS.DadosRet.TituloRet.ValorDocumento := LJsonBoleto.AsFloat['valorNominal'];
                     ARetornoWS.DadosRet.TituloRet.ValorDesconto  := LJsonBoleto.AsFloat['valorDesconto'];
                  end
                  else
                  if (ATipoOperacao = tpConsultaDetalhe) then
                  begin
                     if LJsonObject.AsString['id'] <> '' then
                     begin
                        ARetornoWS.DadosRet.IDBoleto.IDBoleto         := LJsonObject.AsString['id'];
                        ARetornoWS.DadosRet.IDBoleto.CodBarras        := LJsonObject.AsString['codigoBarras'];
                        ARetornoWS.DadosRet.IDBoleto.LinhaDig         := LJsonObject.AsString['linhaDigitavel'];
                        ARetornoWS.DadosRet.IDBoleto.NossoNum         := LJsonObject.AsString['nossoNumero'];
                        ARetornoWS.indicadorContinuidade              := false;
                        ARetornoWS.DadosRet.TituloRet.NossoNumero     := ARetornoWS.DadosRet.IDBoleto.NossoNum;
                        ARetornoWS.DadosRet.TituloRet.Vencimento      := DateCresolToDateTime(LJsonObject.AsString['dtvencimento']);
                        ARetornoWS.DadosRet.TituloRet.ValorDocumento  := LJsonObject.AsFloat['valorNominal'];
                        if LJsonObject.AsString['cdTipoMulta'] = 'ISENTO' then//Sem multa.
                           ARetornoWS.DadosRet.TituloRet.PercentualMulta := 0
                        else
                           ARetornoWS.DadosRet.TituloRet.PercentualMulta  := LJsonObject.AsFloat['valorMulta'];
                        if LJsonObject.AsString['cdTipoJuros'] = 'ISENTO' then
                        begin//Sem juros.
                           ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros := cjIsento;
                           ARetornoWS.DadosRet.TituloRet.ValorMoraJuros  := 0;
                        end
                        else
                        begin
                           if LJsonObject.AsString['cdTipoJuros'] = 'VALOR_FIXO' then
                           begin//VALOR_FIXO > Valor Dia.
                              ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros := cjValorDia;
                              ARetornoWS.DadosRet.TituloRet.ValorMoraJuros  := LJsonObject.AsFloat['valorJuros'];
                           end
                           else
                           begin
                              ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros := cjTaxaMensal;//VALOR_PERCENTUAL > Taxa Mensal.
                              ARetornoWS.DadosRet.TituloRet.ValorMoraJuros  := LJsonObject.AsFloat['valorJuros'];
                           end;
                        end;
                        ARetornoWS.DadosRet.TituloRet.CodBarras       := ARetornoWS.DadosRet.IDBoleto.CodBarras;
                        ARetornoWS.DadosRet.TituloRet.LinhaDig        := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
                        ARetornoWS.DadosRet.TituloRet.SeuNumero      := StrUtils.IfThen(LJsonObject.AsString['NumeroDocumento'] <> '',
                                                                                    LJsonObject.AsString['NumeroDocumento'],
                                                                                    OnlyNumber(LJsonObject.AsString['nossoNumero'])
                                                                                 );
                        ARetornoWS.DadosRet.TituloRet.DataRegistro    := DateCresolToDateTime(LJsonObject.AsString['dtDocumento']);
                        ARetornoWS.DadosRet.TituloRet.Vencimento      := DateCresolToDateTime(LJsonObject.AsString['dtvencimento']);
                        ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca  := LJsonObject.AsString['status'];//0-EM_ABERTO|3-BAIXADO_MANUALMENTE|5-LIQUIDADO
                        if Pos('EM_ABERTO', UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca)) > 0 then//ABERTO
                           ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '1';
                        if Pos('BAIXADO_MANUALMENTE',UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca)) > 0 then//BAIXADO
                           ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '7';
                        if Pos('LIQUIDADO', UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca)) > 0 then//LÍQUIDADO
                           ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '6';
                        ARetornoWS.DadosRet.TituloRet.ValorPago       := LJsonObject.AsFloat['valorNominal'];
                        ARetornoWS.DadosRet.TituloRet.DataMovimento   := DateCresolToDateTime(LJsonObject.AsString['dtDocumento']);
                        ARetornoWS.DadosRet.TituloRet.DataCredito     := DateCresolToDateTime(LJsonObject.AsString['dtDocumento']);
                        ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado     := LJsonObject.AsString['pagadorNome'];
                        ARetornoWS.DadosRet.TituloRet.Sacado.Cidade         := LJsonObject.AsString['pagadorCidade'];
                        ARetornoWS.DadosRet.TituloRet.Sacado.UF             := LJsonObject.AsString['pagadorUf'];
                        ARetornoWS.DadosRet.TituloRet.Sacado.Bairro         := LJsonObject.AsString['pagadorBairro'];
                        ARetornoWS.DadosRet.TituloRet.Sacado.Cep            := LJsonObject.AsString['pagadorCep'];
                        ARetornoWS.DadosRet.TituloRet.Sacado.Numero         := LJsonObject.AsString['pagadorEnderecoNumero'];
                        ARetornoWS.DadosRet.TituloRet.Sacado.Logradouro     := LJsonObject.AsString['pagadorEndereco'];
                        ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF        := LJsonObject.AsString['docPagador'];
                     end;
                  end
                  else
                  if (ATipoOperacao = tpBaixa) then
                  begin
                     // não possui dados de retorno..
                  end
                  else if (ATipoOperacao = tpAltera) then
                  begin
                     // não possui dados de retorno..
                  end;
               end;
            else
               ARejeicao            := ARetornoWS.CriarRejeicaoLista;
               ARejeicao.Codigo     := LJsonObject.AsString['code'];
               ARejeicao.mensagem   := LJsonObject.AsString['message'];
            end;
         Finally
            LJsonObject.free;
         End;
      except
         Result := False;
      end;
   end;
end;

function TRetornoEnvio_Cresol.LerListaRetorno: Boolean;
begin
  result := false;
  raise exception.Create('TRetornoEnvio_Cresol.LerListaRetorno não implementado');
   //Cresol possuí, apenas não implementamos.
   //Como o Cresol (ainda) não permite filtrar por data, para clientes com muitas emissões ficaria inviável, por isso usamos apenas a consulta por id, buscando o boleto individualmente.
   //Exemplo de consulta de todos os boletos:
   //https://cresolapi.governarti.com.br/titulos >>Traz todos os boletos(tem um limite, traz poucos por vez).
   //Exemplo de Consultas por status:
   //FPURL := 'https://cresolapi.governarti.com.br/titulos?status=LIQUIDADO'; >>Traz todos os boletos líquidados.
   //FPURL := 'https://cresolapi.governarti.com.br/titulos?status=BAIXADO_MANUALMENTE'; >>Traz todos os boletos baixados.
   //FPURL := 'https://cresolapi.governarti.com.br/titulos?status=EM_ABERTO'; >>Traz todos os boletos em aberto.
   //Valores para consulta por status: BAIXADO_MANUALMENTE, BAIXADO_PROTESTADO, LIQUIDACAO_EM_PROCESSAMENTO, EM_ABERTO, EM_PROCESSAMENTO, LIQUIDADO, BAIXADO_DECURSO_DE_PRAZO, REJEITADO.
   //*OBS: Caso necessário implementar fazer como base na procedure LerRetorno, mas testar em HOM pois o retorno é um pouco diferente.
end;

function TRetornoEnvio_Cresol.RetornoEnvio(const AIndex: Integer): Boolean;
begin
   Result:=inherited RetornoEnvio(AIndex);
end;

end.

