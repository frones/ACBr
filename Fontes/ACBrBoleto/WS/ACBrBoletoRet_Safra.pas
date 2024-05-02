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
unit ACBrBoletoRet_Safra;

interface

uses
  Classes,
  SysUtils,
  ACBrBoleto,
  ACBrBoletoRetorno,
  ACBrBoletoWS.Rest,
  ACBrJSON,
  ACBrUtil.Base;

type

    { TRetornoEnvio_Safra }

  TRetornoEnvio_Safra = class(TRetornoEnvioREST)
  private
    function DateInterToDateTime(Const AValue: String): TDateTime;
  public
    constructor Create(ABoletoWS: TACBrBoleto); override;
    destructor Destroy; Override;
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

constructor TRetornoEnvio_Safra.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);

end;

function TRetornoEnvio_Safra.DateInterToDateTime(const AValue: String): TDateTime;
var
  data, ano, mes, dia: String;
begin

  ano    := Copy(AValue, 0, 4);
  mes    := Copy(AValue, 6, 2);
  dia    := Copy(AValue, 9, 2);
  data   := Format('%s/%s/%s', [ dia, mes, ano ]);
  Result := StrToDateDef(data, 0);
end;

destructor TRetornoEnvio_Safra.Destroy;
begin
  inherited Destroy;
end;

function TRetornoEnvio_Safra.LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;
var
  LJson, LJSonObject, LJsonViolacao   : TACBrJSONObject;
  ARejeicao     : TACBrBoletoRejeicao;
  LJsonArrayViolacoes: TACBrJSONArray;
  TipoOperacao  : TOperacao;
  x             : Integer;
  LSituacao     : string;

begin
  Result                    := True;
  TipoOperacao              := ACBrBoleto.Configuracoes.WebService.Operacao;
  ARetornoWS.JSONEnvio      := EnvWs;
  ARetornoWS.HTTPResultCode := HTTPResultCode;

  if RetWS <> '' then
  begin
    try
      //LJson := TJson.Create;
      try
        LJson.Parse(RetWS);
        ARetornoWS.JSON := LJson.ToJSON;

        case HTTPResultCode of
          400, 415:
            begin
              LJsonArrayViolacoes := LJson.AsJSONArray['violacoes' ];
              for x          := 0 to LJsonArrayViolacoes.Count - 1 do
              begin
                LJsonViolacao      := LJsonArrayViolacoes.ItemAsJSONObject[ x ];
                ARejeicao          := ARetornoWS.CriarRejeicaoLista;
                ARejeicao.Codigo   := LJsonViolacao.AsString['codigo' ];
                ARejeicao.Versao   := LJsonViolacao.AsString[ 'parametro' ];
                ARejeicao.Mensagem := LJsonViolacao.AsString[ 'razao' ] + ' de ' + LJsonViolacao.AsString[ 'propriedade' ] + ' Valor :' +
                  LJsonViolacao.AsString[ 'valor' ];
              end;
            end;

          404:
            begin
              if LJson.IsJSONObject('codigo') then
                if NaoEstaVazio(LJson.AsString[ 'codigo' ]) then
                begin
                  ARejeicao          := ARetornoWS.CriarRejeicaoLista;
                  ARejeicao.Codigo   := LJson.AsString[ 'codigo' ];
                  ARejeicao.Versao   := LJson.AsString[ 'parametro' ];
                  ARejeicao.Mensagem := LJson.AsString[ 'mensagem' ];
                end;

            end;

        end;

          //retorna quando tiver sucesso
        if (ARetornoWS.ListaRejeicao.Count = 0) then
        begin
          if (TipoOperacao = tpInclui) then
          begin

            ARetornoWS.DadosRet.IDBoleto.CodBarras := LJson.AsString[ 'codigoBarras' ];
            ARetornoWS.DadosRet.IDBoleto.LinhaDig  := LJson.AsString[ 'linhaDigitavel' ];
            ARetornoWS.DadosRet.IDBoleto.NossoNum  := LJson.AsString[ 'nossoNumero' ];

            ARetornoWS.DadosRet.TituloRet.CodBarras   := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig    := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
            ARetornoWS.DadosRet.TituloRet.NossoNumero := ARetornoWS.DadosRet.IDBoleto.NossoNum;

          end
          else
            if (TipoOperacao = tpConsultaDetalhe) then
            begin
              LJSonObject.Parse(LJson.ToJSON);
              ARetornoWS.DadosRet.IDBoleto.CodBarras := LJSonObject.AsString[ 'codigoBarras' ];
              ARetornoWS.DadosRet.IDBoleto.LinhaDig  := LJSonObject.AsString[ 'linhaDigitavel' ];
              ARetornoWS.DadosRet.IDBoleto.NossoNum  := LJSonObject.AsString[ 'nossoNumero' ];
              ARetornoWS.indicadorContinuidade       := false;

              ARetornoWS.DadosRet.TituloRet.NossoNumero    := ARetornoWS.DadosRet.IDBoleto.NossoNum;
              ARetornoWS.DadosRet.TituloRet.Vencimento     := DateInterToDateTime(LJSonObject.AsString[ 'dataVencimento' ]);
              ARetornoWS.DadosRet.TituloRet.ValorDocumento := LJSonObject.AsCurrency[ 'valorNominal' ];
              ARetornoWS.DadosRet.TituloRet.ValorAtual     := LJSonObject.AsCurrency[ 'valorNominal' ];
              ARetornoWS.DadosRet.TituloRet.ValorPago      := LJSonObject.AsCurrency[ 'valorTotalRecebimento' ];
              ARetornoWS.DadosRet.TituloRet.ValorMoraJuros := LJSonObject.AsJSONObject[ 'mora' ].AsCurrency[ 'valor' ];
              ARetornoWS.DadosRet.TituloRet.PercentualMulta := LJSonObject.AsJSONObject[ 'multa' ].AsCurrency[ 'taxa' ];
              ARetornoWS.DadosRet.TituloRet.CodBarras := ARetornoWS.DadosRet.IDBoleto.CodBarras;
              ARetornoWS.DadosRet.TituloRet.LinhaDig  := ARetornoWS.DadosRet.IDBoleto.LinhaDig;

              ARetornoWS.DadosRet.TituloRet.SeuNumero    := LJSonObject.AsString[ 'seuNumero' ];
              ARetornoWS.DadosRet.TituloRet.DataRegistro := DateInterToDateTime(LJSonObject.AsString[ 'dataEmissao' ]);
              ARetornoWS.DadosRet.TituloRet.Vencimento   := DateInterToDateTime(LJSonObject.AsString[ 'dataVencimento' ]);

              ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca := LJSonObject.AsString[ 'situacao' ];
              ARetornoWS.DadosRet.TituloRet.DataMovimento := DateInterToDateTime(LJSonObject.AsString[ 'dataHoraSituacao' ]);
              ARetornoWS.DadosRet.TituloRet.DataCredito := DateInterToDateTime(LJSonObject.AsString[ 'dataHoraSituacao' ]);

              ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado := LJSonObject.AsJSONObject[ 'pagador' ].AsString[ 'nome' ];
              ARetornoWS.DadosRet.TituloRet.Sacado.Cidade := LJSonObject.AsJSONObject[ 'pagador' ].AsString[ 'cidade' ];
              ARetornoWS.DadosRet.TituloRet.Sacado.UF     := LJSonObject.AsJSONObject[ 'pagador' ].AsString[ 'uf' ];
              ARetornoWS.DadosRet.TituloRet.Sacado.Bairro := LJSonObject.AsJSONObject[ 'pagador' ].AsString[ 'bairro' ];
              ARetornoWS.DadosRet.TituloRet.Sacado.Cep    := LJSonObject.AsJSONObject[ 'pagador' ].AsString[ 'cep' ];
              ARetornoWS.DadosRet.TituloRet.Sacado.Numero := LJSonObject.AsJSONObject[ 'pagador' ].AsString[ 'numero' ];
              ARetornoWS.DadosRet.TituloRet.Sacado.Logradouro := LJSonObject.AsJSONObject[ 'pagador' ].AsString[ 'logradouro' ];
              ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF := LJSonObject.AsJSONObject[ 'pagador' ].AsString[ 'cpfCnpj' ];
              LSituacao := LJSonObject.AsString[ 'situacao' ];
              if (LSituacao = C_CANCELADO) or (LSituacao = C_EXPIRADO) or
                 (LSituacao = C_PAGO) or (LSituacao = C_EXPIRADO) then
              begin
                ARetornoWS.DadosRet.TituloRet.ValorPago := LJSonObject.AsCurrency[ 'valorNominal' ];
                ARetornoWS.DadosRet.TituloRet.DataBaixa := DateInterToDateTime(LJSonObject.AsString[ 'dataHoraSituacao' ])
              end;

            end
            else
              if (TipoOperacao = tpBaixa) then
              begin
                  // não possui dados de retorno..
              end
              else
                if (TipoOperacao = tpAltera) then
                begin
                    // não possui dados de retorno..
                end;
        end;

      finally
        LJson.free;
      end;

    except
      Result := false;
    end;

  end;

end;

function TRetornoEnvio_Safra.LerListaRetorno: Boolean;
var
  ListaRetorno: TACBrBoletoRetornoWS;
  LJson, LJSonObject : TACBrJSONObject;
  LRejeicao   : TACBrBoletoRejeicao;
  LJsonArrayBoletos: TACBrJSONArray;
  I           : Integer;
  LJsonString, LSituacao : String;
begin
  Result                      := True;
  ListaRetorno                := ACBrBoleto.CriarRetornoWebNaLista;
  ListaRetorno.HTTPResultCode := HTTPResultCode;
  ListaRetorno.JSONEnvio      := EnvWs;

  if RetWS <> '' then
  begin

    try
      //LJson := TJson.Create;
      try
        LJson.Parse(RetWS);

        case HTTPResultCode of
          400, 404:
            begin
              if (LJson.IsJSONObject('codigo')) then
                if NaoEstaVazio(LJson.AsString[ 'codigo' ]) then
                begin
                  LRejeicao          := ListaRetorno.CriarRejeicaoLista;
                  LRejeicao.Codigo   := LJson.AsString[ 'codigo' ];
                  LRejeicao.Versao   := LJson.AsString[ 'parametro' ];
                  LRejeicao.Mensagem := LJson.AsString[ 'mensagem' ];
                end;
            end;
        end;

          //retorna quando tiver sucesso
        if (ListaRetorno.ListaRejeicao.Count = 0) then
        begin
          LJsonString  := LJson.ToJSON;
          LJsonArrayBoletos := LJson.AsJSONArray[ 'content' ];
          for I        := 0 to Pred(LJsonArrayBoletos.Count) do
          begin
            if I > 0 then
              ListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;


            LJSonObject := LJsonArrayBoletos.ItemAsJSONObject[ I ];

            ListaRetorno.DadosRet.IDBoleto.CodBarras  := LJSonObject.AsString[ 'codigoBarras' ];
            ListaRetorno.DadosRet.IDBoleto.LinhaDig   := LJSonObject.AsString[ 'linhaDigitavel' ];
            ListaRetorno.DadosRet.IDBoleto.NossoNum   := LJSonObject.AsString[ 'nossoNumero' ];
            ListaRetorno.indicadorContinuidade        := false;
            ListaRetorno.DadosRet.TituloRet.CodBarras := ListaRetorno.DadosRet.IDBoleto.CodBarras;
            ListaRetorno.DadosRet.TituloRet.LinhaDig  := ListaRetorno.DadosRet.IDBoleto.LinhaDig;

            ListaRetorno.DadosRet.TituloRet.NossoNumero := ListaRetorno.DadosRet.IDBoleto.NossoNum;
            ListaRetorno.DadosRet.TituloRet.Vencimento  := DateInterToDateTime(LJSonObject.AsString[ 'dataVencimento' ]);

            ListaRetorno.DadosRet.TituloRet.ValorDocumento := LJSonObject.AsCurrency[ 'valorNominal' ];
            ListaRetorno.DadosRet.TituloRet.ValorAtual     := LJSonObject.AsCurrency[ 'valorNominal' ];
            ListaRetorno.DadosRet.TituloRet.ValorPago      := LJSonObject.AsCurrency[ 'valorTotalRecebimento' ];
            ListaRetorno.DadosRet.TituloRet.ValorMoraJuros := LJSonObject.AsJSONObject[ 'mora' ].AsCurrency[ 'valor' ];
            ListaRetorno.DadosRet.TituloRet.PercentualMulta:= LJSonObject.AsJSONObject[ 'multa' ].AsCurrency[ 'taxa' ];

            ListaRetorno.DadosRet.TituloRet.SeuNumero    := LJSonObject.AsString[ 'seuNumero' ];
            ListaRetorno.DadosRet.TituloRet.DataRegistro := DateInterToDateTime(LJSonObject.AsString[ 'dataEmissao' ]);
            ListaRetorno.DadosRet.TituloRet.Vencimento   := DateInterToDateTime(LJSonObject.AsString[ 'dataVencimento' ]);

            ListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca := LJSonObject.AsString[ 'situacao' ];
            ListaRetorno.DadosRet.TituloRet.DataMovimento        := DateInterToDateTime(LJSonObject.AsString[ 'dataHoraSituacao' ]);
            ListaRetorno.DadosRet.TituloRet.DataCredito          := DateInterToDateTime(LJSonObject.AsString[ 'dataHoraSituacao' ]);

            ListaRetorno.DadosRet.TituloRet.Sacado.NomeSacado := LJSonObject.AsJSONObject[ 'pagador' ].AsString[ 'nome' ];
            ListaRetorno.DadosRet.TituloRet.Sacado.Cidade     := LJSonObject.AsJSONObject[ 'pagador' ].AsString[ 'cidade' ];
            ListaRetorno.DadosRet.TituloRet.Sacado.UF         := LJSonObject.AsJSONObject[ 'pagador' ].AsString[ 'uf' ];
            ListaRetorno.DadosRet.TituloRet.Sacado.Bairro     := LJSonObject.AsJSONObject[ 'pagador' ].AsString[ 'bairro' ];
            ListaRetorno.DadosRet.TituloRet.Sacado.Cep        := LJSonObject.AsJSONObject[ 'pagador' ].AsString[ 'cep' ];
            ListaRetorno.DadosRet.TituloRet.Sacado.Numero     := LJSonObject.AsJSONObject[ 'pagador' ].AsString[ 'numero' ];
            ListaRetorno.DadosRet.TituloRet.Sacado.Logradouro := LJSonObject.AsJSONObject[ 'pagador' ].AsString[ 'logradouro' ];
            ListaRetorno.DadosRet.TituloRet.Sacado.CNPJCPF    := LJSonObject.AsJSONObject[ 'pagador' ].AsString[ 'cpfCnpj' ];

            LSituacao := LJSonObject.AsString[ 'situacao' ];

            if (LSituacao = C_CANCELADO) or (LSituacao = C_EXPIRADO) or
              (LSituacao = C_PAGO) or (LSituacao = C_EXPIRADO) then
            begin
              ListaRetorno.DadosRet.TituloRet.ValorPago := LJSonObject.AsCurrency[ 'valorNominal' ];
              ListaRetorno.DadosRet.TituloRet.DataBaixa := DateInterToDateTime(LJSonObject.AsString[ 'dataHoraSituacao' ])
            end;

          end;
        end;

      finally
        LJson.free;
      end;

    except
      Result := false;
    end;

  end;
end;

function TRetornoEnvio_Safra.RetornoEnvio(const AIndex: Integer): Boolean;
begin
  Result := inherited RetornoEnvio(AIndex);
end;

end.
