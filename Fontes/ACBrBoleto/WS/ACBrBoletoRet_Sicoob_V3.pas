{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{ Colaboradores nesse arquivo:  Victor Hugo Gonzales - Panda, Marcelo Santos,  }
{ Delmar de Lima, Daniel Morais InfoCotidiano                                  }
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
//incluido em 10/06/2024

{$I ACBr.inc}

unit ACBrBoletoRet_Sicoob_V3;

interface

uses
  Classes,
  SysUtils,
  ACBrBoleto,
  ACBrBoletoWS,
  ACBrBoletoRetorno,
  DateUtils,
  pcnConversao,
  ACBrBoletoWS.Rest,
  ACBrUtil.Base,
  ACBrJSON,
  ACBrCompress,
  synacode,
  ACBrUtil.FilesIO,
  ACBrUtil.Strings;

type

{ TRetornoEnvio_Sicoob_API }

 TRetornoEnvio_Sicoob_V3 = class(TRetornoEnvioREST)
 private
   function DateBancoobToDateTime(Const AValue : String) : TDateTime;
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
  C_BAIXADO = 'BAIXADO';
  C_EXPIRADO = 'EXPIRADO';
  C_VENCIDO = 'VENCIDO';
  C_EMABERTO = 'EM ABERTO';
  C_PAGO = 'Liquidado';


{ TRetornoEnvio }

constructor TRetornoEnvio_Sicoob_V3.Create(ABoletoWS: TACBrBoleto);
begin
  inherited Create(ABoletoWS);

end;

function TRetornoEnvio_Sicoob_V3.DateBancoobToDateTime(
  const AValue: String): TDateTime;
var
 data, ano, mes, dia : String;
begin
  ano := Copy( aValue, 0,4 );
  mes := Copy( aValue, 6,2 );
  dia := Copy( aValue, 9,2 );
  data := Format( '%s/%s/%s' , [dia,mes,ano]);
  Result := StrToDateDef( data ,0 );
end;

destructor TRetornoEnvio_Sicoob_V3.Destroy;
begin
  inherited Destroy;
end;

function TRetornoEnvio_Sicoob_V3.LerRetorno(const ARetornoWS: TACBrBoletoRetornoWS): Boolean;
var
  LJson, LJSonObject, LJsonListaHistoricoObject, LJsonViolacao : TACBrJSONObject;
  LJsonListaHistoricoArray, LJsonViolacoesArray: TACBrJSONArray;
  LRejeicao: TACBrBoletoRejeicao;
  TipoOperacao : TOperacao;
  x, i, vPos :Integer;
begin
  Result := True;
  TipoOperacao := ACBrBoleto.Configuracoes.WebService.Operacao;
  ARetornoWS.HTTPResultCode  := HTTPResultCode;
  ARetornoWS.JSONEnvio       := EnvWs;
  ARetornoWS.Header.Operacao := TipoOperacao;

  if (HttpResultCode = 204) and (TipoOperacao = tpConsultaDetalhe) then
  begin
    HttpResultCode   := 400;
    Result := False;
    LRejeicao := ARetornoWS.CriarRejeicaoLista;
    LRejeicao.Codigo := '400';
    LRejeicao.Ocorrencia  := '204';
    LRejeicao.Mensagem := 'A requisição foi processada com êxito e não está retornando conteúdo.';
  end;

  if Trim(RetWS) <> '' then
  begin
    try
      LJson := TACBrJSONObject.Parse(RetWS);
      try
        ARetornoWS.JSON :=  LJson.ToJSON;

        if LJson.IsJSONArray('mensagens') then
        begin
          LJsonViolacoesArray := LJson.AsJSONArray['mensagens'];
          for x := 0 to LJsonViolacoesArray.Count-1 do
          begin
            LJsonViolacao        := LJsonViolacoesArray.ItemAsJSONObject[x];
            LRejeicao            := ARetornoWS.CriarRejeicaoLista;
            LRejeicao.Codigo     := LJsonViolacao.AsString['codigo'];
            LRejeicao.mensagem   := LJsonViolacao.AsString['mensagem'];
          end;
        end;

        //retorna quando tiver sucesso
        if (ARetornoWS.ListaRejeicao.Count = 0) then
        begin
          if (TipoOperacao = tpInclui) then
          begin
            LJSonObject := LJson.AsJSONObject['resultado'];
            ARetornoWS.DadosRet.IDBoleto.CodBarras      := LJSonObject.AsString['codigoBarras'];
            ARetornoWS.DadosRet.IDBoleto.LinhaDig       := LJSonObject.AsString['linhaDigitavel'];
            ARetornoWS.DadosRet.IDBoleto.NossoNum       := LJSonObject.AsString['nossoNumero'];
            ARetornoWS.DadosRet.IDBoleto.URLPDF         := LJSonObject.AsString['pdfBoleto'];

            ARetornoWS.DadosRet.TituloRet.CodBarras      := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig       := ARetornoWS.DadosRet.IDBoleto.LinhaDig;
            ARetornoWS.DadosRet.TituloRet.NossoNumero    := ARetornoWS.DadosRet.IDBoleto.NossoNum;

            ARetornoWS.DadosRet.TituloRet.Contrato       := LJSonObject.AsString['numeroCliente'];
            ARetornoWS.DadosRet.TituloRet.EMV            := LJSonObject.AsString['qrCode'];
            ARetornoWS.DadosRet.TituloRet.UrlPix         := LJSonObject.AsString['qrCode'];
            ARetornoWS.DadosRet.TituloRet.Vencimento     := DateBancoobToDateTime(LJSonObject.AsString['dataVencimento']);
            ARetornoWS.DadosRet.TituloRet.NossoNumero    := LJSonObject.AsString['nossoNumero'];
            ARetornoWS.DadosRet.TituloRet.EspecieDoc     := LJSonObject.AsString['codigoEspecieDocumento'];
            ARetornoWS.DadosRet.TituloRet.DataDocumento  := DateBancoobToDateTime(LJSonObject.AsString['dataEmissao']);
            ARetornoWS.DadosRet.TituloRet.ValorDocumento := LJSonObject.AsCurrency['valor'];
            ARetornoWS.DadosRet.TituloRet.ValorDesconto  := LJSonObject.AsCurrency['valorPrimeiroDesconto'];
            ARetornoWS.DadosRet.TituloRet.NumeroDocumento:= LJSonObject.AsString['seuNumero'];
            ARetornoWS.DadosRet.TituloRet.SeuNumero      := trim(LJSonObject.AsString['identificacaoBoletoEmpresa']);
          end
          else if (TipoOperacao = tpConsultaDetalhe) then
          begin
            //LJSonObject := TACBrJSONObject.Create;
            LJSonObject :=  LJson.AsJSONObject['resultado'] ;

            ARetornoWS.DadosRet.IDBoleto.CodBarras       := LJSonObject.AsString['codigoBarras'];
            ARetornoWS.DadosRet.IDBoleto.LinhaDig        := LJSonObject.AsString['linhaDigitavel'];
            ARetornoWS.DadosRet.IDBoleto.NossoNum        := LJSonObject.AsString['nossoNumero'];
            ARetornoWS.indicadorContinuidade             := false;

            ARetornoWS.DadosRet.TituloRet.NossoNumero     := ARetornoWS.DadosRet.IDBoleto.NossoNum;
            ARetornoWS.DadosRet.TituloRet.Vencimento      := DateBancoobToDateTime(LJSonObject.AsString['dataVencimento']);
            ARetornoWS.DadosRet.TituloRet.ValorDocumento  := LJSonObject.AsCurrency['valor'];
            ARetornoWS.DadosRet.TituloRet.EspecieDoc      := LJSonObject.AsString['codigoEspecieDocumento'];
            ARetornoWS.DadosRet.TituloRet.Contrato        := LJSonObject.AsString['numeroCliente'];

            case LJSonObject.AsInteger['tipoMulta'] of
             1 : begin // Multa Valor Fixo
              ARetornoWS.DadosRet.TituloRet.CodigoMulta      := cmValorFixo;
              ARetornoWS.DadosRet.TituloRet.PercentualMulta  := LJSonObject.AsCurrency['valorMulta'];
              ARetornoWS.DadosRet.TituloRet.MultaValorFixo   := True;
              ARetornoWS.DadosRet.TituloRet.DataMulta        := DateBancoobToDateTime(LJSonObject.AsString['dataMulta']);
             end;
             2 : begin // Multa com Valor em Percentual
              ARetornoWS.DadosRet.TituloRet.CodigoMulta      := cmPercentual;
              ARetornoWS.DadosRet.TituloRet.PercentualMulta  := LJSonObject.AsCurrency['valorMulta'];
              ARetornoWS.DadosRet.TituloRet.MultaValorFixo   := False;
              ARetornoWS.DadosRet.TituloRet.DataMulta        := DateBancoobToDateTime(LJSonObject.AsString['dataMulta']);
             end;
             3 : begin // Sem Multa
               ARetornoWS.DadosRet.TituloRet.CodigoMulta     := cmIsento;
               ARetornoWS.DadosRet.TituloRet.PercentualMulta := 0;
             end;
            end;

            ARetornoWS.DadosRet.TituloRet.ValorMulta := ARetornoWS.DadosRet.TituloRet.PercentualMulta;

            case LJSonObject.AsInteger['tipoJurosMora'] of
             1 : begin // Valor Dia
               ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros := cjValorDia;
               ARetornoWS.DadosRet.TituloRet.ValorMoraJuros  := LJSonObject.AsCurrency['valorJurosMora'];
               ARetornoWS.DadosRet.TituloRet.DataMoraJuros   := DateBancoobToDateTime(LJSonObject.AsString['dataJurosMora']);
             end;
             2 : begin // Taxa Mensal
               ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros := cjTaxaMensal;
               ARetornoWS.DadosRet.TituloRet.ValorMoraJuros  := LJSonObject.AsCurrency['valorJurosMora'];
               ARetornoWS.DadosRet.TituloRet.DataMoraJuros   := DateBancoobToDateTime(LJSonObject.AsString['dataJurosMora']);
             end;
             3 : begin // Isento
               ARetornoWS.DadosRet.TituloRet.CodigoMoraJuros := cjIsento;
               ARetornoWS.DadosRet.TituloRet.ValorMoraJuros  := LJSonObject.AsCurrency['valorJurosMora'];
             end;
            end;

            ARetornoWS.DadosRet.TituloRet.CodBarras       := ARetornoWS.DadosRet.IDBoleto.CodBarras;
            ARetornoWS.DadosRet.TituloRet.LinhaDig        := ARetornoWS.DadosRet.IDBoleto.LinhaDig;

            ARetornoWS.DadosRet.TituloRet.NumeroDocumento := LJSonObject.asString['seuNumero'];
            ARetornoWS.DadosRet.TituloRet.SeuNumero       := Trim(LJSonObject.asString['identificacaoBoletoEmpresa']);

            ARetornoWS.DadosRet.TituloRet.DataRegistro    := DateBancoobToDateTime( LJSonObject.asString['dataEmissao'] );
            ARetornoWS.DadosRet.TituloRet.Vencimento      := DateBancoobToDateTime( LJSonObject.asString['dataVencimento'] );

            ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca  := LJSonObject.asString['situacaoBoleto'];

            if UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca) = C_EMABERTO then
               ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '1';

            if (UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca) = C_BAIXADO) or
               (UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca) = C_CANCELADO) then
               ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '7';

            if UpperCase(ARetornoWS.DadosRet.TituloRet.EstadoTituloCobranca) = UpperCase(C_PAGO) then
               ARetornoWS.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '6';


            ARetornoWS.DadosRet.TituloRet.UrlPix  := LJSonObject.AsString['qrCode'];
            ARetornoWS.DadosRet.TituloRet.EMV     := LJSonObject.AsString['qrCode'];

            if LJSonObject.IsJSONObject('pagador') then
            begin
              ARetornoWS.DadosRet.TituloRet.Sacado.NomeSacado     := LJSonObject.AsJSONObject['pagador'].AsString['nome'];
              ARetornoWS.DadosRet.TituloRet.Sacado.Cidade         := LJSonObject.AsJSONObject['pagador'].asString['cidade'];
              ARetornoWS.DadosRet.TituloRet.Sacado.UF             := LJSonObject.AsJSONObject['pagador'].asString['uf'];
              ARetornoWS.DadosRet.TituloRet.Sacado.Bairro         := LJSonObject.AsJSONObject['pagador'].asString['bairro'];
              ARetornoWS.DadosRet.TituloRet.Sacado.Cep            := LJSonObject.AsJSONObject['pagador'].asString['cep'];
              ARetornoWS.DadosRet.TituloRet.Sacado.Numero         := LJSonObject.AsJSONObject['pagador'].asString['numero'];
              ARetornoWS.DadosRet.TituloRet.Sacado.Logradouro     := LJSonObject.AsJSONObject['pagador'].asString['endereco'];
              ARetornoWS.DadosRet.TituloRet.Sacado.CNPJCPF        := LJSonObject.AsJSONObject['pagador'].asString['numeroCpfCnpj'];
            end;

            if(LJSonObject.asString['situacaoBoleto'] = C_PAGO) then
            begin
             // WorkAround para pegar DataPagamento e Valor de Pagamento

             LJsonListaHistoricoArray :=  LJSonObject.AsJSONArray['listaHistorico'];

              for i := 0 to Pred(LJsonListaHistoricoArray.Count) do
              begin
                LJsonListaHistoricoObject := LJsonListaHistoricoArray.ItemAsJSONObject[i];

                if LJsonListaHistoricoObject.AsInteger['tipoHistorico'] = 6 then // 1 = Entrada, 4 = Tarifa Liquidação, 6 = Liquidação
                begin
                 ARetornoWS.DadosRet.TituloRet.DataBaixa := DateBancoobToDateTime(LJsonListaHistoricoObject.AsString['dataHistorico']);
                 vPos := Pos('R$', LJsonListaHistoricoObject.AsString['descricaoHistorico']);
                 ARetornoWS.DadosRet.TituloRet.ValorPago := 0;
                 if vpos > 0 then
                  ARetornoWS.DadosRet.TituloRet.ValorPago := StringToFloatDef(copy(LJsonListaHistoricoObject.AsString['descricaoHistorico'], vPos+2, length(LJsonListaHistoricoObject.AsString['descricaoHistorico'])), 0);
                end;
              end;
            end;

          end
          else if (TipoOperacao = tpBaixa) then
          begin
            // não possui dados de retorno..
          end
          else if (TipoOperacao = tpAltera) then
          begin
            // não possui dados de retorno..
          end;
        end;

      finally
        LJson.free;
      end;

    except
      Result := False;
    end;

  end;

end;

function TRetornoEnvio_Sicoob_V3.LerListaRetorno: Boolean;
var
  ListaRetorno: TACBrBoletoRetornoWS;
  AJson, AJSonObject, AJsonViolacao: TACBrJSONObject;
  AJsonBoletosArray: TACBrJSONArray;
  LMensagemRejeicao: TACBrBoletoRejeicao;
  LSituacao, LRetorno, LMovimento : string;
  I, LCodigoSolicitacao, LIdArquivo: Integer;
  aJsonString:String;
  IDArquivos: TACBrJSONArray;
  LStream : TStringStream;
  LCT     : TCompressType;
  LMeuArq : TStringList;
begin
  Result := True;
  LStream  := TStringStream.Create('');

  ListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;
  ListaRetorno.HTTPResultCode := HTTPResultCode;
  ListaRetorno.JSONEnvio      := EnvWs;
  LCodigoSolicitacao := ACBrBoleto.Configuracoes.WebService.Filtro.NumeroProtocolo;
  LIdArquivo         := ACBrBoleto.Configuracoes.WebService.Filtro.Identificador;

  if RetWS <> '' then
  begin

    try
      AJSon := TACBrJSONObject.Parse(RetWS);
      try

        if HTTPResultCode >= 400 then
        begin
          if (AJson.AsString['mensagems'] <> '') then
          begin
            LMensagemRejeicao     := ListaRetorno.CriarRejeicaoLista;
            LMensagemRejeicao.Codigo     := AJson.AsString['mensagem'];
            LMensagemRejeicao.mensagem   := AJson.AsString['codigo'];
          end;
          if AJson.isJSONArray('mensagens') then
          begin
            AJsonBoletosArray := ajson.AsJSONArray['mensagens'];
            for I := 0 to Pred(AJsonBoletosArray.Count) do
            begin
              aJsonViolacao                := AJsonBoletosArray.ItemAsJSONObject[I];
              LMensagemRejeicao            := ListaRetorno.CriarRejeicaoLista;
              LMensagemRejeicao.Codigo     := AJsonViolacao.AsString['codigo'];
              LMensagemRejeicao.mensagem   := AJsonViolacao.AsString['mensagem'];
            end;
          end;
        end;
        if ((HTTPResultCode >= 200) and (HTTPResultCode < 300)) then
        begin
          ListaRetorno.MsgRetorno := AJson.AsJSONObject['resultado'].AsString['mensagem'];
          if AJson.AsJSONObject['resultado'].ValueExists('quantidadeArquivo') then
             ListaRetorno.MsgRetorno := ListaRetorno.MsgRetorno +'-Quantidade de arquivos :' + inttostr(AJson.AsJSONObject['resultado'].AsInteger['quantidadeArquivo']);
        end;

        //retorna quando tiver sucesso
        if (ListaRetorno.ListaRejeicao.Count = 0) then
        begin
          aJsonString := aJson.ToJSON;
         if (ACBrBoleto.Configuracoes.WebService.Operacao = tpConsulta) then
         begin
           {Solicitar a movimentação da carteira de cobrança registrada para beneficiário informado}
           if ((LIdArquivo=0) and (LCodigoSolicitacao=0)) then
           begin
            //ListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;
            ListaRetorno.DadosRet.TituloRet.NossoNumeroCorrespondente := aJson.AsJSONObject['resultado'].AsString['codigoSolicitacao'];
           end
           else if ((LIdArquivo=0) and (LCodigoSolicitacao>0)) then  {Consultar a situação da solicitação da movimentação}
           begin
              if aJson.AsJSONObject['resultado'].IsJSONArray('idArquivos') then
              begin
                IDArquivos := aJson.AsJSONObject['resultado'].AsJSONArray['idArquivos'];
                for I := 0 to IDArquivos.Count-1 do
                begin
                  if I > 0 then
                     ListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;
                  ListaRetorno.DadosRet.TituloRet.NossoNumeroCorrespondente := IDArquivos.Items[I]
                 // ListaRetorno.ListaArquivosConsultaMovimentacao.Add(IDArquivos.Items[I]);
                end;
              end;
           end
           else if ((LIdArquivo>0) and (LCodigoSolicitacao>0)) then {Download do(s) arquivo(s) de movimentação.}
           begin
              if aJson.AsJSONObject['resultado'].AsString['arquivo'] <> EmptyStr then
              begin
                AJsonBoletosArray := TACBrJSONArray.Create;
                try
                  (*
                  LMeuArq := TStringList.Create();
                  LMeuArq.LoadFromFile('C:\ACBr\Exemplos\ACBrBoleto\Delphi\SicoobBase64Liquidados.txt');
                  LRetorno := String(UTF8ToNativeString(unzip( DecodeBase64(LMeuArq.Text))));
                  LMeuArq.Free;
                  *)

                  LRetorno := String(UTF8ToNativeString(unzip( DecodeBase64(aJson.AsJSONObject['resultado'].AsString['arquivo']))));
                  AJsonBoletosArray := AJsonBoletosArray.Parse( LRetorno );
                  for I := 0 to Pred(AJsonBoletosArray.Count) do
                  begin
                    if I > 0 then
                      ListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;

                    AJSonObject  := AJsonBoletosArray.ItemAsJSONObject[I];

                    ListaRetorno.DadosRet.IDBoleto.CodBarras             := AJSonObject.AsString['codigoBarras'];
                    ListaRetorno.indicadorContinuidade                   := false;
                    ListaRetorno.DadosRet.TituloRet.CodBarras            := ListaRetorno.DadosRet.IDBoleto.CodBarras;

                    ListaRetorno.DadosRet.TituloRet.Vencimento           := DateBancoobToDateTime(AJSonObject.AsString['dataVencimentoTitulo']);
                    ListaRetorno.DadosRet.TituloRet.DataRegistro         := DateBancoobToDateTime(AJSonObject.AsString['dataInicioMovimento']);

                    ListaRetorno.DadosRet.TituloRet.ValorDocumento       := AJSonObject.AsCurrency['valorTitulo'];
                    ListaRetorno.DadosRet.TituloRet.ValorPago            := AJSonObject.AsCurrency['valorLiquido'];
                    ListaRetorno.DadosRet.TituloRet.ValorMoraJuros       := AJSonObject.AsCurrency['valorMora'];
                    ListaRetorno.DadosRet.TituloRet.ValorOutrasDespesas  := AJSonObject.AsCurrency['valorTarifaMovimento'];
                    ListaRetorno.DadosRet.TituloRet.ValorDesconto        := AJSonObject.AsCurrency['valorDesconto'];
                    ListaRetorno.DadosRet.TituloRet.ValorAbatimento      := AJSonObject.AsCurrency['valorAbatimento'];

                    ListaRetorno.DadosRet.TituloRet.SeuNumero            := AJSonObject.asString['seuNumero'];
                    ListaRetorno.DadosRet.IDBoleto.NossoNum              := AJSonObject.asString['numeroTitulo'];
                    ListaRetorno.DadosRet.TituloRet.NossoNumero          := ListaRetorno.DadosRet.IDBoleto.NossoNum;

                    ListaRetorno.DadosRet.TituloRet.DataMovimento        := DateBancoobToDateTime(AJSonObject.asString['dataLiquidacao']);
                    ListaRetorno.DadosRet.TituloRet.DataCredito          := DateBancoobToDateTime(AJSonObject.asString['dataPrevisaoCredito']);

                    ListaRetorno.DadosRet.TituloRet.DataBaixa            := ListaRetorno.DadosRet.TituloRet.DataMovimento;
                    ListaRetorno.DadosRet.TituloRet.EMV                  := AJSonObject.asString['qrCode'];

                    LMovimento := AJSonObject.asString['siglaMovimento'];
                    if LMovimento = 'LIQUI' then
                    begin
                      ListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca := 'Liquidado';
                      ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := AJSonObject.AsString['tipoOpFinanceira'];
                      {
                      case strtoint(AJSonObject.AsString['tipoOpFinanceira']) of
                        01: ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '01 - Via Caixa (idTipoOpFinanceira: 2 e 51)';
                        02: ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '02 - Via Correspondente Bancário (idTipoOpFinanceira: 74 e 75)';
                        03: ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '03 - Via Cartório (idTipoOpFinanceira: 57)';
                        04: ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '04 - Via Compensação (idTipoOpFinanceira: 58)';
                        05: ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '05 - Via Agendamento (idTipoOpFinanceira: 68';
                        07: ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '07 - Via Pix (idTipoOpFinanceira: 212)';
                        08: ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '08 - Via Poupança (idTipoOpFinanceira: 212)';
                        06: ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '06 - Via Retaguarda.';
                      end;
                      }
                    end
                    else if LMovimento = 'ENTR' then
                       ListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca := 'Em Aberto'
                    else if LMovimento = 'PROR' then
                       ListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca := 'Prorrogado'
                    else if LMovimento = 'AVENC' then
                       ListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca := 'A Vencer'
                    else if LMovimento = 'VENC' then
                       ListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca := 'Vencido'
                    else if LMovimento = 'BAIX' then
                      begin
                        ListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca := 'Baixado';
                        ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := AJSonObject.AsString['tipoOpFinanceira'];
                        {
                        case strtoint(AJSonObject.AsString['tipoOpFinanceira']) of
                          01: ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '01 - Por Decurso de Prazo (idTipoOpFinanceira: 11)';
                          02: ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '02 - A Pedido do Cedente (idTipoOpFinanceira: 82)';
                          03: ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '03 - Para Acerto (idTipoOpFinanceira: 80)';
                          04: ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '04 - Por Protesto (idTipoOpFinanceira: 196)';
                          06: ListaRetorno.DadosRet.TituloRet.CodigoEstadoTituloCobranca := '06 - Via Retaguarda';
                        end;
                        }
                      end;
                  end;
                finally
                  AJsonBoletosArray.Free;
                end;
              end;
           end
           else
           begin
            AJsonBoletosArray := AJson.AsJSONArray['resultado'];
            for I := 0 to Pred(AJsonBoletosArray.Count) do
            begin
              if I > 0 then
                ListaRetorno := ACBrBoleto.CriarRetornoWebNaLista;

              AJSonObject  := AJsonBoletosArray.ItemAsJSONObject[I];

              ListaRetorno.DadosRet.IDBoleto.CodBarras             := AJSonObject.AsString['codigoBarras'];
              ListaRetorno.DadosRet.IDBoleto.LinhaDig              := AJSonObject.AsString['linhaDigitavel'];
              ListaRetorno.DadosRet.IDBoleto.NossoNum              := AJSonObject.AsString['nossoNumero'];
              ListaRetorno.indicadorContinuidade                   := false;
              ListaRetorno.DadosRet.TituloRet.CodBarras            := ListaRetorno.DadosRet.IDBoleto.CodBarras;
              ListaRetorno.DadosRet.TituloRet.LinhaDig             := ListaRetorno.DadosRet.IDBoleto.LinhaDig;

              ListaRetorno.DadosRet.TituloRet.NossoNumero          := ListaRetorno.DadosRet.IDBoleto.NossoNum;
              ListaRetorno.DadosRet.TituloRet.Vencimento           := DateBancoobToDateTime(AJSonObject.AsString['dataVencimento']);

              ListaRetorno.DadosRet.TituloRet.ValorDocumento       := AJSonObject.AsCurrency['valorNominal'];
              ListaRetorno.DadosRet.TituloRet.ValorAtual           := AJSonObject.AsCurrency['valorNominal'];
              ListaRetorno.DadosRet.TituloRet.ValorPago            := AJSonObject.AsCurrency['valorTotalRecebimento'];
              ListaRetorno.DadosRet.TituloRet.ValorMoraJuros       := AJSonObject.AsJSONObject['mora'].AsCurrency['valor'];
              ListaRetorno.DadosRet.TituloRet.PercentualMulta      := AJSonObject.AsJSONObject['multa'].AsFloat['taxa'];

              ListaRetorno.DadosRet.TituloRet.NumeroDocumento      := AJSonObject.asString['seuNumero'];
              ListaRetorno.DadosRet.TituloRet.SeuNumero            := trim(AJSonObject.asString['identificacaoBoletoEmpresa']);
              ListaRetorno.DadosRet.TituloRet.DataRegistro         := DateBancoobToDateTime( AJSonObject.asString['dataEmissao'] );
              ListaRetorno.DadosRet.TituloRet.Vencimento           := DateBancoobToDateTime( AJSonObject.asString['dataVencimento'] );

              ListaRetorno.DadosRet.TituloRet.EstadoTituloCobranca := AJSonObject.asString['situacao'];
              ListaRetorno.DadosRet.TituloRet.DataMovimento        := DateBancoobToDateTime( AJSonObject.asString['dataHoraSituacao']);
              ListaRetorno.DadosRet.TituloRet.DataCredito          := DateBancoobToDateTime( AJSonObject.asString['dataHoraSituacao']);

              ListaRetorno.DadosRet.TituloRet.Sacado.NomeSacado    := AJSonObject.AsJSONObject['pagador'].asString['nome'];
              ListaRetorno.DadosRet.TituloRet.Sacado.Cidade        := AJSonObject.AsJSONObject['pagador'].asString['cidade'];
              ListaRetorno.DadosRet.TituloRet.Sacado.UF            := AJSonObject.AsJSONObject['pagador'].asString['uf'];
              ListaRetorno.DadosRet.TituloRet.Sacado.Bairro        := AJSonObject.AsJSONObject['pagador'].asString['bairro'];
              ListaRetorno.DadosRet.TituloRet.Sacado.Cep           := AJSonObject.AsJSONObject['pagador'].asString['cep'];
              ListaRetorno.DadosRet.TituloRet.Sacado.Numero        := AJSonObject.AsJSONObject['pagador'].asString['numero'];
              ListaRetorno.DadosRet.TituloRet.Sacado.Logradouro    := AJSonObject.AsJSONObject['pagador'].asString['logradouro'];
              ListaRetorno.DadosRet.TituloRet.Sacado.CNPJCPF       := AJSonObject.AsJSONObject['pagador'].asString['cpfCnpj'];

              LSituacao := AJSonObject.asString['situacao'];

              if(  LSituacao = C_CANCELADO ) or
                 ( LSituacao = C_EXPIRADO ) or
                 ( LSituacao = C_PAGO ) or
                 ( LSituacao = C_EXPIRADO ) then
                 begin
                    ListaRetorno.DadosRet.TituloRet.ValorPago                   := AJSonObject.AsCurrency['valorNominal'];
                    ListaRetorno.DadosRet.TituloRet.DataBaixa                   := DateBancoobToDateTime( AJSonObject.asString['dataHoraSituacao'])
                 end;


            end;

           end;
         end
        end;

      finally
        AJson.free;
      end;

    except
      Result := False;
    end;

  end;
end;

function TRetornoEnvio_Sicoob_V3.RetornoEnvio(const AIndex: Integer): Boolean;
begin
  Result:=inherited RetornoEnvio(AIndex);
end;

end.

