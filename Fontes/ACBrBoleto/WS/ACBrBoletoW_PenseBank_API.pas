{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo:  Ederson Selvati                                }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrBoletoW_PenseBank_API;

interface

uses
  Classes, SysUtils, ACBrBoletoWS, pcnConversao, ACBrBoletoConversao,
  Jsons;

type
  { TBoletoW_PenseBank_API }
  TBoletoW_PenseBank_API = class(TBoletoWSREST)
  private
    function CodigoTipoTitulo(AEspecieDoc:String): Integer;
  protected
    procedure DefinirURL; override;
    procedure DefinirContentType; override;
    procedure GerarHeader; override;
    procedure GerarDados; override;
    procedure DefinirAuthorization; override;
    function GerarTokenAutenticacao: string; override;
    procedure DefinirKeyUser;
    procedure DefinirAutenticacao;
    function ValidaAmbiente: Integer;
    procedure RequisicaoJson;
    procedure RequisicaoAltera;
    procedure RequisicaoBaixa;
    procedure RequisicaoConsulta;
    procedure RequisicaoConsultaLista;
    procedure RequisicaoCancelar;
    procedure GerarPagador(AJson: TJsonObject);
    procedure GerarBenificiarioFinal(AJson: TJsonObject);
    procedure GerarJuros(AJson: TJsonObject);
    procedure GerarMulta(AJson: TJsonObject);
    procedure GerarDesconto(AJson: TJsonObject);

    procedure AlteraDataVencimento(AJson: TJsonObject);
    procedure AlterarProtesto(AJson: TJsonObject);

  public
    constructor Create(ABoletoWS: TBoletoWS); override;

    function GerarRemessa: string; override;
    function Enviar: boolean; override;

  end;

const
  C_URL            = 'https://pensebank.com.br';
  C_URL_HOM        = 'https://sandbox.pensebank.com.br';

  C_ACCEPT         = 'application/json';
  C_AUTHORIZATION  = 'Authorization';
implementation

uses
  strutils, DateUtils, ACBrUtil.Strings, ACBrUtil.DateTime,
  ACBrBoleto;

{ TBoletoW_PenseBank_API }

procedure TBoletoW_PenseBank_API.DefinirURL;
var
  ID, NConvenio : String;
begin
  FPURL     := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao,C_URL, C_URL_HOM);

  if ATitulo <> nil then
    ID      := ATitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(ATitulo);

  NConvenio := OnlyNumber(Boleto.Cedente.Convenio);

  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui           : FPURL := FPURL + '/Boleto';
    tpConsulta         : FPURL := FPURL + '/BoletoConsulta';
    tpConsultaDetalhe  : FPURL := FPURL + '/BoletoConsultaLista';
    tpAltera           : begin
                           if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaAlterarVencimento then
                             FPURL := FPURL + '/BoletoProrrogacao'
                           else if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaProtestar then
                             FPURL := FPURL + '/BoletoProtesto';
                         end;
    tpBaixa            : FPURL := FPURL + '/BoletoBaixa';
    tpCancelar         : FPURL := FPURL + '/BoletoCancelamento';
  end;

end;

procedure TBoletoW_PenseBank_API.DefinirContentType;
begin
  FPContentType := 'application/json';
end;


procedure TBoletoW_PenseBank_API.GerarHeader;
begin
  DefinirContentType;
  DefinirKeyUser;
end;

procedure TBoletoW_PenseBank_API.GerarDados;
begin
  if Assigned(Boleto) then
   case Boleto.Configuracoes.WebService.Operacao of
     tpInclui:
       begin
         FMetodoHTTP:= htPOST;  // Define Método POST para Incluir
         RequisicaoJson;
       end;
     tpAltera:
       begin
         FMetodoHTTP:= htPOST;  // Define Método PATCH para alteracao
         RequisicaoAltera;
       end;
     tpBaixa :
       begin
         FMetodoHTTP:= htPOST;  // Define Método POST para Baixa
         RequisicaoBaixa;
       end;
     tpConsulta :
       begin
         FMetodoHTTP:= htGET;   //Define Método GET Consulta
         RequisicaoConsulta;
       end;
     tpConsultaDetalhe :
       begin
         FMetodoHTTP:= htGET;   //Define Método GET Consulta
         RequisicaoConsultaLista;
       end;

     tpCancelar :
       begin
         FMetodoHTTP:= htPOST;   //Define Método GET Consulta
         RequisicaoCancelar;
       end;
   else
     raise EACBrBoletoWSException.Create(ClassName + Format(
       S_OPERACAO_NAO_IMPLEMENTADO, [
       TipoOperacaoToStr(
       Boleto.Configuracoes.WebService.Operacao)]));
   end;

end;

procedure TBoletoW_PenseBank_API.DefinirAuthorization;
begin
  FPAuthorization := C_Authorization + ': ' + 'Bearer ' + GerarTokenAutenticacao;
end;

function TBoletoW_PenseBank_API.GerarTokenAutenticacao: string;
begin
  Result:= '';
  if Assigned(OAuth) then
    Result := OAuth.ClientID
  else
    raise EACBrBoletoWSException.Create(ClassName + Format( S_ERRO_GERAR_TOKEN_AUTENTICACAO, [OAuth.ErroComunicacao] ));

end;

procedure TBoletoW_PenseBank_API.DefinirKeyUser;
begin
  if Assigned(ATitulo) then
    FPKeyUser := ATitulo.ACBrBoleto.Cedente.CedenteWS.KeyUser;

end;

procedure TBoletoW_PenseBank_API.DefinirAutenticacao;
begin
  FPAuthorization := C_ACCESS_TOKEN + ': ' + GerarTokenAutenticacao;
end;

function TBoletoW_PenseBank_API.ValidaAmbiente: Integer;
begin
  Result := StrToIntDef(IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, '1','2'),2);
end;

procedure TBoletoW_PenseBank_API.RequisicaoJson;
var
  Data: string;
  Json: TJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    Json := TJsonObject.Create;
    try
      Json.Add('idexterno').Value.AsString                              := ATitulo.SeuNumero;
      Json.Add('dataEmissao').Value.AsString                            := FormatDateBr(ATitulo.DataDocumento, 'DD/MM/YYYY');
      Json.Add('dataVencimento').Value.AsString                         := FormatDateBr(ATitulo.Vencimento, 'DD/MM/YYYY');
      Json.Add('valorOriginal').Value.AsNumber                          := ATitulo.ValorDocumento;
      Json.Add('valorAbatimento').Value.AsNumber                        := ATitulo.ValorAbatimento;
      if (ATitulo.DataProtesto > 0) then
        Json.Add('quantidadeDiasProtesto').Value.AsInteger              := Trunc(ATitulo.DataProtesto - ATitulo.Vencimento);
      if (ATitulo.DiasDeNegativacao > 0) then
      begin
        Json.Add('quantidadeDiasNegativacao').Value.AsInteger           := ATitulo.DiasDeNegativacao;
        Json.Add('orgaoNegativador').Value.AsInteger                    := StrToInt64Def(ATitulo.orgaoNegativador,0);
      end;
      if (ATitulo.DataLimitePagto > 0 ) then
      begin
        Json.Add('indicadorAceiteTituloVencido').Value.AsString         := 'S';
        Json.Add('numeroDiasLimiteRecebimento').Value.AsInteger         := Trunc(ATitulo.DataLimitePagto - ATitulo.Vencimento);
      end;
      Json.Add('codigoAceite').Value.AsString                           := IfThen(ATitulo.Aceite = atSim,'A','N');
      Json.Add('codigoTipoTitulo').Value.AsInteger                      := codigoTipoTitulo(ATitulo.EspecieDoc);
      Json.Add('descricaoTipoTitulo').Value.AsString                    := ATitulo.EspecieDoc;
      if ATitulo.TipoPagamento = tpAceita_Qualquer_Valor then
        Json.Add('indicadorPermissaoRecebimentoParcial').Value.AsString := 'S';

      Json.Add('numeroTituloBeneficiario').Value.AsString               := Copy(Trim(UpperCase(ATitulo.NumeroDocumento)),0,15);
      Json.Add('campoUtilizacaoBeneficiario').Value.AsString            := Copy(Trim(StringReplace(UpperCase(ATitulo.Mensagem.Text),'\r\n',' ',[rfReplaceAll])),0,30);
      Json.Add('numeroTituloCliente').Value.AsString                    := Boleto.Banco.MontarCampoNossoNumero(ATitulo);
      Json.Add('mensagemBloquetoOcorrencia').Value.AsString             := UpperCase(Copy(Trim(ATitulo.Instrucao1 +' '+ATitulo.Instrucao2+' '+ATitulo.Instrucao3),0,165));

      GerarDesconto(Json);
      GerarJuros(Json);
      GerarMulta(Json);
      GerarPagador(Json);
      GerarBenificiarioFinal(Json);

      Json.Add('cnpjSh').Value.AsString := FPKeyUser;
      Json.Add('pix').Value.AsBoolean := Boleto.Cedente.CedenteWS.IndicadorPix;
      Json.Add('emailGeracao').Value.AsBoolean := Boleto.Cedente.CedenteWS.IndicadorEmail;
      Json.Add('sms').Value.AsBoolean := Boleto.Cedente.CedenteWS.IndicadorSMS;

      Data := Json.Stringify;
      FPDadosMsg := Data;
    finally
      Json.Free;
    end;
  end;
end;

procedure TBoletoW_PenseBank_API.RequisicaoAltera;
var
  Data: string;
  Json: TJSONObject;
begin
  if Assigned(ATitulo) then
  begin

    Json := TJsonObject.Create;
    try
      case Integer(ATitulo.OcorrenciaOriginal.Tipo) of
        7: //RemessaAlterarVencimento
          begin
            AlteraDataVencimento(Json);
          end;
        9:  //RemessaProtestar
          begin
            AlterarProtesto(Json);
          end;
      end;
      Data := Json.Stringify;
      FPDadosMsg := Data;
    finally
      Json.Free;
    end;
  end;

end;

procedure TBoletoW_PenseBank_API.RequisicaoBaixa;
var
  Data: string;
  Json: TJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    Json := TJsonObject.Create;
    try
      Json.Add('idboleto').Value.AsString := ATitulo.NumeroDocumento;
      Json.Add('numeroTituloCliente').Value.AsString := ATitulo.NossoNumero;
      Data := Json.Stringify;

      FPDadosMsg := Data;

    finally
      Json.Free;
    end;
  end;

end;

procedure TBoletoW_PenseBank_API.RequisicaoCancelar;
var
  Data: string;
  Json: TJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    Json := TJsonObject.Create;
    try
      Json.Add('idboleto').Value.AsString := ATitulo.NumeroDocumento;
      Json.Add('numeroTituloCliente').Value.AsString := ATitulo.NossoNumero;
      Data := Json.Stringify;
      FPDadosMsg := Data;
    finally
      Json.Free;
    end;
  end;
end;

procedure TBoletoW_PenseBank_API.RequisicaoConsulta;
var
  Data: string;
  Json: TJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    Json := TJsonObject.Create;
    try
      Json.Add('idboleto').Value.AsString := ATitulo.NumeroDocumento;
      Json.Add('numeroTituloCliente').Value.AsString := ATitulo.NossoNumero;
      Data := Json.Stringify;

      FPDadosMsg := Data;

    finally
      Json.Free;
    end;
  end;
end;

procedure TBoletoW_PenseBank_API.RequisicaoConsultaLista;
begin
//
end;

procedure TBoletoW_PenseBank_API.GerarPagador(AJson: TJsonObject);
var
  JsonDadosPagador: TJsonObject;
  JsonPairPagador: TJsonPair;

begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonDadosPagador := TJSONObject.Create;
      try
        JsonDadosPagador.Add('tipoInscricao').Value.AsInteger   := StrToInt(IfThen(Length( OnlyNumber(ATitulo.Sacado.CNPJCPF)) = 11,'1','2'));
        JsonDadosPagador.Add('numeroInscricao').Value.AsString  := OnlyNumber(ATitulo.Sacado.CNPJCPF);
        JsonDadosPagador.Add('nome').Value.AsString             := ATitulo.Sacado.NomeSacado;
        JsonDadosPagador.Add('endereco').Value.AsString         := ATitulo.Sacado.Logradouro + ' ' + ATitulo.Sacado.Numero;
        JsonDadosPagador.Add('cep').Value.AsInteger             := StrToInt(OnlyNumber(ATitulo.Sacado.CEP));
        JsonDadosPagador.Add('cidade').Value.AsString           := ATitulo.Sacado.Cidade;
        JsonDadosPagador.Add('bairro').Value.AsString           := ATitulo.Sacado.Bairro;
        JsonDadosPagador.Add('uf').Value.AsString               := ATitulo.Sacado.UF;
        JsonDadosPagador.Add('telefone').Value.AsString         := ATitulo.Sacado.Fone;
        JsonDadosPagador.Add('email').Value.AsString            := ATitulo.Sacado.Email;

        JsonPairPagador := TJsonPair.Create(AJson, 'pagador');
        try
          JsonPairPagador.Value.AsObject := JsonDadosPagador;
          AJson.Add('pagador').Assign(JsonPairPagador);
        finally
          JsonPairPagador.Free;
        end;
      finally
        JsonDadosPagador.Free;
      end;
    end;

  end;

end;

procedure TBoletoW_PenseBank_API.GerarBenificiarioFinal(AJson: TJsonObject);
var
  JsonSacadorAvalista: TJsonObject;
  JsonPairSacadorAvalista: TJsonPair;

begin
  if Assigned(ATitulo) then
  begin
      if ATitulo.Sacado.SacadoAvalista.CNPJCPF = EmptyStr then
        Exit;

      if Assigned(AJson) then
      begin
        JsonSacadorAvalista := TJSONObject.Create;

        try
          JsonSacadorAvalista.Add('tipoInscricao').Value.AsInteger   :=  StrToInt(IfThen( Length( OnlyNumber(ATitulo.Sacado.SacadoAvalista.CNPJCPF)) = 11,'1','2'));
          JsonSacadorAvalista.Add('numeroInscricao').Value.AsNumber  :=  StrToInt64Def(OnlyNumber(ATitulo.Sacado.SacadoAvalista.CNPJCPF),0);
          JsonSacadorAvalista.Add('nome').Value.AsString             :=  ATitulo.Sacado.SacadoAvalista.NomeAvalista;

          JsonPairSacadorAvalista := TJsonPair.Create(AJson, 'beneficiarioFinal');
          try
            JsonPairSacadorAvalista.Value.AsObject := JsonSacadorAvalista;
            AJson.Add('beneficiarioFinal').Assign(JsonPairSacadorAvalista);
          finally
            JsonPairSacadorAvalista.Free;
          end;
        finally
          JsonSacadorAvalista.Free;
        end;
      end;

    end;
end;

procedure TBoletoW_PenseBank_API.GerarJuros(AJson: TJsonObject);
var
  JsonJuros: TJsonObject;
  JsonPairJuros: TJsonPair;

begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonJuros := TJSONObject.Create;
      try
        if (ATitulo.ValorMoraJuros > 0) then
        begin
          JsonJuros.Add('tipo').Value.AsInteger             := StrToIntDef(ATitulo.CodigoMora, 3);
          case (StrToIntDef(ATitulo.CodigoMora, 3)) of
            1 : JsonJuros.Add('valor').Value.AsNumber       := ATitulo.ValorMoraJuros;
            2 : JsonJuros.Add('porcentagem').Value.AsNumber := ATitulo.ValorMoraJuros;
          end;

          JsonPairJuros := TJsonPair.Create(AJson, 'jurosMora');
          try
            JsonPairJuros.Value.AsObject := JsonJuros;
            AJson.Add('jurosMora').Assign(JsonPairJuros);
          finally
            JsonPairJuros.Free;
          end;
        end;
      finally
        JsonJuros.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_PenseBank_API.GerarMulta(AJson: TJsonObject);
var
  JsonMulta: TJsonObject;
  JsonPairMulta: TJsonPair;
  ACodMulta: integer;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonMulta := TJSONObject.Create;

      try
        if ATitulo.PercentualMulta > 0 then
        begin
          if ATitulo.MultaValorFixo then
            ACodMulta := 1
          else
            ACodMulta := 2;
        end
        else
          ACodMulta := 3;


        if (ATitulo.DataMulta > 0) then
        begin
          JsonMulta.Add('tipo').Value.AsInteger             := ACodMulta;
          JsonMulta.Add('data').Value.AsString              := FormatDateBr(ATitulo.DataMulta, 'DD.MM.YYYY');
          case ACodMulta of
            1 : JsonMulta.Add('valor').Value.AsNumber       := ATitulo.PercentualMulta;
            2 : JsonMulta.Add('porcentagem').Value.AsNumber := ATitulo.PercentualMulta;
          end;

          JsonPairMulta := TJsonPair.Create(AJson, 'multa');
          try
            JsonPairMulta.Value.AsObject := JsonMulta;
            AJson.Add('multa').Assign(JsonPairMulta);
          finally
            JsonPairMulta.Free;
          end;
        end;
      finally
        JsonMulta.Free;
      end;
    end;
  end;
end;

procedure TBoletoW_PenseBank_API.GerarDesconto(AJson: TJsonObject);
var
  JsonDesconto: TJsonObject;
  JsonPairDesconto: TJsonPair;
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonDesconto := TJSONObject.Create;
      try

        if (ATitulo.DataDesconto > 0) then
        begin
          JsonDesconto.Add('tipo').Value.AsInteger             := integer(ATitulo.TipoDesconto);
          JsonDesconto.Add('dataExpiracao').Value.AsString     := FormatDateBr(ATitulo.DataDesconto, 'DD.MM.YYYY');
          case integer(ATitulo.TipoDesconto) of
            1 : JsonDesconto.Add('valor').Value.AsNumber       := ATitulo.ValorDesconto;
            2 : JsonDesconto.Add('porcentagem').Value.AsNumber := ATitulo.ValorDesconto;
          end;

          JsonPairDesconto := TJsonPair.Create(AJson, 'desconto');
          try
            JsonPairDesconto.Value.AsObject := JsonDesconto;
            AJson.Add('desconto').Assign(JsonPairDesconto);
          finally
            JsonPairDesconto.Free;
          end;
        end;
      finally
        JsonDesconto.Free;
      end;
    end;
  end;

end;

procedure TBoletoW_PenseBank_API.AlteraDataVencimento(AJson: TJsonObject);
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      if (ATitulo.Vencimento > 0) then
      begin
        AJson.Add('idboleto').Value.AsString := ATitulo.NumeroDocumento;
        AJson.Add('dataVencimento').Value.AsString := FormatDateBr(ATitulo.Vencimento, 'DD/MM/YYYY');
        AJson.Add('numeroTituloCliente').Value.AsString := ATitulo.NossoNumero;
      end;
    end;

  end;

end;

procedure TBoletoW_PenseBank_API.AlterarProtesto(AJson: TJsonObject);
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      if (ATitulo.DiasDeProtesto > 0) then
      begin
        AJson.Add('idboleto').Value.AsString := ATitulo.NumeroDocumento;
        AJson.Add('numeroTituloCliente').Value.AsString := ATitulo.NossoNumero;
        AJson.Add('quantidadeDiasProtesto').Value.AsInteger := ATitulo.DiasDeProtesto;
      end;

    end;
  end;
end;

function TBoletoW_PenseBank_API.CodigoTipoTitulo(AEspecieDoc : String): Integer;
begin
{ Pegando o tipo de EspecieDoc }
  if AEspecieDoc = 'CH' then
    AEspecieDoc   := '01'
  else if AEspecieDoc = 'DM' then
    AEspecieDoc   := '02'
  else if AEspecieDoc = 'DMI' then
    AEspecieDoc   := '03'
  else if AEspecieDoc = 'DS' then
    AEspecieDoc   := '04'
  else if AEspecieDoc = 'DSI' then
    AEspecieDoc   := '05'
  else if AEspecieDoc = 'DR' then
    AEspecieDoc   := '06'
  else if AEspecieDoc = 'LC' then
    AEspecieDoc   := '07'
  else if AEspecieDoc = 'NCC' then
    AEspecieDoc   := '08'
  else if AEspecieDoc = 'NCE' then
    AEspecieDoc   := '09'
  else if AEspecieDoc = 'NCI' then
    AEspecieDoc   := '10'
  else if AEspecieDoc = 'NCR' then
    AEspecieDoc   := '11'
  else if AEspecieDoc = 'NP' then
    AEspecieDoc   := '12'
  else if AEspecieDoc = 'NPR' then
    AEspecieDoc   := '13'
  else if AEspecieDoc = 'TM' then
    AEspecieDoc   := '14'
  else if AEspecieDoc = 'TS' then
    AEspecieDoc   := '15'
  else if AEspecieDoc = 'NS' then
    AEspecieDoc   := '16'
  else if AEspecieDoc = 'RC' then
    AEspecieDoc   := '17'
  else if AEspecieDoc = 'FAT' then
    AEspecieDoc   := '18'
  else if AEspecieDoc = 'ND' then
    AEspecieDoc   := '19'
  else if AEspecieDoc = 'AP' then
    AEspecieDoc   := '20'
  else if AEspecieDoc = 'ME' then
    AEspecieDoc   := '21'
  else if AEspecieDoc = 'PC' then
    AEspecieDoc   := '22';
  Result := StrToIntDef(AEspecieDoc,0);
end;

constructor TBoletoW_PenseBank_API.Create(ABoletoWS: TBoletoWS);
begin
  inherited Create(ABoletoWS);

  FPAccept := C_ACCEPT;
end;

function TBoletoW_PenseBank_API.GerarRemessa: string;
begin
  Result := inherited GerarRemessa;

end;

function TBoletoW_PenseBank_API.Enviar: boolean;
begin
  Result := inherited Enviar;

end;
end.
