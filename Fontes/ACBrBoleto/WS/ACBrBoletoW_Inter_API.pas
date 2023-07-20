{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo:  Victor Hugo Gonzales - Panda, Leandro do Couto,}
{ Delmar de Lima                                                               }
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
unit ACBrBoletoW_Inter_API;

interface

uses
  Classes, SysUtils, ACBrBoletoWS, pcnConversao, ACBrBoletoConversao,
  synacode, strutils, DateUtils, ACBrDFeSSL, synautil, ACBrBoleto, Jsons, httpsend,
  ACBrBoletoWS.Rest;

type

  { TBoletoW_Inter_API }
  TBoletoW_Inter_API = class(TBoletoWSREST)
  private
    function CodigoTipoTitulo(AEspecieDoc: String): String;
    function DateIntertoDateTime(const AValue: String): TDateTime;
    function DateTimeToDateInter( const AValue:TDateTime ):String;
  protected

    procedure DefinirURL; override;
    procedure DefinirContentType; override;
    procedure GerarHeader; override;
    procedure GerarDados; override;
    procedure DefinirAuthorization; override;
    function GerarTokenAutenticacao: string; override;
    function DefinirParametros: String;
    procedure DefinirParamOAuth; override;
    procedure DefinirKeyUser;
    procedure DefinirAutenticacao;
    function ValidaAmbiente: Integer;
    procedure RequisicaoJson;
    procedure RequisicaoAltera;
    procedure RequisicaoBaixa;
    procedure RequisicaoConsulta;
    procedure RequisicaoConsultaDetalhe;
    procedure GerarPagador(AJson: TJsonObject);
    procedure GerarBenificiarioFinal(AJson: TJsonObject);
    procedure GerarJuros(AJson: TJsonObject);
    procedure GerarMulta(AJson: TJsonObject);
    procedure GerarDesconto(AJson: TJsonObject);
    procedure GeraDadosInstrucao(AJson: TJsonObject);

    procedure AlteraDataVencimento(AJson: TJsonObject);
    procedure AtribuirDesconto(AJson: TJsonObject);
    procedure AlteracaoDesconto(AJson: TJsonObject);
    procedure AlteracaoDataDesconto(AJson: TJsonObject);
    procedure AlterarProtesto(AJson: TJsonObject);
    procedure AtribuirAbatimento(AJson: TJsonObject);
    procedure AlteracaoAbatimento(AJson: TJsonObject);
    procedure AtribuirJuros(AJson: TJsonObject);
    procedure AtribuirMulta(AJson: TJsonObject);
    procedure AtribuirNegativacao(AJson: TJsonObject);
    procedure AlteracaoSeuNumero(AJson: TJsonObject);
    procedure AlteracaoEnderecoPagador(AJson: TJsonObject);
    procedure AlteracaoPrazo(AJson: TJsonObject);

  public
    constructor Create(ABoletoWS: TBoletoWS); override;

    function GerarRemessa: string; override;
    function Enviar: boolean; override;

  end;

const
  C_URL             = 'https://cdpj.partners.bancointer.com.br/cobranca/v2';
  C_URL_HOM         = 'https://cdpj.partners.bancointer.com.br/cobranca/v2';

  C_URL_OAUTH_PROD  = 'https://cdpj.partners.bancointer.com.br/oauth/v2/token';
  C_URL_OAUTH_HOM   = 'https://cdpj.partners.bancointer.com.br/oauth/v2/token';

  C_CONTENT_TYPE    = 'application/json';
  C_ACCEPT          = 'application/json';
  C_AUTHORIZATION   = 'Authorization';

  C_ACCEPT_ENCODING = 'gzip, deflate, br';

  C_CHARSET         = 'utf-8';
  C_ACCEPT_CHARSET  = 'utf-8';


implementation

uses
  ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrUtil.DateTime, ACBrUtil.Base;

{ TBoletoW_Inter_API }

procedure TBoletoW_Inter_API.DefinirURL;
var
  aNossoNumero: string;
begin

  if( aTitulo <> nil ) then
  aNossoNumero := PadLeft(aTitulo.NossoNumero, 11, '0');

  FPURL := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, C_URL,C_URL_HOM);

  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui:
      FPURL := FPURL + '/boletos';
    tpConsulta:
      FPURL := FPURL + '/boletos?'+DefinirParametros;
    tpAltera:
      FPURL := FPURL + '/comandoInstrucao';
    tpConsultaDetalhe:
      FPURL := FPURL + '/boletos/'+aNossoNumero;
    tpBaixa:
      FPURL := FPURL + '/boletos/' + aNossoNumero + '/cancelar';
  end;

end;

procedure TBoletoW_Inter_API.DefinirContentType;
begin
  FPContentType := C_CONTENT_TYPE;
end;

procedure TBoletoW_Inter_API.GerarHeader;
begin
  DefinirContentType;
  DefinirKeyUser;
end;

procedure TBoletoW_Inter_API.GerarDados;
begin
  if Assigned(Boleto) then

    DefinirURL;

  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui:
      begin
        FMetodoHTTP := htPOST; // Define Método POST para Incluir
        RequisicaoJson;
      end;
    tpAltera:
      begin
        raise EACBrBoletoWSException.Create
          (ClassName + Format(S_OPERACAO_NAO_IMPLEMENTADO,
          [TipoOperacaoToStr(Boleto.Configuracoes.WebService.Operacao)]));
      end;
    tpBaixa:
      begin
        FMetodoHTTP := htPOST; // Define Método POST para Baixa
        RequisicaoBaixa;
      end;
    tpConsulta:
      begin
        FMetodoHTTP := htGET; // Define Método GET Consulta
        RequisicaoConsulta;
      end;
    tpConsultaDetalhe:
      begin
        FMetodoHTTP := htGET; // Define Método GET Consulta Detalhe
        RequisicaoConsultaDetalhe;
      end;

  else
    raise EACBrBoletoWSException.Create
      (ClassName + Format(S_OPERACAO_NAO_IMPLEMENTADO,
      [TipoOperacaoToStr(Boleto.Configuracoes.WebService.Operacao)]));
  end;

end;

procedure TBoletoW_Inter_API.DefinirAuthorization;
begin
  FPAuthorization := C_AUTHORIZATION + ': Bearer ' + GerarTokenAutenticacao;
end;

function TBoletoW_Inter_API.GerarTokenAutenticacao: string;
begin
  OAuth.Payload := True;
  Result := inherited GerarTokenAutenticacao;
end;

procedure TBoletoW_Inter_API.RequisicaoBaixa;
var
  Stream: TMemoryStream;
  aStr: String;
  Json: TJsonObject;
begin
  FPContentType := 'x-www-form-urlencoded';
  Json := TJsonObject.Create;
  try
    Json.Add('motivoCancelamento').Value.asString := 'PAGODIRETOAOCLIENTE';
    FPDadosMsg := Json.Stringify;
  finally
    Json.Free;
  end;
end;

procedure TBoletoW_Inter_API.DefinirKeyUser;
begin
  if Assigned(aTitulo) then
    FPKeyUser := '';
end;

function TBoletoW_Inter_API.DefinirParametros: String;
var
  Consulta: TStringList;
  Documento: String;
begin
  if Assigned(Boleto.Configuracoes.WebService.Filtro) then
  begin
    Documento := OnlyNumber
      (Boleto.Configuracoes.WebService.Filtro.cnpjCpfPagador);

    Consulta := TStringList.Create;
    Consulta.Delimiter := '&';
    try

      Consulta.Add( 'itensPorPagina=1000' );

      if Boleto.Configuracoes.WebService.Filtro.indiceContinuidade > 0 then
        Consulta.Add('paginaAtual='+ FloatToStr(Boleto.Configuracoes.WebService.Filtro.indiceContinuidade));

      case Boleto.Configuracoes.WebService.Filtro.indicadorSituacao of
        isbBaixado:
          begin
            Consulta.Add( 'filtrarDataPor=SITUACAO' );
            Consulta.Add('situacao=PAGO,CANCELADO');
            Consulta.Add('dataInicial=' +DateTimeToDateInter(Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataInicio));
            Consulta.Add('dataFinal=' +DateTimeToDateInter(Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataFinal));
            Consulta.Add( 'ordenarPor=DATASITUACAO' );
          end;
        isbAberto:
          begin

            if Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataInicio > 0 then
            begin
              Consulta.Add( 'filtrarDataPor=VENCIMENTO' );
              Consulta.Add('situacao=EMABERTO,VENCIDO');
              Consulta.Add('dataInicial=' +DateTimeToDateInter(Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataInicio));
              Consulta.Add('dataFinal=' +DateTimeToDateInter(Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataFinal));
              Consulta.Add( 'ordenarPor=DATAVENCIMENTO' );
            end;

            if Boleto.Configuracoes.WebService.Filtro.dataRegistro.DataInicio > 0
            then
            begin
              Consulta.Add( 'filtrarDataPor=EMISSAO' );
              Consulta.Add('situacao=EMABERTO,VENCIDO');
              Consulta.Add('dataInicial=' +DateTimeToDateInter(Boleto.Configuracoes.WebService.Filtro.dataRegistro.DataInicio));
              Consulta.Add('dataFinal=' +DateTimeToDateInter(Boleto.Configuracoes.WebService.Filtro.DataRegistro.DataFinal));
              Consulta.Add( 'ordenarPor=DATASITUACAO' );
            end;
          end;
      end;

    finally
      result := Consulta.DelimitedText;
      Consulta.Free;
    end;

  end;

end;

procedure TBoletoW_Inter_API.DefinirParamOAuth;
begin
  FParamsOAuth := Format( 'client_id=%s&client_secret=%s&scope=%s&grant_type=client_credentials',
                   [Boleto.Cedente.CedenteWS.ClientID,
                    Boleto.Cedente.CedenteWS.ClientSecret,
                    Boleto.Cedente.CedenteWS.Scope] );
end;

function TBoletoW_Inter_API.DateIntertoDateTime(
  const AValue: String): TDateTime;
begin
  Result := StrToDateDef( StringReplace( AValue,'.','/', [rfReplaceAll] ),0);
end;

function TBoletoW_Inter_API.DateTimeToDateInter(
  const AValue: TDateTime): String;
begin
  result := FormatDateBr( aValue, 'YYYY-MM-DD');
end;

procedure TBoletoW_Inter_API.DefinirAutenticacao;
begin
end;

function TBoletoW_Inter_API.ValidaAmbiente: Integer;
begin
  result := StrToIntDef(IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, '1','2'), 2);
end;

procedure TBoletoW_Inter_API.RequisicaoJson;
var
  Data: string;
  Json: TJsonObject;
  aSeuNumero: String;
begin
  if Assigned(aTitulo) then
  begin
    Json := TJsonObject.Create;
    try

      aSeuNumero := ATitulo.SeuNumero;

      Json.Add('seuNumero').Value.asString := aSeuNumero;
      Json.Add('valorNominal').Value.asNumber := aTitulo.ValorDocumento;
      Json.Add('dataVencimento').Value.asString := DateTimeToDateInter(aTitulo.Vencimento);
      Json.Add('numDiasAgenda').Value.asNumber := 60;

      GerarDesconto(Json);
      GerarJuros(Json);
      GerarMulta(Json);
      GerarPagador(Json);
      GerarBenificiarioFinal(Json);

      Data := Json.Stringify;

      FPDadosMsg := Data;

    finally
      Json.Free;
    end;
  end;
end;

procedure TBoletoW_Inter_API.RequisicaoAltera;
begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.GeraDadosInstrucao(AJson: TJsonObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.RequisicaoConsulta;
begin
  // Sem Payload - Define Método GET
end;

procedure TBoletoW_Inter_API.RequisicaoConsultaDetalhe;
begin
  // Sem Payload - Define Método GET
end;

procedure TBoletoW_Inter_API.GerarPagador(AJson: TJsonObject);
var
  JsonDadosPagador: TJsonObject;
  JsonPairPagador: TJSONPair;

begin
  if Assigned(aTitulo) then
  begin
    if Assigned(AJson) then
    begin

      JsonDadosPagador := TJsonObject.Create;

      try

        JsonDadosPagador.Add('cpfCnpj').Value.asString :=OnlyNumber(aTitulo.Sacado.CNPJCPF);
        JsonDadosPagador.Add('tipoPessoa').Value.asString :=IfThen(Length(OnlyNumber(aTitulo.Sacado.CNPJCPF)) = 11, 'FISICA','JURIDICA');
        JsonDadosPagador.Add('nome').Value.asString :=aTitulo.Sacado.NomeSacado;
        JsonDadosPagador.Add('endereco').Value.asString :=aTitulo.Sacado.Logradouro;
        JsonDadosPagador.Add('numero').Value.asString := aTitulo.Sacado.Numero;
        JsonDadosPagador.Add('complemento').Value.asString :=aTitulo.Sacado.Complemento;
        JsonDadosPagador.Add('bairro').Value.asString := aTitulo.Sacado.Bairro;
        JsonDadosPagador.Add('cep').Value.asString := OnlyNumber(aTitulo.Sacado.CEP);
        JsonDadosPagador.Add('cidade').Value.asString := aTitulo.Sacado.Cidade;
        JsonDadosPagador.Add('uf').Value.asString := aTitulo.Sacado.UF;
        JsonDadosPagador.Add('telefone').Value.asString := IfThen(aTitulo.Sacado.Fone = '', '0', aTitulo.Sacado.Fone);
        JsonDadosPagador.Add('email').Value.asString := aTitulo.Sacado.Email;

        JsonPairPagador := TJSONPair.Create(AJson, 'pagador');
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

procedure TBoletoW_Inter_API.GerarBenificiarioFinal(AJson: TJsonObject);

var
  JsonSacadorAvalista: TJsonObject;
  JsonPairSacadorAvalista: TJSONPair;

begin

  if Assigned(aTitulo) then
  begin
    if aTitulo.Sacado.SacadoAvalista.CNPJCPF = EmptyStr then
      Exit;

    if Assigned(AJson) then
    begin
      JsonSacadorAvalista := TJsonObject.Create;

      try
        JsonSacadorAvalista.Add('nome').Value.asString :=
          aTitulo.Sacado.SacadoAvalista.NomeAvalista;
        JsonSacadorAvalista.Add('cpfCnpj').Value.asString :=
          OnlyNumber(aTitulo.Sacado.SacadoAvalista.CNPJCPF);
        JsonSacadorAvalista.Add('tipoPessoa').Value.asString :=
          IfThen(Length(OnlyNumber(aTitulo.Sacado.SacadoAvalista.CNPJCPF)) = 11,
          'FISICA', 'JURIDICA');
        JsonSacadorAvalista.Add('cep').Value.asString :=
          aTitulo.Sacado.SacadoAvalista.CEP;
        JsonSacadorAvalista.Add('endereco').Value.asString :=
          aTitulo.Sacado.SacadoAvalista.Logradouro;
        JsonSacadorAvalista.Add('bairro').Value.asString :=
          aTitulo.Sacado.SacadoAvalista.Bairro;
        JsonSacadorAvalista.Add('cidade').Value.asString :=
          aTitulo.Sacado.SacadoAvalista.Cidade;
        JsonSacadorAvalista.Add('uf').Value.asString :=
          aTitulo.Sacado.SacadoAvalista.UF;

        JsonPairSacadorAvalista := TJSONPair.Create(AJson, 'beneficiarioFinal');
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

procedure TBoletoW_Inter_API.GerarJuros(AJson: TJsonObject);
var
  JsonJuros: TJsonObject;
  JsonPairJuros: TJSONPair;

begin
  if Assigned(aTitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonJuros := TJsonObject.Create;
      try
        if (aTitulo.ValorMoraJuros > 0) then
        begin

          if ATitulo.CodigoMora = '' then
          begin
            case aTitulo.CodigoMoraJuros of
              cjValorDia: aTitulo.CodigoMora := '1';
              cjTaxaMensal: aTitulo.CodigoMora := '2';
              cjIsento: aTitulo.CodigoMora := '3';
            end;
          end;

          case (StrToIntDef(aTitulo.CodigoMora, 3)) of
            1:
              begin
                JsonJuros.Add('codigoMora').Value.asString := 'VALORDIA';
                JsonJuros.Add('data').Value.asString := DateTimeToDateInter(aTitulo.DataMulta);
                JsonJuros.Add('valor').Value.asNumber := aTitulo.ValorMoraJuros;
                JsonJuros.Add('taxa').Value.asNumber := 0;
              end;
            2:
              begin
                JsonJuros.Add('codigoMora').Value.asString := 'TAXAMENSAL';
                JsonJuros.Add('data').Value.asString := DateTimeToDateInter(aTitulo.DataMulta);
                JsonJuros.Add('taxa').Value.asNumber := aTitulo.ValorMoraJuros;
                JsonJuros.Add('valor').Value.asNumber := 0;
              end;
            3:
              begin
                JsonJuros.Add('codigoMora').Value.asString := 'ISENTO';
                JsonJuros.Add('taxa').Value.asNumber := 0;
                JsonJuros.Add('valor').Value.asNumber := 0;
              end;
          end;

          JsonPairJuros := TJSONPair.Create(AJson, 'mora');
          try
            JsonPairJuros.Value.AsObject := JsonJuros;
            AJson.Add('mora').Assign(JsonPairJuros);


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

procedure TBoletoW_Inter_API.GerarMulta(AJson: TJsonObject);
var
  JsonMulta: TJsonObject;
  JsonPairMulta: TJSONPair;
  ACodMulta: Integer;
begin
  if Assigned(aTitulo) then
  begin
    if Assigned(AJson) then
    begin
      JsonMulta := TJsonObject.Create;

      try
        if aTitulo.PercentualMulta > 0 then
          begin
            if aTitulo.MultaValorFixo then
              ACodMulta := 1
            else
              ACodMulta := 2;
          end
        else
            ACodMulta := 3;

        if (aTitulo.DataMulta > 0) then
        begin

          case ACodMulta of
            1:
              begin
                JsonMulta.Add('codigoMulta').Value.asString := 'VALORFIXO';
                JsonMulta.Add('data').Value.asString :=DateTimeToDateInter(aTitulo.DataMulta);
                JsonMulta.Add('valor').Value.asNumber :=aTitulo.PercentualMulta;
                JsonMulta.Add('taxa').Value.asNumber :=0;
              end;
            2:
              begin
                JsonMulta.Add('codigoMulta').Value.asString := 'PERCENTUAL';
                JsonMulta.Add('data').Value.asString := DateTimeToDateInter(aTitulo.DataMulta);
                JsonMulta.Add('taxa').Value.asNumber := aTitulo.PercentualMulta;
                JsonMulta.Add('valor').Value.asNumber :=0;
              end;
            3:
              begin
                JsonMulta.Add('codigoMulta').Value.asString := 'NAOTEMMULTA';
                JsonMulta.Add('valor').Value.asNumber := 0;
                JsonMulta.Add('taxa').Value.asNumber := 0;
              end;

          end;

          JsonPairMulta := TJSONPair.Create(AJson, 'multa');
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

procedure TBoletoW_Inter_API.GerarDesconto(AJson: TJsonObject);
var
  JsonDesconto: TJsonObject;
  JsonPairDesconto: TJSONPair;
begin
  if Assigned(aTitulo) then
  begin
    if Assigned(AJson) then
    begin

      JsonDesconto := TJsonObject.Create;

      try

        if (aTitulo.DataDesconto > 0) then
        begin
          JsonDesconto := TJsonObject.Create;
          JsonDesconto.Add('data').Value.asString :=
            DateTimeToDateInter(aTitulo.DataDesconto );
          case Integer(aTitulo.TipoDesconto) of
            1:
              begin
                JsonDesconto.Add('codigoDesconto').Value.asString := 'VALORFIXODATAINFORMADA';
                JsonDesconto.Add('valor').Value.asNumber := aTitulo.ValorDesconto;
                 JsonDesconto.Add('taxa').Value.asNumber := 0;
              end;
            2:
              begin
                JsonDesconto.Add('codigoDesconto').Value.asString := 'PERCENTUALDATAINFORMADA';
                JsonDesconto.Add('taxa').Value.asNumber := aTitulo.ValorDesconto;
                JsonDesconto.Add('valor').Value.asNumber := 0;
              end;
          else
            begin
              JsonDesconto.Add('codigoDesconto').Value.asString := 'NAOTEMDESCONTO';
              JsonDesconto.Add('valor').Value.asNumber := 0;
              JsonDesconto.Add('taxa').Value.asNumber := 0;
            end;
          end;

        end
        else
        begin
          JsonDesconto.Add('codigoDesconto').Value.asString := 'NAOTEMDESCONTO';
          JsonDesconto.Add('valor').Value.asNumber := 0;
          JsonDesconto.Add('taxa').Value.asNumber := 0;
        end;

        JsonPairDesconto := TJSONPair.Create(AJson, 'desconto1');
        try
          JsonPairDesconto.Value.AsObject := JsonDesconto;
          AJson.Add('desconto1').Assign(JsonPairDesconto);
        finally
          JsonPairDesconto.Free;
        end;

        if (aTitulo.DataDesconto2 > 0) then
        begin
          JsonDesconto := TJsonObject.Create;
          JsonDesconto.Add('data').Value.asString := DateTimeToDateInter(aTitulo.DataDesconto2);
          case Integer(aTitulo.TipoDesconto) of
            1:
              begin
                JsonDesconto.Add('codigoDesconto').Value.asString := 'VALORFIXODATAINFORMADA';
                JsonDesconto.Add('valor').Value.asNumber := aTitulo.ValorDesconto2;
                JsonDesconto.Add('taxa').Value.asNumber := 0;
              end;
            2:
              begin
                JsonDesconto.Add('codigoDesconto').Value.asString := 'PERCENTUALDATAINFORMADA';
                JsonDesconto.Add('taxa').Value.asNumber := aTitulo.ValorDesconto2;
                JsonDesconto.Add('valor').Value.asNumber := 0;
              end;
          else
            begin
              JsonDesconto.Add('codigoDesconto').Value.asString := 'NAOTEMDESCONTO';
              JsonDesconto.Add('valor').Value.asNumber := 0;
              JsonDesconto.Add('taxa').Value.asNumber := 0;
            end;
          end;

          JsonPairDesconto := TJSONPair.Create(AJson, 'desconto2');

          try
            JsonPairDesconto.Value.AsObject := JsonDesconto;
            AJson.Add('desconto2').Assign(JsonPairDesconto);
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

procedure TBoletoW_Inter_API.AlteraDataVencimento(AJson: TJsonObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.AtribuirDesconto(AJson: TJsonObject);
begin

  if Assigned(aTitulo) then
  begin

    if Assigned(AJson) then
    begin
      AJson.Add('valor1').Value.asNumber := aTitulo.ValorDesconto;
    end;

  end;

end;

procedure TBoletoW_Inter_API.AlteracaoDesconto(AJson: TJsonObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.AlteracaoDataDesconto(AJson: TJsonObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.AlterarProtesto(AJson: TJsonObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.AtribuirAbatimento(AJson: TJsonObject);

begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.AlteracaoAbatimento(AJson: TJsonObject);

begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.AtribuirJuros(AJson: TJsonObject);

begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.AtribuirMulta(AJson: TJsonObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.AtribuirNegativacao(AJson: TJsonObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.AlteracaoSeuNumero(AJson: TJsonObject);

begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.AlteracaoEnderecoPagador(AJson: TJsonObject);

begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.AlteracaoPrazo(AJson: TJsonObject);

begin
  // sem Payload
end;

function TBoletoW_Inter_API.CodigoTipoTitulo(AEspecieDoc: String): String;
begin
end;

constructor TBoletoW_Inter_API.Create(ABoletoWS: TBoletoWS);
begin
  inherited Create(ABoletoWS);

  FPAccept := C_ACCEPT;

  if Assigned(OAuth) then
  begin
    if OAuth.Ambiente = taHomologacao then
      OAuth.URL := C_URL_OAUTH_HOM
    else
      OAuth.URL := C_URL_OAUTH_PROD;

    OAuth.Payload := OAuth.Ambiente = taHomologacao;
  end;

end;

function TBoletoW_Inter_API.GerarRemessa: string;
begin
  result := inherited GerarRemessa;
end;

function TBoletoW_Inter_API.Enviar: boolean;
begin
  result := inherited Enviar;
end;

end.

