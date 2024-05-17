{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo:  Victor Hugo Gonzales - Panda, Leandro do Couto,}
{ Delmar de Lima, Daniel de Morais InfoCotidiano, ActioSistemas                }
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
  synacode, strutils, DateUtils, ACBrDFeSSL, synautil, ACBrBoleto, httpsend,
  ACBrBoletoWS.Rest, ACBrJSON;

type

  { TBoletoW_Inter_API }
  TBoletoW_Inter_API = class(TBoletoWSREST)
  private
//    function CodigoTipoTitulo(AEspecieDoc: String): String;
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
    procedure GerarPagador(AJson: TACBrJSONObject);
    procedure GerarBenificiarioFinal(AJson: TACBrJSONObject);
    procedure GerarJuros(AJson: TACBrJSONObject);
    procedure GerarMulta(AJson: TACBrJSONObject);
    procedure GerarDesconto(AJson: TACBrJSONObject);
    procedure GeraDadosInstrucao(AJson: TACBrJSONObject);

    procedure AlteraDataVencimento(AJson: TACBrJSONObject);
    procedure AtribuirDesconto(AJson: TACBrJSONObject);
    procedure AlteracaoDesconto(AJson: TACBrJSONObject);
    procedure AlteracaoDataDesconto(AJson: TACBrJSONObject);
    procedure AlterarProtesto(AJson: TACBrJSONObject);
    procedure AtribuirAbatimento(AJson: TACBrJSONObject);
    procedure AlteracaoAbatimento(AJson: TACBrJSONObject);
    procedure AtribuirJuros(AJson: TACBrJSONObject);
    procedure AtribuirMulta(AJson: TACBrJSONObject);
    procedure AtribuirNegativacao(AJson: TACBrJSONObject);
    procedure AlteracaoSeuNumero(AJson: TACBrJSONObject);
    procedure AlteracaoEnderecoPagador(AJson: TACBrJSONObject);
    procedure AlteracaoPrazo(AJson: TACBrJSONObject);

  public
    constructor Create(ABoletoWS: TBoletoWS); override;

    function GerarRemessa: string; override;
    function Enviar: boolean; override;

  end;

const
  C_URL             = 'https://cdpj.partners.bancointer.com.br/cobranca/v2';
  C_URL_HOM         = 'https://cdpj.partners.bancointer.com.br/cobranca/v2';

  C_URLPIX          = 'https://cdpj.partners.bancointer.com.br/cobranca/v3';
  C_URL_HOMPIX      = 'https://cdpj.partners.bancointer.com.br/cobranca/v3';

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
  LNossoNumero, LNossoNumeroCorrespondente: string;
begin
  if Assigned(Atitulo) then
    begin
      LNossoNumero := PadLeft(ATitulo.NossoNumero, 11, '0');

      if Boleto.Cedente.CedenteWS.IndicadorPix then
        begin
          LNossoNumero               := ATitulo.NossoNumero;
          LNossoNumeroCorrespondente := ATitulo.NossoNumeroCorrespondente;
        end;
    end;


  if Boleto.Configuracoes.WebService.Ambiente = taProducao then
   FPURL := IfThen(Boleto.Cedente.CedenteWS.IndicadorPix, C_URLPIX, C_URL)
  else
   FPURL := IfThen(Boleto.Cedente.CedenteWS.IndicadorPix, C_URL_HOMPIX, C_URL_HOM);

  if Boleto.Cedente.CedenteWS.IndicadorPix then
    begin
      if EstaVazio(LNossoNumeroCorrespondente) and
      ((Boleto.Configuracoes.WebService.Operacao = tpConsultaDetalhe)  or  (Boleto.Configuracoes.WebService.Operacao = tpBaixa)) then
         raise Exception.Create('Boleto PIX necessario informar codigoSolicitacao, Exemplo:'+sLineBreak+
                                'NossoNumeroCorrespondente := "183e982a-34e5-4bc0-9643-def5432a"');
      case Boleto.Configuracoes.WebService.Operacao of
        tpInclui:         FPURL := FPURL + '/cobrancas';
        tpConsulta:       FPURL := FPURL + '/cobrancas?' + DefinirParametros;
        tpAltera:         FPURL := FPURL + '/comandoInstrucao';
        tpConsultaDetalhe:FPURL := FPURL + '/cobrancas/' + LNossoNumeroCorrespondente;
        tpBaixa:          FPURL := FPURL + '/cobrancas/' + LNossoNumeroCorrespondente + '/cancelar';
      end;
    end
  else
    begin
      case Boleto.Configuracoes.WebService.Operacao of
        tpInclui:         FPURL := FPURL + '/boletos';
        tpConsulta:       FPURL := FPURL + '/boletos?' + DefinirParametros;
        tpAltera:         FPURL := FPURL + '/comandoInstrucao';
        tpConsultaDetalhe:FPURL := FPURL + '/boletos/' + LNossoNumero;
        tpBaixa:          FPURL := FPURL + '/boletos/' + LNossoNumero + '/cancelar';
      end
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
  Str: String;
  LJson: TACBrJSONObject;
begin
  FPContentType := 'x-www-form-urlencoded';
  LJson := TACBrJSONObject.Create;
  try
   if (ATitulo <> nil) and (Boleto.Cedente.CedenteWS.IndicadorPix) then
     LJson.AddPair('motivoCancelamento','Solicitado Pela Empresa')
   else
     LJson.AddPair('motivoCancelamento','PAGODIRETOAOCLIENTE');
   FPDadosMsg := LJson.ToJSON;
  finally
    LJson.Free;
  end;
end;

procedure TBoletoW_Inter_API.DefinirKeyUser;
begin
  if Assigned(aTitulo) then
    FPKeyUser := '';
end;

function TBoletoW_Inter_API.DefinirParametros: String;
var
  LConsulta: TStringList;
  LDocumento, LSituacaoAbertos, LSituacaoBaixados, LSituacaoVencidos, LSituacaoCancelados: String;
begin
  if Assigned(Boleto.Configuracoes.WebService.Filtro) then
  begin
    if Boleto.Cedente.CedenteWS.IndicadorPix then
    begin
      LSituacaoBaixados:= 'RECEBIDO';
      LSituacaoVencidos:= 'ATRASADO';
      LSituacaoAbertos := 'A_RECEBER';
      LSituacaoCancelados := 'CANCELADO';
    end
    else
    begin
      LSituacaoBaixados:= 'PAGO,CANCELADO';
      LSituacaoAbertos:=  'EMABERTO,VENCIDO';
      LSituacaoVencidos:= 'VENCIDO';
      LSituacaoCancelados := 'CANCELADO';
    end;

    LDocumento := OnlyNumber
      (Boleto.Configuracoes.WebService.Filtro.cnpjCpfPagador);

    LConsulta := TStringList.Create;
    LConsulta.Delimiter := '&';
    try

      LConsulta.Add( 'itensPorPagina=1000' );

      if Boleto.Configuracoes.WebService.Filtro.indiceContinuidade > 0 then
        LConsulta.Add('paginaAtual='+ FloatToStr(Boleto.Configuracoes.WebService.Filtro.indiceContinuidade));

      case Boleto.Configuracoes.WebService.Filtro.indicadorSituacao of
        isbBaixado:
          begin
            LConsulta.Add('filtrarDataPor=SITUACAO' );
            LConsulta.Add('situacao='+LSituacaoBaixados);
            LConsulta.Add('dataInicial=' +DateTimeToDateInter(Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataInicio));
            LConsulta.Add('dataFinal=' +DateTimeToDateInter(Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataFinal));
            LConsulta.Add( 'ordenarPor=DATASITUACAO' );
          end;
        isbCancelado:
          begin
            LConsulta.Add('filtrarDataPor=SITUACAO' );
            LConsulta.Add('situacao='+LSituacaoCancelados);
            LConsulta.Add('dataInicial=' +DateTimeToDateInter(Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataInicio));
            LConsulta.Add('dataFinal=' +DateTimeToDateInter(Boleto.Configuracoes.WebService.Filtro.dataMovimento.DataFinal));
            LConsulta.Add( 'ordenarPor=DATASITUACAO' );
          end;
        isbAberto:
          begin
            if Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataInicio > 0 then
            begin
              LConsulta.Add('filtrarDataPor=VENCIMENTO' );
              case Boleto.Configuracoes.WebService.Filtro.boletoVencido of
                ibvSim: LConsulta.Add('situacao='+LSituacaoVencidos);
              else
                 LConsulta.Add('situacao='+LSituacaoAbertos);
              end;
              LConsulta.Add('dataInicial=' +DateTimeToDateInter(Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataInicio));
              LConsulta.Add('dataFinal=' +DateTimeToDateInter(Boleto.Configuracoes.WebService.Filtro.dataVencimento.DataFinal));
              LConsulta.Add( 'ordenarPor=DATAVENCIMENTO' );
            end;

            if Boleto.Configuracoes.WebService.Filtro.dataRegistro.DataInicio > 0
            then
            begin
              LConsulta.Add( 'filtrarDataPor=EMISSAO' );
              LConsulta.Add('situacao='+LSituacaoAbertos);
              LConsulta.Add('dataInicial=' +DateTimeToDateInter(Boleto.Configuracoes.WebService.Filtro.dataRegistro.DataInicio));
              LConsulta.Add('dataFinal=' +DateTimeToDateInter(Boleto.Configuracoes.WebService.Filtro.DataRegistro.DataFinal));
              LConsulta.Add( 'ordenarPor=DATASITUACAO' );
            end;
          end;
      end;

    finally
      Result := LConsulta.DelimitedText;
      LConsulta.Free;
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
  LData: string;
  LJson: TACBrJSONObject;
  LSeuNumero: String;
begin
  if Assigned(aTitulo) then
  begin
    LJson := TACBrJSONObject.Create;
    try

      LSeuNumero := ATitulo.SeuNumero;

      LJson.AddPair('seuNumero',LSeuNumero);
      LJson.AddPair('valorNominal',aTitulo.ValorDocumento);
      LJson.AddPair('dataVencimento',DateTimeToDateInter(aTitulo.Vencimento));
      LJson.AddPair('numDiasAgenda',DaysBetween(ATitulo.Vencimento, ATitulo.DataLimitePagto));

      GerarDesconto(LJson);
      GerarJuros(LJson);
      GerarMulta(LJson);
      GerarPagador(LJson);
      GerarBenificiarioFinal(LJson);

      LData := LJson.ToJSON;

      FPDadosMsg := LData;

    finally
      LJson.Free;
    end;
  end;
end;

procedure TBoletoW_Inter_API.RequisicaoAltera;
begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.GeraDadosInstrucao(AJson: TACBrJSONObject);
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

procedure TBoletoW_Inter_API.GerarPagador(AJson: TACBrJSONObject);
var
  LJsonDadosPagador: TACBrJSONObject;

begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin

    LJsonDadosPagador := TACBrJSONObject.Create;

    LJsonDadosPagador.AddPair('cpfCnpj',OnlyNumber(ATitulo.Sacado.CNPJCPF));
    LJsonDadosPagador.AddPair('tipoPessoa',IfThen(Length(OnlyNumber(ATitulo.Sacado.CNPJCPF)) = 11, 'FISICA','JURIDICA'));
    LJsonDadosPagador.AddPair('nome',ATitulo.Sacado.NomeSacado);
    LJsonDadosPagador.AddPair('endereco',ATitulo.Sacado.Logradouro);
    LJsonDadosPagador.AddPair('numero',ATitulo.Sacado.Numero);
    LJsonDadosPagador.AddPair('complemento',ATitulo.Sacado.Complemento);
    LJsonDadosPagador.AddPair('bairro',ATitulo.Sacado.Bairro);
    LJsonDadosPagador.AddPair('cep',OnlyNumber(ATitulo.Sacado.CEP));
    LJsonDadosPagador.AddPair('cidade',ATitulo.Sacado.Cidade);
    LJsonDadosPagador.AddPair('uf',ATitulo.Sacado.UF);
    LJsonDadosPagador.AddPair('telefone',IfThen(ATitulo.Sacado.Fone = '', '0', ATitulo.Sacado.Fone));
    LJsonDadosPagador.AddPair('email',ATitulo.Sacado.Email);


    AJson.AddPair('pagador',LJsonDadosPagador);
  end;

end;

procedure TBoletoW_Inter_API.GerarBenificiarioFinal(AJson: TACBrJSONObject);

var
  LJsonSacadorAvalista: TACBrJSONObject;


begin

  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    if ATitulo.Sacado.SacadoAvalista.CNPJCPF = EmptyStr then
      Exit;

    LJsonSacadorAvalista := TACBrJSONObject.Create;

    LJsonSacadorAvalista.AddPair('nome',ATitulo.Sacado.SacadoAvalista.NomeAvalista);
    LJsonSacadorAvalista.AddPair('cpfCnpj',OnlyNumber(ATitulo.Sacado.SacadoAvalista.CNPJCPF));
    LJsonSacadorAvalista.AddPair('tipoPessoa',ifThen(Length(OnlyNumber(ATitulo.Sacado.SacadoAvalista.CNPJCPF)) = 11, 'FISICA', 'JURIDICA'));
    LJsonSacadorAvalista.AddPair('cep',ATitulo.Sacado.SacadoAvalista.CEP);
    LJsonSacadorAvalista.AddPair('endereco',ATitulo.Sacado.SacadoAvalista.Logradouro);
    LJsonSacadorAvalista.AddPair('bairro',ATitulo.Sacado.SacadoAvalista.Bairro);
    LJsonSacadorAvalista.AddPair('cidade',ATitulo.Sacado.SacadoAvalista.Cidade);
    LJsonSacadorAvalista.AddPair('uf',ATitulo.Sacado.SacadoAvalista.UF);

    AJson.AddPair('beneficiarioFinal',LJsonSacadorAvalista);
  end;
end;

procedure TBoletoW_Inter_API.GerarJuros(AJson: TACBrJSONObject);
var
  LJsonJuros: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJson) and (ATitulo.ValorMoraJuros > 0) then
  begin
    LJsonJuros := TACBrJSONObject.Create;

    if ATitulo.CodigoMora = '' then
    begin
      case ATitulo.CodigoMoraJuros of
        cjValorDia  : ATitulo.CodigoMora := '1';
        cjTaxaMensal: ATitulo.CodigoMora := '2';
        cjIsento    : ATitulo.CodigoMora := '3';
      end;
    end;

    if Boleto.Cedente.CedenteWS.IndicadorPix then
    begin
      case (StrToIntDef(ATitulo.CodigoMora, 3)) of
        1:
          begin
            LJsonJuros.AddPair('codigo','VALORDIA');
            LJsonJuros.AddPair('valor',ATitulo.ValorMoraJuros);
          end;
        2:
          begin
            LJsonJuros.AddPair('codigo','TAXAMENSAL');
            LJsonJuros.AddPair('taxa',ATitulo.ValorMoraJuros);
          end;
      end;
    end
    else
    begin
      case (StrToIntDef(ATitulo.CodigoMora, 3)) of
        1:
          begin
            LJsonJuros.AddPair('codigoMora','VALORDIA');
            LJsonJuros.AddPair('data', DateTimeToDateInter(ATitulo.DataMulta));
            LJsonJuros.AddPair('valor', ATitulo.ValorMoraJuros);
            LJsonJuros.AddPair('taxa', 0);
          end;
        2:
          begin
            LJsonJuros.AddPair('codigoMora', 'TAXAMENSAL');
            LJsonJuros.AddPair('data', DateTimeToDateInter(ATitulo.DataMulta));
            LJsonJuros.AddPair('taxa', ATitulo.ValorMoraJuros);
            LJsonJuros.AddPair('valor', 0);
          end;
        3:
          begin
            LJsonJuros.AddPair('codigoMora', 'ISENTO');
            LJsonJuros.AddPair('taxa', 0);
            LJsonJuros.AddPair('valor', 0);
          end;
      end;
    end;
    AJson.AddPair('mora',LJsonJuros);
  end;
end;

procedure TBoletoW_Inter_API.GerarMulta(AJson: TACBrJSONObject);
var
  LJsonMulta: TACBrJSONObject;
  LCodMulta: Integer;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonMulta := TACBrJSONObject.Create;

    if ATitulo.PercentualMulta > 0 then
    begin
      if ATitulo.MultaValorFixo then
        LCodMulta := 1
      else
        LCodMulta := 2;
    end
    else
        LCodMulta := 3;

    if (ATitulo.DataMulta > 0) then
    begin
      if Boleto.Cedente.CedenteWS.IndicadorPix then
      begin
        case LCodMulta of
          1:
            begin
              LJsonMulta.AddPair('codigo','VALORFIXO');
              LJsonMulta.AddPair('valor', ATitulo.PercentualMulta);
            end;
          2:
            begin
              LJsonMulta.AddPair('codigo','PERCENTUAL');
              LJsonMulta.AddPair('taxa',ATitulo.PercentualMulta);
            end;
          3:
            begin
//                  //LJsonMulta.AddPair('codigoMulta').Value.asString := 'NAOTEMMULTA'; //2024-01-08 - TK5016 - 00215-23 - retirado
            end;
        end;
      end
      else  // qdo nao é hibrido
      begin
        case LCodMulta of
          1:
            begin
              LJsonMulta.AddPair('codigoMulta','VALORFIXO');
              LJsonMulta.AddPair('data',DateTimeToDateInter(ATitulo.DataMulta));
              LJsonMulta.AddPair('valor',ATitulo.PercentualMulta);
              LJsonMulta.AddPair('taxa',0);
            end;
          2:
            begin
              LJsonMulta.AddPair('codigoMulta','PERCENTUAL');
              LJsonMulta.AddPair('data',DateTimeToDateInter(ATitulo.DataMulta));
              LJsonMulta.AddPair('taxa',ATitulo.PercentualMulta);
              LJsonMulta.AddPair('valor',0);
            end;
          3:
            begin
              LJsonMulta.AddPair('codigoMulta','NAOTEMMULTA');
              LJsonMulta.AddPair('valor',0);
              LJsonMulta.AddPair('taxa',0);
            end;
        end;
      end;
        AJson.AddPair('multa',LJsonMulta);
    end;
  end;
end;

procedure TBoletoW_Inter_API.GerarDesconto(AJson: TACBrJSONObject);
var
  LJsonDesconto, LJsonDesconto2, LJsonDesconto3: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonDesconto := TACBrJSONObject.Create; // verificar
    if (ATitulo.DataDesconto > 0) then
    begin

      LJsonDesconto.AddPair('data',DateTimeToDateInter(ATitulo.DataDesconto ));

      case Integer(ATitulo.TipoDesconto) of
        1: // Valor Fixo até a data informada
          begin
            if Boleto.Cedente.CedenteWS.IndicadorPix then
              begin
                LJsonDesconto.AddPair('codigo','VALORFIXODATAINFORMADA');
                LJsonDesconto.AddPair('taxa',ATitulo.ValorDesconto);
                LJsonDesconto.AddPair('quantidadeDias',(ATitulo.Vencimento - ATitulo.DataDesconto));
              end
            else
              begin
                LJsonDesconto.AddPair('codigoDesconto','VALORFIXODATAINFORMADA');
                LJsonDesconto.AddPair('valor',ATitulo.ValorDesconto);
                LJsonDesconto.AddPair('taxa',0);
              end;
          end;
        2: // percentual até a data informada
          begin
            if Boleto.Cedente.CedenteWS.IndicadorPix then
              begin
                LJsonDesconto.AddPair('codigo','PERCENTUALDATAINFORMADA');
                LJsonDesconto.AddPair('taxa',ATitulo.ValorDesconto);
                LJsonDesconto.AddPair('quantidadeDias',(ATitulo.Vencimento - ATitulo.DataDesconto));
              end
            else
              begin
                LJsonDesconto.AddPair('codigoDesconto','PERCENTUALDATAINFORMADA');
                LJsonDesconto.AddPair('taxa',ATitulo.ValorDesconto);
                LJsonDesconto.AddPair('valor',0);
              end;
          end;
      else
        begin
          if not Boleto.Cedente.CedenteWS.IndicadorPix then
            begin
              LJsonDesconto.AddPair('codigoDesconto','NAOTEMDESCONTO');
              LJsonDesconto.AddPair('valor',0);
              LJsonDesconto.AddPair('taxa',0);
            end;
        end;
      end;
    end
    else
    begin
      if not Boleto.Cedente.CedenteWS.IndicadorPix then
        begin
          LJsonDesconto.AddPair('codigoDesconto','NAOTEMDESCONTO');
          LJsonDesconto.AddPair('valor',0);
          LJsonDesconto.AddPair('taxa',0);
        end;
    end;

    if ((Boleto.Cedente.CedenteWS.IndicadorPix) and (ATitulo.DataDesconto > 0))  then
       AJson.AddPair('desconto', LJsonDesconto)
    else if not Boleto.Cedente.CedenteWS.IndicadorPix then
       AJson.AddPair('desconto1', LJsonDesconto) ;

    if ((ATitulo.DataDesconto2 > 0) and (not Boleto.Cedente.CedenteWS.IndicadorPix)) then
    begin
      LJsonDesconto2 := TACBrJSONObject.Create;
      LJsonDesconto2.AddPair('data',ATitulo.DataDesconto2);

      case Integer(ATitulo.TipoDesconto) of
        1:
          begin
            LJsonDesconto2.AddPair('codigoDesconto','VALORFIXODATAINFORMADA');
            LJsonDesconto2.AddPair('valor',ATitulo.ValorDesconto2);
            LJsonDesconto2.AddPair('taxa',0);
          end;
        2:
          begin
            LJsonDesconto2.AddPair('codigoDesconto','PERCENTUALDATAINFORMADA');
            LJsonDesconto2.AddPair('taxa',ATitulo.ValorDesconto2);
            LJsonDesconto2.AddPair('valor',0);
          end;
      else
        begin
          LJsonDesconto2.AddPair('codigoDesconto','NAOTEMDESCONTO');
          LJsonDesconto2.AddPair('valor',0);
          LJsonDesconto2.AddPair('taxa',0);
        end;
      end;

      AJson.AddPair('desconto2',LJsonDesconto2);
    end;
  end;
end;

procedure TBoletoW_Inter_API.AlteraDataVencimento(AJson: TACBrJSONObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.AtribuirDesconto(AJson: TACBrJSONObject);
begin
  if Assigned(aTitulo) and Assigned(AJson) then
  begin
    AJson.AddPair('valor1',aTitulo.ValorDesconto);
  end;
end;

procedure TBoletoW_Inter_API.AlteracaoDesconto(AJson: TACBrJSONObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.AlteracaoDataDesconto(AJson: TACBrJSONObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.AlterarProtesto(AJson: TACBrJSONObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.AtribuirAbatimento(AJson: TACBrJSONObject);

begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.AlteracaoAbatimento(AJson: TACBrJSONObject);

begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.AtribuirJuros(AJson: TACBrJSONObject);

begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.AtribuirMulta(AJson: TACBrJSONObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.AtribuirNegativacao(AJson: TACBrJSONObject);
begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.AlteracaoSeuNumero(AJson: TACBrJSONObject);

begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.AlteracaoEnderecoPagador(AJson: TACBrJSONObject);

begin
  // Sem Payload
end;

procedure TBoletoW_Inter_API.AlteracaoPrazo(AJson: TACBrJSONObject);

begin
  // sem Payload
end;

//function TBoletoW_Inter_API.CodigoTipoTitulo(AEspecieDoc: String): String;
//begin
//end;

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
var
  LJsonObject : TACBrJSONObject;
  LOperacao : TOperacao;
begin
  Result := inherited Enviar;
  LOperacao := Boleto.Configuracoes.WebService.Operacao;
  (*
  Tratamento automatico qdo tpINCLUI com pix.
  Entao ele vai enviar capturar a reposta para pegar o codigoSolicitacao e realizar uma consulta
  detalhe automaticamente para retornar o boleto com QrCODE
  *)
  if (Boleto.Cedente.CedenteWS.IndicadorPix) and (Boleto.Configuracoes.WebService.Operacao = tpInclui) then
  begin
    try
      LJsonObject := TACBrJSONObject.Parse( FRetornoWS );
      if NaoEstaVazio(LJsonObject.AsString['codigoSolicitacao']) then
      begin
        ATitulo.NossoNumeroCorrespondente := LJsonObject.AsString['codigoSolicitacao'];
        Boleto.Configuracoes.WebService.Operacao := tpConsultaDetalhe;
        GerarDados;
        Result := inherited Enviar;
      end;
    finally
      LJsonObject.Free;
      Boleto.Configuracoes.WebService.Operacao := tpInclui;
    end;
  end;
end;

end.

