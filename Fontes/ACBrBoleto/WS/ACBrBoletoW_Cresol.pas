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
unit ACBrBoletoW_Cresol;

interface

uses
   SysUtils,
   StrUtils,
   ACBrBoleto,
   ACBrJSON,
   ACBrBoletoConversao,
   ACBrBoletoWS,
   ACBrBoletoWS.Rest,
   pcnConversao;

type
   { TBoletoW_Cresol}
   TBoletoW_Cresol = class(TBoletoWSREST)
   private
      function DateTimeToDateCresol( const AValue:TDateTime ):String;
   protected
      function GerarTokenAutenticacao: string; override;

      procedure DefinirURL; override;
      procedure DefinirContentType; override;
      procedure GerarHeader; override;
      procedure GerarDados; override;
      procedure DefinirAuthorization; override;
      procedure DefinirParamOAuth; override;
      procedure DefinirKeyUser;
      procedure RequisicaoJson;
      procedure RequisicaoAltera;
      procedure RequisicaoConsultaDetalhe;
      procedure GerarJuros(AJson: TACBrJSONObject);
      procedure GerarMulta(AJson: TACBrJSONObject);
      procedure AlteraDataVencimento(AJson: TACBrJSONObject);
   public
      constructor Create(ABoletoWS: TBoletoWS); override;
      function GerarRemessa: string; override;
      function Enviar: boolean; override;
   end;

const
   C_URL                = 'https://cresolapi.governarti.com.br/';
   C_URL_HOM            = 'https://api-dev.governarti.com.br/';

   C_URL_TOKEN          = 'https://cresolauth.governarti.com.br/auth/realms/cresol/protocol/openid-connect/token';
   C_URL_TOKEN_HOM      = 'https://auth-dev.governarti.com.br/auth/realms/cresol/protocol/openid-connect/token';

   C_CONTENT_TYPE    = 'application/json';
   C_ACCEPT          = 'application/json';

   C_ACCEPT_ENCODING = 'gzip, deflate, br';

implementation

uses
   ACBrUtil.Strings,
   ACBrUtil.DateTime;

{ TBoletoW_Cresol}

procedure TBoletoW_Cresol.DefinirURL;
var
   LNossoNumero: string;
begin
   if( aTitulo <> nil ) then
      LNossoNumero := OnlyNumber(aTitulo.NossoNumeroCorrespondente);
   FPURL := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, C_URL,C_URL_HOM);
   case Boleto.Configuracoes.WebService.Operacao of
      tpInclui:  FPURL := FPURL + 'titulos';
      tpAltera:
      begin
         if ATitulo.OcorrenciaOriginal.Tipo = ACBrBoleto.toRemessaAlterarVencimento then
            FPURL := FPURL + 'titulos/' + LNossoNumero;// Feito um Put com a DtVencimento para alterar o vencimento.
      end;
      tpConsultaDetalhe:  FPURL := FPURL + 'titulos/' + LNossoNumero;//Se não tiver IDBolApi, vai pela URL que traz todos os boletos
      tpConsulta:  FPURL := FPURL + 'titulos' ;
      tpBaixa:  FPURL := FPURL + 'titulos/' + LNossoNumero + '/operacao/baixar';
   end;
//Exemplo de Consultas por status:
//FPURL := 'https://cresolapi.governarti.com.br/titulos?status=LIQUIDADO';
//FPURL := 'https://cresolapi.governarti.com.br/titulos?status=BAIXADO_MANUALMENTE';
//FPURL := 'https://cresolapi.governarti.com.br/titulos?status=EM_ABERTO';
end;

procedure TBoletoW_Cresol.DefinirContentType;
begin
   FPContentType := C_CONTENT_TYPE;
end;

procedure TBoletoW_Cresol.GerarHeader;
begin
   DefinirContentType;
   DefinirAuthorization;
   FPIdentificador := 'ApiCresol';
end;

procedure TBoletoW_Cresol.GerarDados;
begin
   if Assigned(Boleto) then
      DefinirURL;
   case Boleto.Configuracoes.WebService.Operacao of
      tpInclui: begin
         FMetodoHTTP := htPOST;//Define Método POST para Incluir
         RequisicaoJson;
      end;
      tpAltera: begin
         FMetodoHTTP := htPUT;//Define Método PUT para Alterar;
         RequisicaoAltera;
      end;
      tpBaixa: begin
         FMetodoHTTP := htPUT;//Define Método PUT para Baixa
         //Na baixa não precisa enviar o Body, vai enviar o ID do boleto na URL.
      end;
      tpConsultaDetalhe: begin
         FMetodoHTTP := htGET;//Define Método GET Consulta Detalhe
         RequisicaoConsultaDetalhe;
      end;
   else
      raise EACBrBoletoWSException.Create
      (ClassName + Format(S_OPERACAO_NAO_IMPLEMENTADO,
      [TipoOperacaoToStr(Boleto.Configuracoes.WebService.Operacao)]));
   end;
end;

procedure TBoletoW_Cresol.DefinirAuthorization;
begin
   DefinirKeyUser;
   FPAuthorization:= 'Authorization: Bearer ' + GerarTokenAutenticacao;
end;

function TBoletoW_Cresol.GerarTokenAutenticacao: string;
begin
   OAuth.Payload := True;
   Result := inherited GerarTokenAutenticacao;
end;

procedure TBoletoW_Cresol.DefinirKeyUser;
begin
   FPKeyUser := Boleto.Cedente.CedenteWS.KeyUser;
end;

procedure TBoletoW_Cresol.DefinirParamOAuth;
begin
   FParamsOAuth := Format( 'client_id=%s&scope=%s&client_secret=%s&username=%s&password=%s&grant_type=password',
                   ['cresolApi',
                    Boleto.Cedente.CedenteWS.Scope,
                    'cr3s0l4p1',
                    Boleto.Cedente.CedenteWS.ClientID,
                    Boleto.Cedente.CedenteWS.ClientSecret] );
end;

function TBoletoW_Cresol.DateTimeToDateCresol(const AValue: TDateTime): String;
begin
   result := FormatDateBr( aValue, 'YYYY-MM-DD');
end;

procedure TBoletoW_Cresol.RequisicaoJson;
var
   LJson: TACBrJSONObject;
begin
   if Assigned(aTitulo) then
   begin
      LJson := TACBrJSONObject.Create;
      try
         LJson.AddPair('idEmissao', 2); // emissão pelo cliente
         LJson.AddPair('idEspecie' , StrToIntDef(ATitulo.EspecieDoc, 2));
         LJson.AddPair('tipoPagador', IfThen(Length(OnlyNumber(aTitulo.Sacado.CNPJCPF)) = 11, '0', '1'));
         LJson.AddPair('docPagador',  OnlyNumber(aTitulo.Sacado.CNPJCPF));
         LJson.AddPair('pagadorNome', Copy(aTitulo.Sacado.NomeSacado, 1, 50));
         LJson.AddPair('pagadorEndereco', aTitulo.Sacado.Logradouro);
         LJson.AddPair('pagadorEnderecoNumero', aTitulo.Sacado.Numero);
         LJson.AddPair('pagadorBairro', aTitulo.Sacado.Bairro);
         LJson.AddPair('pagadorCep', StrToIntDef(aTitulo.Sacado.CEP, 0));
         LJson.AddPair('pagadorCidade', aTitulo.Sacado.Cidade);
         LJson.AddPair('pagadorUf', aTitulo.Sacado.UF);
         LJson.AddPair('numeroDocumento', aTitulo.NumeroDocumento);
         LJson.AddPair('dtVencimento', DateTimeToDateCresol(aTitulo.Vencimento));
         LJson.AddPair('dtDocumento', DateTimeToDateCresol(aTitulo.DataDocumento));
         LJson.AddPair('valorNominal', ATitulo.ValorDocumento);
         if aTitulo.ValorDesconto > 0 then
            LJson.AddPair('valorDesconto', aTitulo.ValorDesconto);
         GerarJuros(LJson);
         GerarMulta(LJson);
         if aTitulo.DiasDeProtesto > 0 then
            LJson.AddPair('nrProtestoDias', aTitulo.DiasDeProtesto);
         FPDadosMsg := Format('[%s]',[LJson.ToJSON]);
      finally
         LJson.Free;
      end;
   end;
end;

procedure TBoletoW_Cresol.RequisicaoAltera;
var
   LJson: TACBrJSONObject;
begin
   if Assigned(aTitulo) then
   begin
      LJson := TACBrJSONObject.Create;
      try
         if (ATitulo.Vencimento > 0) then
            LJson.AddPair('dtVencimento', DateTimeToDateCresol(aTitulo.Vencimento));
         FPDadosMsg := LJson.ToJSON;
      finally
         LJson.Free;
      end;
   end;
end;

procedure TBoletoW_Cresol.RequisicaoConsultaDetalhe;
begin
  // Sem Payload
end;

procedure TBoletoW_Cresol.GerarJuros(AJson: TACBrJSONObject);
begin
   if Assigned(aTitulo) then
   begin
      if Assigned(AJson) then
      begin
         if ATitulo.CodigoMora = '' then
         begin
            case aTitulo.CodigoMoraJuros of
               cjValorDia   : aTitulo.CodigoMora := '1';
               cjTaxaMensal : aTitulo.CodigoMora := '2';
               cjIsento     : aTitulo.CodigoMora := '3';
            else
               aTitulo.CodigoMora := '3';
            end;
         end;
         case (StrToIntDef(aTitulo.CodigoMora, 0)) of
            0,3:
            begin // Isento
               AJson.AddPair('cdTipoJuros', 'ISENTO');
               AJson.AddPair('valorJuros', 0);
            end;
            1:
            begin   // Reais
               AJson.AddPair('cdTipoJuros', 'VALOR_FIXO');
               AJson.AddPair('valorJuros', aTitulo.ValorMoraJuros);
             end;
            2:
            begin   // Percentual
               AJson.AddPair('cdTipoJuros', 'VALOR_PERCENTUAL');
               AJson.AddPair('valorJuros', aTitulo.ValorMoraJuros);
            end;
         end;
      end;
   end;
end;

procedure TBoletoW_Cresol.GerarMulta(AJson: TACBrJSONObject);
var
   LCodigoMulta: Integer;
begin
   if Assigned(aTitulo) then
   begin
      if Assigned(AJson) then
      begin
         if aTitulo.PercentualMulta > 0 then
         begin
            if aTitulo.MultaValorFixo then
               LCodigoMulta := 1
            else
               LCodigoMulta := 2;
         end
         else
            LCodigoMulta := 3;
         case LCodigoMulta of
            1: begin
               AJson.AddPair('cdTipoMulta', 'VALOR_FIXO');
               AJson.AddPair('valorMulta', aTitulo.PercentualMulta);
            end;
            2:begin
               AJson.AddPair('cdTipoMulta', 'VALOR_PERCENTUAL');
               AJson.AddPair('valorMulta', aTitulo.PercentualMulta);
            end;
            3: begin
               AJson.AddPair('cdTipoMulta', 'ISENTO');
               AJson.AddPair('valorMulta', 0);
            end;
         end;
      end;
   end;
end;

procedure TBoletoW_Cresol.AlteraDataVencimento(AJson: TACBrJSONObject);//Cresol possui operação.
begin
   if Assigned(ATitulo) then
      if Assigned(AJson) then
         if (ATitulo.Vencimento > 0) then
            AJson.AddPair('dataVencimento', DateTimeToDateCresol(aTitulo.Vencimento));
end;

constructor TBoletoW_Cresol.Create(ABoletoWS: TBoletoWS);
begin
   inherited Create(ABoletoWS);
   FPAccept := C_ACCEPT;
   if Assigned(OAuth) then
   begin
      if OAuth.Ambiente = taHomologacao then
         OAuth.URL := C_URL_TOKEN_HOM
      else
         OAuth.URL := C_URL_TOKEN;
      OAuth.Payload := True;
   end;
end;

function TBoletoW_Cresol.GerarRemessa: string;
begin
   DefinirCertificado;
   result := inherited GerarRemessa;
end;

function TBoletoW_Cresol.Enviar: boolean;
begin
   DefinirCertificado;
   result := inherited Enviar;
end;

end.

