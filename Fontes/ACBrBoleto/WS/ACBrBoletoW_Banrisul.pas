{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
//incluido em 12/09/2024
{$I ACBr.inc}
unit ACBrBoletoW_Banrisul;

interface

uses
   Classes,
   SysUtils,
   ACBrBoletoWS,
   pcnConversao,
   ACBrBoletoConversao,
   ACBrBoleto,
   ACBrBoletoWS.Rest,
   ACBrJSON;

type

   { TBoletoW_Banrisul }
   TBoletoW_Banrisul = class(TBoletoWSREST)
   private
      function CodigoTipoTitulo(AEspecieDoc: String): Integer;
   protected
      procedure DefinirURL; override;
      procedure DefinirContentType; override;
      procedure GerarHeader; override;
      procedure GerarDados; override;
      procedure DefinirAuthorization; override;
      function GerarTokenAutenticacao: string; override;
      procedure DefinirKeyUser;
      procedure DefinirAutenticacao;
      function ValidaAmbiente: String;
      procedure RequisicaoJson;
      procedure RequisicaoBaixa;
      procedure RequisicaoConsulta;
      procedure RequisicaoConsultaDetalhe;

      procedure GeraDadoBoleto(AJson: TACBrJSONObject);
      procedure GerarPagador(AJson: TACBrJSONObject);
      procedure GerarBenificiarioFinal(AJson: TACBrJSONObject);
      procedure GerarIntrucoes(AJson: TACBrJSONObject);
      procedure GerarJuros(AJson: TACBrJSONObject);
      procedure GerarMulta(AJson: TACBrJSONObject);
      procedure GerarDesconto(AJson: TACBrJSONObject);
      procedure GerarProtesto(AJson: TACBrJSONObject);
      procedure GerarPag_Parcial(AJson: TACBrJSONObject);
      procedure GerarHibrido(AJson: TACBrJSONObject);

   public
      constructor Create(ABoletoWS: TBoletoWS); override;

      function GerarRemessa: string; override;
      function Enviar: boolean; override;

   end;

const
   C_URL     = 'https://api.banrisul.com.br/cobranca/v1';
   C_URL_HOM = 'https://apidev.banrisul.com.br/cobranca/v1';

   C_URL_OAUTH_PROD = 'https://api.banrisul.com.br/auth/oauth/v2/token';
   C_URL_OAUTH_HOM  = 'https://apidev.banrisul.com.br/auth/oauth/v2/token';

   C_ACCEPT = 'application/json';
   C_AUTHORIZATION = 'Authorization';

implementation

uses
   synacode,
   strutils,
   DateUtils,
   ACBrUtil.DateTime,
   ACBrUtil.Base,
   ACBrUtil.Strings;

{ TBoletoW_Banrisul }

procedure TBoletoW_Banrisul.DefinirURL;
var
   DevAPP, ID, NConvenio: String;
begin
   FPURL := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, C_URL,
     C_URL_HOM);

   if ATitulo <> nil then
      ID := OnlyNumber(ATitulo.ACBrBoleto.Banco.MontarCampoNossoNumero(ATitulo));

   case Boleto.Configuracoes.WebService.Operacao of
      tpInclui:
         FPURL := FPURL + '/boletos'; // + DevAPP;
      // tpConsulta         : FPURL := FPURL + '/boletos'+ '&' + DefinirParametros; //' + DevAPP + '&' + DefinirParametros;
      tpConsultaDetalhe:
         FPURL := FPURL + '/boletos/' + ID;
         // + ID + DevAPP + '&numeroConvenio='+ NConvenio;
      tpBaixa:
         FPURL := FPURL + '/boletos/' + ID + '/baixar';
         // + ID + '/baixar'+DevAPP;
   end;

end;

procedure TBoletoW_Banrisul.DefinirContentType;
begin
   FPContentType := 'application/json';
end;

procedure TBoletoW_Banrisul.GerarHeader;
begin
   DefinirContentType;
   DefinirKeyUser;
end;

procedure TBoletoW_Banrisul.GerarDados;
begin
   if Assigned(Boleto) then
      case Boleto.Configuracoes.WebService.Operacao of
         tpInclui:
            begin
               FMetodoHTTP := htPOST; // Define Método POST para Incluir
               RequisicaoJson;
            end;
         tpBaixa:
            begin
               FMetodoHTTP := htPOST; // Define Método POST para Baixa
               RequisicaoBaixa;
            end;
         // tpConsulta :
         // begin
         // FMetodoHTTP:= htGET;   //Define Método GET Consulta
         // RequisicaoConsulta;
         // end;
         tpConsultaDetalhe:
            begin
               FMetodoHTTP := htGET; // Define Método GET Consulta Detalhe
               RequisicaoConsultaDetalhe;
            end;

      else
         raise EACBrBoletoWSException.Create(ClassName + Format(S_OPERACAO_NAO_IMPLEMENTADO,
                                            [TipoOperacaoToStr(Boleto.Configuracoes.WebService.Operacao)]));
      end;

end;

procedure TBoletoW_Banrisul.DefinirAuthorization;
begin
   FPAuthorization := C_AUTHORIZATION + ': ' + 'Bearer ' +  GerarTokenAutenticacao;
end;

function TBoletoW_Banrisul.GerarTokenAutenticacao: string;
begin
   Result := inherited GerarTokenAutenticacao;
end;

procedure TBoletoW_Banrisul.DefinirKeyUser;
begin
   if Assigned(ATitulo) then
      FPKeyUser := '';

   FPHeaders.Clear;
   FPHeaders.Add('bergs-beneficiario: ' + trim(Boleto.Cedente.Convenio));

   if Assigned(Boleto) then
   begin
      if Boleto.Configuracoes.WebService.Operacao = tpBaixa then
         FPHeaders.Add('bergs-ambiente: ' + ValidaAmbiente);
   end;

end;

procedure TBoletoW_Banrisul.DefinirAutenticacao;
begin
   FPAuthorization := C_ACCESS_TOKEN + ': ' + GerarTokenAutenticacao;
end;

function TBoletoW_Banrisul.ValidaAmbiente: String;
begin
   if Boleto.Configuracoes.WebService.VersaoDF = 'T' then
      Result := 'T'
   else
      Result := IfThen(Boleto.Configuracoes.WebService.Ambiente = taProducao, 'P', 'T');
end;

procedure TBoletoW_Banrisul.RequisicaoJson;
var
   LJsonObject: TACBrJSONObject;
begin
   if Assigned(ATitulo) then
   begin
      LJsonObject := TACBrJSONObject.Create;

      LJsonObject.AddPair('ambiente', ValidaAmbiente);

      GeraDadoBoleto(LJsonObject);

      FPDadosMsg := LJsonObject.ToJSON;
   end;
end;

procedure TBoletoW_Banrisul.GeraDadoBoleto(AJson: TACBrJSONObject);
var
   LJsonBoleto: TACBrJSONObject;
begin
   if Assigned(ATitulo) and Assigned(AJson) then
   begin

      LJsonBoleto := TACBrJSONObject.Create;
      //OnlyNumber(Boleto.Banco.MontarCampoNossoNumero(ATitulo))
      if ( StrToInt(ATitulo.NossoNumero) > 0) then
        LJsonBoleto.AddPair('nosso_numero',OnlyNumber(Boleto.Banco.MontarCampoNossoNumero(ATitulo)));

      LJsonBoleto.AddPair('seu_numero', ATitulo.NumeroDocumento);

      LJsonBoleto.AddPair('data_emissao', FormatDateBr(ATitulo.DataDocumento, 'YYYY-MM-DD'));
      LJsonBoleto.AddPair('data_vencimento', FormatDateBr(ATitulo.Vencimento, 'YYYY-MM-DD'));
      LJsonBoleto.AddPair('valor_nominal', StringReplace(FormatFloat('0.00', ATitulo.ValorDocumento),
                                                         ',', '.', [rfReplaceAll]) );
      LJsonBoleto.AddPair('especie', PadLeft(IntToStr(CodigoTipoTitulo(ATitulo.EspecieDoc)),2,'0'));

      // LJsonBoleto.AddPair('valor_iof', 0);

      LJsonBoleto.AddPair('id_titulo_empresa', Copy(trim(UpperCase(ATitulo.SeuNumero)), 0, 25));

      GerarPagador(LJsonBoleto);

      GerarIntrucoes(LJsonBoleto);

      // GerarBenificiarioFinal( LJsonBoleto );

      //if ATitulo.TipoPagamento = tpAceita_Valores_entre_Minimo_Maximo then
         GerarPag_Parcial(LJsonBoleto);

      if Boleto.Cedente.CedenteWS.IndicadorPix = True then
         GerarHibrido(LJsonBoleto);

      AJson.AddPair('titulo', LJsonBoleto);

   end;

end;

procedure TBoletoW_Banrisul.RequisicaoBaixa;
var
   LData: string;
   LJsonObject: TACBrJSONObject;
begin
   { if Assigned(ATitulo) then
     begin
     LJsonObject := TACBrJSONObject.Create;
     try
     LJsonObject.AddPair('numeroConvenio', StrToInt64Def(OnlyNumber(Boleto.Cedente.Convenio),0));
     LData := LJsonObject.ToJSON;

     FPDadosMsg := LData;

     finally
     LJsonObject.Free;
     end;
     end;
   }
end;

procedure TBoletoW_Banrisul.RequisicaoConsulta;
begin
   // Sem Payload - Define Método GET
end;

procedure TBoletoW_Banrisul.RequisicaoConsultaDetalhe;
begin
   // Sem Payload - Define Método GET
end;

procedure TBoletoW_Banrisul.GerarPagador(AJson: TACBrJSONObject);
var
   LJsonPagadorObject: TACBrJSONObject;
begin
   if Assigned(ATitulo) and Assigned(AJson) then
   begin
      LJsonPagadorObject := TACBrJSONObject.Create;
      try
         LJsonPagadorObject.AddPair('tipo_pessoa', IfThen(Length(OnlyNumber(ATitulo.Sacado.CNPJCPF)) = 11, 'F', 'J'));
         LJsonPagadorObject.AddPair('cpf_cnpj',    OnlyNumber(ATitulo.Sacado.CNPJCPF));//StrToInt64(OnlyNumber(ATitulo.Sacado.CNPJCPF)));
         LJsonPagadorObject.AddPair('nome',        Copy(trim(ATitulo.Sacado.NomeSacado), 0, 40));

         LJsonPagadorObject.AddPair('endereco',    Copy(trim(ATitulo.Sacado.Logradouro + ',' +
                                                             ATitulo.Sacado.Numero), 0, 35));

         LJsonPagadorObject.AddPair('cep',         OnlyNumber(ATitulo.Sacado.CEP));
         LJsonPagadorObject.AddPair('cidade',      Copy(trim(ATitulo.Sacado.Cidade), 0, 15));
         // LJsonPagadorObject.AddPair('bairro',      ATitulo.Sacado.Bairro);
         LJsonPagadorObject.AddPair('uf',          ATitulo.Sacado.UF);
         LJsonPagadorObject.AddPair('aceite',      IfThen(ATitulo.Aceite = atSim, 'A', 'N'));

         AJson.AddPair('pagador', LJsonPagadorObject);

      finally
         LJsonPagadorObject.Free;
      end;
   end;

end;

procedure TBoletoW_Banrisul.GerarPag_Parcial(AJson: TACBrJSONObject);
var
   LJsonPag_Parcial: TACBrJSONObject;
begin
   if Assigned(ATitulo) and Assigned(AJson) then
   begin
      LJsonPag_Parcial := TACBrJSONObject.Create;

      LJsonPag_Parcial.AddPair('autoriza', 1);
      LJsonPag_Parcial.AddPair('codigo', 3);
	  
      if ATitulo.TipoPagamento = tpAceita_Valores_entre_Minimo_Maximo then
	   begin
		   LJsonPag_Parcial.AddPair('autoriza', 2);
		   LJsonPag_Parcial.AddPair('codigo', 2);


         if (ATitulo.ValorMinPagamento > 0) and (ATitulo.ValorMaxPagamento > 0) then
            LJsonPag_Parcial.AddPair('tipo', 2)
         else
            LJsonPag_Parcial.AddPair('tipo', 1);

         LJsonPag_Parcial.AddPair('quantidade', ATitulo.QtdePagamentoParcial);


         if ATitulo.ValorMinPagamento > 0 then
            LJsonPag_Parcial.AddPair('valor_min', StringReplace(FormatFloat('0.00', ATitulo.ValorMinPagamento),
                                                                  ',', '.', [rfReplaceAll]) )
         else
         if ATitulo.PercentualMinPagamento > 0 then
            LJsonPag_Parcial.AddPair('percentual_min', StringReplace(FormatFloat('0.00', ATitulo.PercentualMinPagamento),
                                                                       ',', '.', [rfReplaceAll]) );

         if ATitulo.ValorMinPagamento > 0 then
            LJsonPag_Parcial.AddPair('valor_max', StringReplace(FormatFloat('0.00', ATitulo.ValorMaxPagamento),
                                                                 ',', '.', [rfReplaceAll]) )
         else
         if ATitulo.PercentualMaxPagamento > 0 then
            LJsonPag_Parcial.AddPair('percentual_max',  StringReplace(FormatFloat('0.00', ATitulo.PercentualMaxPagamento),
                                                      ',', '.', [rfReplaceAll]) );
      end;

      AJson.AddPair('pag_parcial', LJsonPag_Parcial);
   end;

end;

procedure TBoletoW_Banrisul.GerarBenificiarioFinal (AJson: TACBrJSONObject);
var
   LJsonSacadorAvalista: TACBrJSONObject;
begin
   if Assigned(ATitulo) and Assigned(AJson) then
   begin
      if ATitulo.Sacado.SacadoAvalista.CNPJCPF = EmptyStr then
         Exit;

      LJsonSacadorAvalista := TACBrJSONObject.Create;

      LJsonSacadorAvalista.AddPair('tipoInscricao',
                                    StrToInt(IfThen(Length(OnlyNumber(ATitulo.Sacado.SacadoAvalista.CNPJCPF)) = 11, '1', '2')));

      LJsonSacadorAvalista.AddPair('numeroInscricao',
                                    StrToInt64Def(OnlyNumber(ATitulo.Sacado.SacadoAvalista.CNPJCPF), 0));

      LJsonSacadorAvalista.AddPair('nome',   ATitulo.Sacado.SacadoAvalista.NomeAvalista);

      AJson.AddPair('beneficiarioFinal', LJsonSacadorAvalista);
   end;
end;

procedure TBoletoW_Banrisul.GerarIntrucoes(AJson: TACBrJSONObject);
var
   LJsonInstrucoesObject: TACBrJSONObject;
begin
   if Assigned(ATitulo) and Assigned(AJson) then
   begin
      LJsonInstrucoesObject := TACBrJSONObject.Create;
      try
         GerarJuros(LJsonInstrucoesObject);
         GerarMulta(LJsonInstrucoesObject);
         GerarDesconto(LJsonInstrucoesObject);
         GerarProtesto(LJsonInstrucoesObject);

         AJson.AddPair('instrucoes', LJsonInstrucoesObject);

      finally
         LJsonInstrucoesObject.Free;
      end;
   end;
end;

procedure TBoletoW_Banrisul.GerarJuros(AJson: TACBrJSONObject);
var
   LJsonJurosObject: TACBrJSONObject;
begin
   if Assigned(ATitulo) and Assigned(AJson) then
   begin
      LJsonJurosObject := TACBrJSONObject.Create;
      if (ATitulo.ValorMoraJuros > 0) then
      begin
         if ATitulo.CodigoMora = '' then
         begin
            case ATitulo.CodigoMoraJuros of
               cjValorDia:    ATitulo.CodigoMora := '1';
               cjTaxaMensal:  ATitulo.CodigoMora := '2';
               cjIsento:      ATitulo.CodigoMora := '3';
            end;
         end;

         LJsonJurosObject.AddPair('codigo', StrToIntDef(ATitulo.CodigoMora, 3));
         LJsonJurosObject.AddPair('data', FormatDateBr(ATitulo.DataMoraJuros,'YYYY-MM-DD'));

         case (StrToIntDef(ATitulo.CodigoMora, 3)) of
            1: LJsonJurosObject.AddPair('valor', StringReplace(FormatFloat('0.00', ATitulo.ValorMoraJuros),
                                                                ',', '.', [rfReplaceAll]) );
            2: LJsonJurosObject.AddPair('taxa', StringReplace(FormatFloat('0.00', ATitulo.ValorMoraJuros),
                                                                ',', '.', [rfReplaceAll]) );
         end;

         AJson.AddPair('juros', LJsonJurosObject);

      end;
   end;
end;

procedure TBoletoW_Banrisul.GerarMulta(AJson: TACBrJSONObject);
var
   LJsonMultaObject: TACBrJSONObject;
   LCodigoMulta: Byte;
begin
   if Assigned(ATitulo) and Assigned(AJson) then
   begin
      LJsonMultaObject := TACBrJSONObject.Create;

      if ATitulo.PercentualMulta > 0 then
      begin
         if ATitulo.MultaValorFixo then
            LCodigoMulta := 1
         else
            LCodigoMulta := 2;
      end
      else
         LCodigoMulta := 0;

      if (ATitulo.DataMulta > 0) then
      begin
         LJsonMultaObject.AddPair('codigo', LCodigoMulta);

         if (LCodigoMulta > 0) then
            LJsonMultaObject.AddPair('data', FormatDateBr(ATitulo.DataMulta, 'YYYY-MM-DD'));

         case LCodigoMulta of
            1: LJsonMultaObject.AddPair('valor', StringReplace(FormatFloat('0.0', ATitulo.PercentualMulta),
                                                                ',', '.', [rfReplaceAll]) );
            2: LJsonMultaObject.AddPair('taxa', StringReplace(FormatFloat('0.0', ATitulo.PercentualMulta),
                                                                ',', '.', [rfReplaceAll]) );
         end;

         AJson.AddPair('multa', LJsonMultaObject);

      end;
   end;
end;

procedure TBoletoW_Banrisul.GerarDesconto(AJson: TACBrJSONObject);
var
   LJsonDescontoObject, LJsonDescontoObject2: TACBrJSONObject;
begin
   if Assigned(ATitulo) and Assigned(AJson) then
   begin
      LJsonDescontoObject := TACBrJSONObject.Create;
      if (ATitulo.DataDesconto > 0) then
      begin
         LJsonDescontoObject.AddPair('codigo', Integer(ATitulo.TipoDesconto));
         LJsonDescontoObject.AddPair('data', FormatDateBr(ATitulo.DataDesconto, 'YYYY-MM-DD'));

         case Integer(ATitulo.TipoDesconto) of
            1, 3: LJsonDescontoObject.AddPair('valor', StringReplace(FormatFloat('0.00', ATitulo.ValorDesconto),
                                                                     ',', '.', [rfReplaceAll]) );
            2, 5: LJsonDescontoObject.AddPair('taxa', StringReplace(FormatFloat('0.00', ATitulo.ValorDesconto),
                                                                      ',', '.', [rfReplaceAll]) );
         end;

         AJson.AddPair('desconto', LJsonDescontoObject);

      end;

   end;
end;

procedure TBoletoW_Banrisul.GerarProtesto(AJson: TACBrJSONObject);
var
   LJsonProtestoObject: TACBrJSONObject;
   LCodigoProtesto: Byte;
begin
   if Assigned(ATitulo) and Assigned(AJson) then
   begin
      LJsonProtestoObject := TACBrJSONObject.Create;

      if ATitulo.DataProtesto > 0 then
         LCodigoProtesto := 1
      else
         LCodigoProtesto := 3;

      if (ATitulo.DataProtesto > 0) and (LCodigoProtesto = 1) then
      begin
         LJsonProtestoObject.AddPair('codigo', LCodigoProtesto);
         LJsonProtestoObject.AddPair('prazo', ATitulo.DiasDeProtesto);

         AJson.AddPair('protesto', LJsonProtestoObject);

      end;
   end;

end;

procedure TBoletoW_Banrisul.GerarHibrido(AJson: TACBrJSONObject);
var
   LJsonHibridoObject: TACBrJSONObject;
begin
   if Assigned(ATitulo) and Assigned(AJson) then
   begin
      LJsonHibridoObject := TACBrJSONObject.Create;

      LJsonHibridoObject.AddPair('autoriza', 'S');
      //LJsonHibridoObject.AddPair('situacao', 'P');
      // QRCODE pendente, deve consultar para obter
      // LJsonHibridoObject.AddPair('txid', ATitulo.QrCode.txId);

      AJson.AddPair('hibrido', LJsonHibridoObject);

   end;
end;

function TBoletoW_Banrisul.CodigoTipoTitulo (AEspecieDoc: String): Integer;
begin
   if AEspecieDoc = 'CH' then
      AEspecieDoc := '01'
   else if AEspecieDoc = 'DM' then
      AEspecieDoc := '02'
   else if AEspecieDoc = 'DMI' then
      AEspecieDoc := '03'
   else if AEspecieDoc = 'DS' then
      AEspecieDoc := '04'
   else if AEspecieDoc = 'DSI' then
      AEspecieDoc := '05'
   else if AEspecieDoc = 'DR' then
      AEspecieDoc := '06'
   else if AEspecieDoc = 'LC' then
      AEspecieDoc := '07'
   else if AEspecieDoc = 'NCC' then
      AEspecieDoc := '08'
   else if AEspecieDoc = 'NCE' then
      AEspecieDoc := '09'
   else if AEspecieDoc = 'NCI' then
      AEspecieDoc := '10'
   else if AEspecieDoc = 'NCR' then
      AEspecieDoc := '11'
   else if AEspecieDoc = 'NP' then
      AEspecieDoc := '12'
   else if AEspecieDoc = 'NPR' then
      AEspecieDoc := '13'
   else if AEspecieDoc = 'TM' then
      AEspecieDoc := '14'
   else if AEspecieDoc = 'TS' then
      AEspecieDoc := '15'
   else if AEspecieDoc = 'NS' then
      AEspecieDoc := '16'
   else if AEspecieDoc = 'RC' then
      AEspecieDoc := '17'
   else if AEspecieDoc = 'FAT' then
      AEspecieDoc := '18'
   else if AEspecieDoc = 'ND' then
      AEspecieDoc := '19'
   else if AEspecieDoc = 'AP' then
      AEspecieDoc := '20'
   else if AEspecieDoc = 'ME' then
      AEspecieDoc := '21'
   else if AEspecieDoc = 'PC' then
      AEspecieDoc := '22';

   Result := StrToIntDef(AEspecieDoc, 0);
end;

constructor TBoletoW_Banrisul.Create(ABoletoWS: TBoletoWS);
begin
   inherited Create(ABoletoWS);

   FPAccept := C_ACCEPT;

   if Assigned(OAuth) then
   begin
      OAuth.URL := IfThen(OAuth.Ambiente = taHomologacao, C_URL_OAUTH_HOM, C_URL_OAUTH_PROD);

      OAuth.Payload := True;
   end;
end;

function TBoletoW_Banrisul.GerarRemessa: string;
begin
   Result := inherited GerarRemessa;
end;

function TBoletoW_Banrisul.Enviar: boolean;
begin
   Result := inherited Enviar;
end;

end.
