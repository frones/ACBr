{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
{ Colaboradores nesse arquivo:  Victor Hugo Gonzales - Panda                   }
{                               Igless                                         }
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


unit ACBrBoletoW_Credisis;

interface

Uses
   Classes,
   SysUtils,
   DateUtils,
   ACBrBoletoWS,

   pcnConversao,
   pcnGerador,
   ACBrBoletoConversao,
   //Soap.InvokeRegistry,
   //Soap.SOAPHTTPClient,
   //Soap.XSBuiltIns,
   Types,

   ACBrValidador,
   ACBrBoleto;

const
    IS_OPTN = $0001;
    IS_UNBD = $0002;
    IS_UNQL = $0008;


Type

//  TipoOperacao = (INCLUSAO, ALTERACAO, BAIXA, BAIXA_MANUAL, CANCELAMENTO);

 { TBoletoW_Credisis }
  TBoletoW_Credisis  = class(TBoletoWSSOAP)
  private

  protected
    procedure DefinirEnvelopeSoap; override;
    procedure DefinirRootElement; override;

    procedure GerarHeader; override;
    procedure GerarDados; override;


    procedure GerarTitulo;
    procedure GerarAvalista;
    procedure GerarPagador;
    Procedure GeraProtesto;
    Procedure GeraMulta;
    Procedure GeraJuros;
    Procedure GeraDesconto;

    procedure DefinirURL; override;

    function  Modulo11(Valor: String; Base: Integer = 9; Resto : boolean = false) : string;



  public
    constructor Create(ABoletoWS: TBoletoWS); override;

    function GerarRemessa: String; override;
    function Enviar: Boolean; override;
  end;

Const
  C_CredSis_URL = 'https://credisiscobranca.com.br/v2/ws?wsdl';
  C_CredSis_SOAP_ATTRIBUTTES = 'xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" '+
                       'xmlns:urn="urn:CredisisBoletoInterface" xmlns:urn1="urn:CredisisBoletoInterface-CredisisWebService"';

implementation

uses
 pcnConsts, ACBrBoletoPcnConsts, ACBrUtil.Base, ACBrUtil.XMLHTML, ACBrUtil.Strings;


{ TBoletoW_Credisis }

constructor TBoletoW_Credisis.Create(ABoletoWS: TBoletoWS);
begin
    inherited Create(ABoletoWS);
    FPSoapEnvelopeAtributtes := C_CredSis_SOAP_ATTRIBUTTES;
end;

procedure TBoletoW_Credisis.DefinirEnvelopeSoap;
var Texto: String;

begin
  {$IFDEF FPC}
   Texto := '<' + ENCODING_UTF8 + '>';    // Envelope já está sendo montado em UTF8
  {$ELSE}
   Texto := '';  // Isso forçará a conversão para UTF8, antes do envio
  {$ENDIF}

  FPDadosMsg := RemoverDeclaracaoXML(FPDadosMsg);

  Texto := Texto + '<' + FPSoapVersion + ':Envelope ' + FPSoapEnvelopeAtributtes + '>';
  Texto := Texto + '  <' + FPSoapVersion + ':Header>';
  Texto := Texto + '    <urn:Chave>';
  Texto := Texto + '      <token>'+ ATitulo.ACBrBoleto.Cedente.CedenteWS.ClientSecret +'</token>';
  Texto := Texto + '      <convenio>'+ATitulo.ACBrBoleto.Cedente.Convenio+'</convenio>';
  Texto := Texto + '    </urn:Chave>';
  Texto := Texto + '  </soapenv:Header>';
  Texto := Texto + '<' + FPSoapVersion + ':Body>';
  Texto := Texto + '  <urn1:gerarBoletos>';
  Texto := Texto + '     <layout>'+'default'+'</layout>';
  Texto := Texto + FPDadosMsg;
  Texto := Texto + '  </urn1:gerarBoletos>';
  Texto := Texto + '</' + FPSoapVersion + ':Body>';
  Texto := Texto + '</' + FPSoapVersion + ':Envelope>';

  FPEnvelopeSoap := Texto;

  FPSoapAction  := 'INCLUSAO';
  FPMimeType    := '';
end;

procedure TBoletoW_Credisis.DefinirRootElement;
begin
    FPRootElement:= '';
    FPCloseRootElement:= '';
end;

procedure TBoletoW_Credisis.GerarHeader;
begin

end;

procedure TBoletoW_Credisis.DefinirURL;
begin
    FPURL := C_CredSis_URL;
end;

function TBoletoW_Credisis.Enviar: Boolean;
begin
    Result:=inherited Enviar;
end;

procedure TBoletoW_Credisis.GeraDesconto;
Var iTipo : Integer;
begin
    if Assigned(ATitulo) then
    Begin
        iTipo := 0;
        if ( ATitulo.ValorDesconto > 0 ) then
        Begin
            case ATitulo.CodigoDesconto of
               cdSemDesconto : iTipo := 3;
               cdValorFixo   : iTipo := 2;
            end;
            if iTipo = 0 then
             iTipo := 1;

            Gerador.wGrupo('desconto1');
            Gerador.wCampo(tcDe2, '#01', 'valor', 01, 15, 1, ATitulo.ValorDesconto, DSC_VALOR_DESCONTO);
            Gerador.wCampo(tcDat, '#02', 'data', 10, 10, 1, ATitulo.DataDesconto, DSC_DATA_DESCONTO);
            Gerador.wCampo(tcInt, '#03', 'tipo', 01, 10, 1, iTipo, DSC_DATA_DESCONTO);
            Gerador.wGrupo('/desconto1');
        End;
        if ( ATitulo.ValorDesconto2 > 0 ) then
        Begin
            case ATitulo.CodigoDesconto of
               cdSemDesconto : iTipo := 3;
               cdValorFixo   : iTipo := 2;
            end;
            if iTipo = 0 then
             iTipo := 1;

            Gerador.wGrupo('desconto2');
            Gerador.wCampo(tcDe2, '#01', 'valor', 01, 15, 1, ATitulo.ValorDesconto2, DSC_VALOR_DESCONTO);
            Gerador.wCampo(tcDat, '#02', 'data', 10, 10, 1, ATitulo.DataDesconto2, DSC_DATA_DESCONTO);
            Gerador.wCampo(tcInt, '#03', 'tipo', 01, 10, 1, iTipo, DSC_DATA_DESCONTO);
            Gerador.wGrupo('/desconto2');
        End;
        if ( ATitulo.ValorDesconto3 > 0 ) then
        Begin
            case ATitulo.CodigoDesconto of
               cdSemDesconto : iTipo := 3;
               cdValorFixo   : iTipo := 2;
            end;
            if iTipo = 0 then
             iTipo := 1;

            Gerador.wGrupo('desconto3');
            Gerador.wCampo(tcDe2, '#01', 'valor', 01, 15, 1, ATitulo.ValorDesconto3, DSC_VALOR_DESCONTO);
            Gerador.wCampo(tcDat, '#02', 'data', 10, 10, 1, ATitulo.DataDesconto3, DSC_DATA_DESCONTO);
            Gerador.wCampo(tcInt, '#03', 'tipo', 01, 10, 1, iTipo, DSC_DATA_DESCONTO);
            Gerador.wGrupo('/desconto3');
        End;

    End;
end;

procedure TBoletoW_Credisis.GeraJuros;
Var iTipo : Integer;
    iDias : Integer;
begin
    if Assigned(ATitulo) then
    Begin
        if ( ATitulo.ValorMoraJuros > 0 ) and ( ATitulo.DataMoraJuros > 0) then
        Begin
            iTipo := 0;
            iDias := 0;
            case ATitulo.CodigoMoraJuros of
               cjValorDia,
               cjValorMensal : iTipo := 1;
               cjTaxaMensal,
               cjTaxaDiaria  : iTipo := 2;
               cjIsento      : iTipo := 3;
            end;

            if ATitulo.DataMoraJuros <> ATitulo.Vencimento then
             iDias := DaysBetween(ATitulo.Vencimento, ATitulo.DataMoraJuros);

            Gerador.wGrupo('juros');
            Gerador.wCampo(tcDe2, '#01', 'valor', 01, 15, 1, ATitulo.ValorMoraJuros, DSC_VALOR_MORA_JUROS);
            Gerador.wGrupo('carencia');
            Gerador.wCampo(tcInt, '#01', 'dias', 01, 10, 1, iDias, DSC_DIAS_PROTESTO);
            Gerador.wCampo(tcInt, '#02', 'tipo', 01, 10, 1, iTipo, DSC_CODIGO_MORA_JUROS);
            Gerador.wGrupo('/carencia');
            Gerador.wCampo(tcInt, '#01', 'tipo', 01, 10, 1, iTipo, DSC_CODIGO_MORA_JUROS);
            Gerador.wGrupo('/juros');
        End;
    End;

end;

procedure TBoletoW_Credisis.GeraMulta;
Var iTipo : Integer;
    iDias : Integer;
begin
    if Assigned(ATitulo) then
    Begin
        if ( ATitulo.PercentualMulta > 0 ) and ( ATitulo.DataMulta > 0) then
        Begin
            iTipo := 0;
            iDias := 0;

            Case ATitulo.CodigoMulta Of
              cmValorFixo  : iTipo := 1;
              cmPercentual : iTipo := 2;
              cmIsento     : iTipo := 3;
            End;

            if ATitulo.DataMulta <> ATitulo.Vencimento then
             iDias := DaysBetween(ATitulo.Vencimento, ATitulo.DataMulta);

            Gerador.wGrupo('multa');
            Gerador.wCampo(tcDe2, '#01', 'valor', 01, 15, 1, ATitulo.PercentualMulta, DSC_PERCENTUAL_MULTA);
            Gerador.wGrupo('carencia');
            Gerador.wCampo(tcInt, '#01', 'dias', 01, 10, 1, iDias, DSC_DIAS_PROTESTO);
            Gerador.wCampo(tcInt, '#02', 'tipo', 01, 10, 1, iTipo, DSC_DIAS_PROTESTO);
            Gerador.wGrupo('/carencia');
            Gerador.wCampo(tcInt, '#02', 'tipo', 01, 10, 1, iTipo, DSC_DIAS_PROTESTO);
            Gerador.wGrupo('/multa');
        End;
    End;
end;

procedure TBoletoW_Credisis.GeraProtesto;
Var iTipo : Integer;
begin
    if Assigned(ATitulo) then
    Begin
        iTipo := 0;

        Case ATitulo.CodigoNegativacao Of
         cnNenhum           : iTipo := 1;
         cnProtestarCorrido : iTipo := 2;
         cnProtestarUteis   : iTipo := 1;
         cnNaoProtestar     : iTipo := 1;
         cnNegativar        : iTipo := 1;
         cnNaoNegativar     : iTipo := 1;
         cnCancelamento     : iTipo := 1;
        End;

        Gerador.wGrupo('protesto');
        Gerador.wCampo(tcStr, '#01', 'tipo', 01, 1, 1, iTipo, DSC_DIAS_PROTESTO);
        Gerador.wCampo(tcInt, '#02', 'dias', 01, 2, 1, ATitulo.DiasDeProtesto, DSC_DIAS_PROTESTO);
        Gerador.wGrupo('/protesto');
    End;
end;

procedure TBoletoW_Credisis.GerarAvalista;
begin
  if Assigned(ATitulo) then
  Begin
      if NaoEstaVazio(ATitulo.Sacado.SacadoAvalista.CNPJCPF) then
      begin
          Gerador.wGrupo('avalista');

          if (Integer(ATitulo.Sacado.SacadoAvalista.Pessoa) = 0) then
           Gerador.wCampo(tcStr, '#01', 'nome', 01, 40, 1, ATitulo.Sacado.SacadoAvalista.NomeAvalista, DSC_NOME_AVALISTA)
          else
           Gerador.wCampo(tcStr, '#01', 'nome', 01, 40, 1, ATitulo.Sacado.SacadoAvalista.NomeAvalista, DSC_NOME_AVALISTA);

          Gerador.wCampoCNPJCPF('#02', 'cpfCnpj', ATitulo.Sacado.SacadoAvalista.CNPJCPF);

         Gerador.wGrupo('/avalista');
      end;
  End;
end;

procedure TBoletoW_Credisis.GerarDados;
begin
  if Assigned(ATitulo) then
    with ATitulo do
    begin
      case Boleto.Configuracoes.WebService.Operacao of
        tpInclui: GerarTitulo;
      else
        raise EACBrBoletoWSException.Create(ClassName + Format( S_OPERACAO_NAO_IMPLEMENTADO, [
                                                  TipoOperacaoToStr( Boleto.Configuracoes.WebService.Operacao ) ] ));
      end;
    end;
end;


procedure TBoletoW_Credisis.GerarPagador;
begin
    if Assigned(ATitulo) then
    Begin
        Gerador.wGrupo('pagador');
        Gerador.wCampo(tcStr, '#01', 'nome', 01, 60, 1, ATitulo.Sacado.NomeSacado, DSC_NOME_SACADO);
        Gerador.wCampo(tcStr, '#02', 'cpfCnpj', 01, 14, 1, ATitulo.Sacado.CNPJCPF, DSC_CPF);

        Gerador.wGrupo('endereco');
        Gerador.wCampo(tcStr, '#03', 'endereco', 01, 40, 1, ATitulo.Sacado.Logradouro, DSC_LOGRADOURO);
        Gerador.wCampo(tcStr, '#04', 'bairro'  , 01, 15, 1, ATitulo.Sacado.Bairro, DSC_BAIRRO);
        Gerador.wCampo(tcStr, '#05', 'cep'     , 08, 08, 1, ATitulo.Sacado.Cep, DSC_CEP);
        Gerador.wCampo(tcStr, '#06', 'cidade'  , 01, 15, 1, ATitulo.Sacado.Cidade, DSC_CIDADE);
        Gerador.wCampo(tcStr, '#07', 'uf'      , 02, 02, 1, ATitulo.Sacado.UF, DSC_UF);
        Gerador.wCampo(tcStr, '#08', 'numero'  , 02, 10, 1, ATitulo.Sacado.Numero, DSC_NUMERO_SACADO);
        Gerador.wGrupo('/endereco');

        Gerador.wGrupo('contatos');
        Gerador.wGrupo('item');
        Gerador.wCampo(tcStr, '#09', 'contato'    , 02, 10, 1, ATitulo.Sacado.Fone, DSC_FONE );
        Gerador.wCampo(tcInt, '#10', 'tipoContato', 01, 10, 1, 1, DSC_FONE );

        Gerador.wGrupo('/item');
        Gerador.wGrupo('/contatos');
        Gerador.wGrupo('/pagador');
    end;
end;

function TBoletoW_Credisis.GerarRemessa: String;
begin
    Result:=inherited GerarRemessa;
end;

procedure TBoletoW_Credisis.GerarTitulo;
Var vNossoNumero:String;
    FCalculoDigito : TACBrCalcDigito;

    Function CalcularDigitoVerificador(const ACBrTitulo: TACBrTitulo): string;
    Begin
        FCalculoDigito.CalculoPadrao;
        FCalculoDigito.Documento := ACBrTitulo.NossoNumero;
        FCalculoDigito.Calcular;
        if FCalculoDigito.ModuloFinal = 0 then
         Result := '1'
        else
         Result := IntToStr(FCalculoDigito.DigitoFinal);
    End;

begin
    if Assigned(ATitulo) then
    Begin
        FCalculoDigito := TACBrCalcDigito.Create;

        vNossoNumero := '097'+
                        CalcularDigitoVerificador(ATitulo)+
                        PadLeft(ATitulo.ACBrBoleto.Cedente.Agencia,4,'0')+
                        PadLeft(ATitulo.ACBrBoleto.Cedente.Convenio,6,'0')+
                        PadLeft(ATitulo.NossoNumero,6,'0');

        Gerador.wGrupo('boletos');
        Gerador.wGrupo('boleto');

        GerarAvalista;
        GerarPagador;

        Gerador.wCampo(tcStr, '#01', 'documento', 11, 11, 1, ATitulo.NumeroDocumento, DSC_NUMERO_DOCUMENTO);
        Gerador.wCampo(tcDat, '#02', 'dataEmissao', 10, 10, 1, ATitulo.DataDocumento, DSC_DATA_DOCUMENTO);
        Gerador.wCampo(tcDat, '#03', 'dataVencimento', 10, 10, 1, ATitulo.Vencimento, DSC_DATA_VENCIMENTO);
        Gerador.wCampo(tcDat, '#04', 'dataLimitePagamento', 10, 10, 1, ATitulo.DataLimitePagto, DSC_DATA_LIMITE_PAGAMENTO);
        Gerador.wCampo(tcDe2, '#05', 'valor', 01, 15, 1, ATitulo.ValorDocumento, DSC_VALOR_DOCUMENTO);
        //<!--Optional:-->
        //Gerador.wCampo(tcInt, '#10', 'grupoParcela', 1, 10, 1, 1, '' );
        //<grupoParcela>?</grupoParcela>

        //<!--Optional:-->
        Gerador.wCampo(tcStr, '#07', 'nossonumero', 1, 30, 1, vNossoNumero, DSC_NOSSO_NUMERO);
        //<!--Optional:-->
        Gerador.wCampo(tcInt, '#08', 'quantidadeParcelas', 1, 1, 1, ATitulo.Parcela, DSC_NNFINI );
        //<!--Optional:-->
        Gerador.wCampo(tcInt, '#09', 'intervaloParcela', 1, 1, 1, ATitulo.TotalParcelas, DSC_NNFFIN);

        //<!--Optional:-->
        //<layout>'default_'</layout>

        //<!--Optional:-->
        //<formato>?</formato>

        If ATitulo.EspecieDoc = 'DMI' Then
         Gerador.wCampo(tcStr, '#12', 'codigoEspecie', 01, 15, 1, '03', DSC_TIPO_ESPECIE)
        Else If ATitulo.EspecieDoc = 'DSI' Then
         Gerador.wCampo(tcStr, '#12', 'codigoEspecie', 01, 15, 1, '05', DSC_TIPO_ESPECIE)
        Else If ATitulo.EspecieDoc = 'NP' Then
         Gerador.wCampo(tcStr, '#12', 'codigoEspecie', 01, 15, 1, '12', DSC_TIPO_ESPECIE)
        Else If ATitulo.EspecieDoc = 'RC' Then
         Gerador.wCampo(tcStr, '#12', 'codigoEspecie', 01, 15, 1, '17', DSC_TIPO_ESPECIE)
        Else If ATitulo.EspecieDoc = 'AP' Then
         Gerador.wCampo(tcStr, '#12', 'codigoEspecie', 01, 15, 1, '20', DSC_TIPO_ESPECIE)
        Else If ATitulo.EspecieDoc = 'ME' Then
         Gerador.wCampo(tcStr, '#12', 'codigoEspecie', 01, 15, 1, '21', DSC_TIPO_ESPECIE)
        Else If ATitulo.EspecieDoc = 'NF' Then
         Gerador.wCampo(tcStr, '#12', 'codigoEspecie', 01, 15, 1, '23', DSC_TIPO_ESPECIE);

        GeraProtesto;

        //<!--Optional:-->
        //<tipoEnvio>?</tipoEnvio>
        //<!--Optional:-->

        Gerador.wCampo(tcStr, '#13', 'instrucao', 01, 100, 1, ATitulo.Mensagem.Text, DSC_INSTRUCAO1);

        GeraMulta;
        GeraJuros;
        GeraDesconto;

        Gerador.wGrupo('/boleto');
        Gerador.wGrupo('/boletos');

        FreeAndNil(FCalculoDigito);
    End;
end;

function TBoletoW_Credisis.Modulo11(Valor: String; Base: Integer; Resto: boolean): string;
var
   Soma : integer;
   Contador, Peso, Digito : integer;
begin
   Valor := Trim(valor);
   Soma := 0;
   Peso := 2;
   for Contador := Length(Valor) downto 1 do
   begin
      Soma := Soma + (StrToInt(Valor[Contador]) * Peso);
      if Peso < Base then
         Peso := Peso + 1
      else
         Peso := 2;
   end;

   if Resto then
      Result := IntToStr(Soma mod 11)
   else
   begin
      Digito := 11 - (Soma mod 11);
      if (Digito > 9) then
         Digito := 0;
      Result := IntToStr(Digito);
   end
end;


end.
