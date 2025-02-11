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
  strutils,
  ACBrBoletoWS,
  ACBrBoletoWS.Rest,
  ACBrJSON, 
  ACBrUtil.Strings;

type
  { TBoletoW_PenseBank_API }
  TBoletoW_PenseBank_API = class(TBoletoWSREST)
  private
    function CodigoTipoTitulo(AEspecieDoc:string): Integer;
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
    procedure GerarPagador(AJson: TACBrJSONObject);
    procedure GerarBenificiarioFinal(AJson: TACBrJSONObject);
    procedure GerarJuros(AJson: TACBrJSONObject);
    procedure GerarMulta(AJson: TACBrJSONObject);
    procedure GerarDesconto(AJson: TACBrJSONObject);

    procedure AlteraDataVencimento(AJson: TACBrJSONObject);
    procedure AlteraPedidoNegativacao(AJson: TACBrJSONObject);
    procedure AlterarProtesto(AJson: TACBrJSONObject);

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

  CHARS_VALIDOS : TSetOfChars = ['A'..'Z','0'..'9',
                                 ' ','-','.',
                                 'À','Á','Â','Ã','Ä','Å',
                                 'È','É','Ê','Ë',
                                 'Ì','Í','Î','Ï',
                                 'Ò','Ó','Ô','Õ','Ö',
                                 'Ù','Ú','Û','Ü'];
implementation

uses
  ACBrUtil.DateTime,
  ACBrBoleto,
  SysUtils,
  pcnConversao,
  ACBrBoletoConversao, 
  ACBrBancoPenseBank;

{ TBoletoW_PenseBank_API }

procedure TBoletoW_PenseBank_API.DefinirURL;
begin
  FPURL     := IfThen(Boleto.Configuracoes.WebService.Ambiente = tawsProducao,C_URL, C_URL_HOM);

  case Boleto.Configuracoes.WebService.Operacao of
    tpInclui           : FPURL := FPURL + '/Boleto';
    tpConsulta         : FPURL := FPURL + '/BoletoConsulta';
    tpConsultaDetalhe  : FPURL := FPURL + '/BoletoConsultaLista';
    tpAltera           :
      begin
        case ATitulo.OcorrenciaOriginal.Tipo of
         ToRemessaAlterarVencimento:
           FPURL := FPURL + '/BoletoProrrogacao';
          ToRemessaProtestar:
            FPURL := FPURL + '/BoletoProtesto';
          ToRemessaPedidoNegativacao, ToRemessaNegativacaoSerasa, ToRemessaExcluirNegativacaoBaixar, ToRemessaExcluirNegativacaoSerasaBaixar:
            FPURL := FPURL + '/BoletoNegativacao';
        end;
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
       raise EACBrBoletoWSException.Create(ClassName + Format(S_OPERACAO_NAO_IMPLEMENTADO, [TipoOperacaoToStr(Boleto.Configuracoes.WebService.Operacao)]));
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
  Result := StrToIntDef(IfThen(Boleto.Configuracoes.WebService.Ambiente = tawsProducao, '1','2'),2);
end;

procedure TBoletoW_PenseBank_API.RequisicaoJson;
var
  LJsonObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    LJsonObject := TACBrJSONObject.Create;
    try
      LJsonObject.AddPair('idexterno', ATitulo.SeuNumero);
      LJsonObject.AddPair('dataEmissao', FormatDateBr(ATitulo.DataDocumento, 'DD/MM/YYYY'));
      LJsonObject.AddPair('dataVencimento', FormatDateBr(ATitulo.Vencimento, 'DD/MM/YYYY'));
      LJsonObject.AddPair('valorOriginal', ATitulo.ValorDocumento);
      LJsonObject.AddPair('valorAbatimento', ATitulo.ValorAbatimento);

      if (ATitulo.DataProtesto > 0) then
        LJsonObject.AddPair('quantidadeDiasProtesto', Trunc(ATitulo.DataProtesto - ATitulo.Vencimento));

      if (ATitulo.DiasDeNegativacao > 0) then
      begin
        LJsonObject.AddPair('quantidadeDiasNegativacao', ATitulo.DiasDeNegativacao);
        LJsonObject.AddPair('orgaoNegativador', StrToInt64Def(ATitulo.orgaoNegativador,0));
      end;

      if (ATitulo.DataLimitePagto > 0 ) then
      begin
        LJsonObject.AddPair('indicadorAceiteTituloVencido', 'S');
        LJsonObject.AddPair('numeroDiasLimiteRecebimento', Trunc(ATitulo.DataLimitePagto - ATitulo.Vencimento));
      end
      else
      begin
        LJsonObject.AddPair('indicadorAceiteTituloVencido', 'N');
        LJsonObject.AddPair('numeroDiasLimiteRecebimento', 0);
      end;

      LJsonObject.AddPair('codigoAceite', IfThen(ATitulo.Aceite = atSim,'A','N'));
      LJsonObject.AddPair('codigoTipoTitulo', codigoTipoTitulo(ATitulo.EspecieDoc));
      LJsonObject.AddPair('descricaoTipoTitulo', ATitulo.EspecieDoc);

      if ATitulo.TipoPagamento = tpAceita_Qualquer_Valor then
        LJsonObject.AddPair('indicadorPermissaoRecebimentoParcial', 'S');

      LJsonObject.AddPair('campoUtilizacaoBeneficiario',Trim(Copy(OnlyCharsInSet(AnsiUpperCase(ATitulo.NumeroDocumento),CHARS_VALIDOS),0,30)));
      LJsonObject.AddPair('numeroTituloBeneficiario', Copy(Trim(UpperCase(IfThen(ATitulo.SeuNumero<>'',ATitulo.SeuNumero,ATitulo.NumeroDocumento))),0,15));
      //LJsonObject.AddPair('numeroTituloCliente', Boleto.Banco.MontarCampoNossoNumero(ATitulo));
      LJsonObject.AddPair('mensagemBloquetoOcorrencia', UpperCase(Copy(Trim(ATitulo.Mensagem.Text),0,30)));


      GerarDesconto(LJsonObject);
      GerarJuros(LJsonObject);
      GerarMulta(LJsonObject);
      GerarPagador(LJsonObject);
      GerarBenificiarioFinal(LJsonObject);

      LJsonObject.AddPair('cnpjSh', FPKeyUser);
      LJsonObject.AddPair('pix', Boleto.Cedente.CedenteWS.IndicadorPix);
      LJsonObject.AddPair('emailGeracao', Boleto.Cedente.CedenteWS.IndicadorEmail);
      LJsonObject.AddPair('sms', Boleto.Cedente.CedenteWS.IndicadorSMS);
      LJsonObject.AddPair('fatura', False);

      FPDadosMsg := LJsonObject.ToJSON;
    finally
      LJsonObject.Free;
    end;
  end;
end;

procedure TBoletoW_PenseBank_API.RequisicaoAltera;
var
  LJsonObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    LJsonObject := TACBrJSONObject.Create;
    try
      case ATitulo.OcorrenciaOriginal.Tipo of
        ToRemessaAlterarVencimento:
          AlteraDataVencimento(LJsonObject);
        ToRemessaProtestar:
          AlterarProtesto(LJsonObject);
        ToRemessaPedidoNegativacao, ToRemessaNegativacaoSerasa, ToRemessaExcluirNegativacaoBaixar, ToRemessaExcluirNegativacaoSerasaBaixar:
          AlteraPedidoNegativacao(LJsonObject);
      end;
      FPDadosMsg := LJsonObject.ToJSON;
    finally
      LJsonObject.Free;
    end;
  end;
end;

procedure TBoletoW_PenseBank_API.RequisicaoBaixa;
var
  LJsonObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    LJsonObject := TACBrJSONObject.Create;
    LJsonObject.AddPair('idboleto', ATitulo.NumeroDocumento);
    LJsonObject.AddPair('numeroTituloCliente', Boleto.Banco.MontarCampoNossoNumero(ATitulo));

    FPDadosMsg := LJsonObject.ToJSON;
  end;
end;

procedure TBoletoW_PenseBank_API.RequisicaoCancelar;
var
  LJsonObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) then
  begin
    LJsonObject := TACBrJSONObject.Create;

    LJsonObject.AddPair('idboleto', ATitulo.NumeroDocumento);
    LJsonObject.AddPair('numeroTituloCliente', Boleto.Banco.MontarCampoNossoNumero(ATitulo));

    FPDadosMsg := LJsonObject.ToJSON;
  end;
end;

procedure TBoletoW_PenseBank_API.RequisicaoConsulta;
var
  LJsonObject: TACBrJSONObject;
  LIdBoleto : string;
  LNossoNumero : Boolean;
  LTamNossoNumero : Integer;
begin
  if Assigned(ATitulo) then
  begin
    LJsonObject := TACBrJSONObject.Create;

    LIdBoleto := IfThen(ATitulo.SeuNumero <> '',ATitulo.SeuNumero,ATitulo.NumeroDocumento);
    LTamNossoNumero := ATitulo.ACBrBoleto.Banco.CalcularTamMaximoNossoNumero(ATitulo.Carteira,
                                                                             ATitulo.NossoNumero,
                                                                             ATitulo.ACBrBoleto.Cedente.Convenio);

    LNossoNumero := (ATitulo.NossoNumero = '') or (ATitulo.NossoNumero = '0') or (ATitulo.NossoNumero = PadLeft(ATitulo.NossoNumero,LTamNossoNumero,'0'));

    if (LIdBoleto <> '') and (LNossoNumero) then
      LJsonObject.AddPair('idexterno', LIdBoleto)
    else
    begin
      LJsonObject.AddPair('idboleto', LIdBoleto);
      LJsonObject.AddPair('numeroTituloCliente', Boleto.Banco.MontarCampoNossoNumero(ATitulo));
    end;

    FPDadosMsg := LJsonObject.ToJSON;
  end;
end;

procedure TBoletoW_PenseBank_API.RequisicaoConsultaLista;
begin
// sem payload
end;

procedure TBoletoW_PenseBank_API.GerarPagador(AJson: TACBrJSONObject);
var
  LJsonObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonObject := TACBrJSONObject.Create;
    LJsonObject.AddPair('tipoInscricao', StrToInt(IfThen(Length( OnlyNumber(ATitulo.Sacado.CNPJCPF)) = 11,'1','2')));
    LJsonObject.AddPair('numeroInscricao', OnlyNumber(ATitulo.Sacado.CNPJCPF));
    LJsonObject.AddPair('nome', ATitulo.Sacado.NomeSacado);
    LJsonObject.AddPair('endereco', ATitulo.Sacado.Logradouro + ' ' + ATitulo.Sacado.Numero);
    LJsonObject.AddPair('cep', StrToInt(OnlyNumber(ATitulo.Sacado.CEP)));
    LJsonObject.AddPair('cidade', ATitulo.Sacado.Cidade);
    LJsonObject.AddPair('bairro', ATitulo.Sacado.Bairro);
    LJsonObject.AddPair('uf', ATitulo.Sacado.UF);
    LJsonObject.AddPair('telefone', ATitulo.Sacado.Fone);
    LJsonObject.AddPair('email', ATitulo.Sacado.Email);

    AJson.AddPair('pagador',LJsonObject);
  end;
end;

procedure TBoletoW_PenseBank_API.GerarBenificiarioFinal(AJson: TACBrJSONObject);
var
  LJsonObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJson)then
  begin
    if ATitulo.Sacado.SacadoAvalista.CNPJCPF = EmptyStr then
      Exit;

    LJsonObject := TACBrJSONObject.Create;

    LJsonObject.AddPair('tipoInscricao', StrToInt(IfThen( Length( OnlyNumber(ATitulo.Sacado.SacadoAvalista.CNPJCPF)) = 11,'1','2')));
    LJsonObject.AddPair('numeroInscricao', StrToInt64Def(OnlyNumber(ATitulo.Sacado.SacadoAvalista.CNPJCPF),0));
    LJsonObject.AddPair('nome', ATitulo.Sacado.SacadoAvalista.NomeAvalista);

    AJson.AddPair('beneficiarioFinal',LJsonObject);

  end;
end;

procedure TBoletoW_PenseBank_API.GerarJuros(AJson: TACBrJSONObject);
var
  LJsonObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonObject := TACBrJSONObject.Create;

    if (ATitulo.ValorMoraJuros > 0) then
    begin
      LJsonObject.AddPair('tipo', StrToIntDef(ATitulo.CodigoMora, 3));
      case (StrToIntDef(ATitulo.CodigoMora, 3)) of
        1 : LJsonObject.AddPair('valor', ATitulo.ValorMoraJuros);
        2 : LJsonObject.AddPair('porcentagem', ATitulo.ValorMoraJuros);
      end;

      AJson.AddPair('jurosMora',LJsonObject);

    end;
  end;
end;

procedure TBoletoW_PenseBank_API.GerarMulta(AJson: TACBrJSONObject);
var
  LJsonObject: TACBrJSONObject;
  LCodMulta: Integer;
begin
  if Assigned(ATitulo) and Assigned(AJson) then
  begin
    LJsonObject := TACBrJSONObject.Create;

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
      LJsonObject.AddPair('tipo', LCodMulta);

      if LCodMulta in [1, 2] then
        LJsonObject.AddPair('data', FormatDateBr(ATitulo.DataMulta, 'DD.MM.YYYY'));
      case LCodMulta of
        1 : LJsonObject.AddPair('valor', ATitulo.PercentualMulta);
        2 : LJsonObject.AddPair('porcentagem', ATitulo.PercentualMulta);
      end;

      AJson.AddPair('multa',LJsonObject);

    end;
  end;
end;

procedure TBoletoW_PenseBank_API.GerarDesconto(AJson: TACBrJSONObject);
var
  LJsonObject: TACBrJSONObject;
begin
  if Assigned(ATitulo) and Assigned(AJson)then
  begin
    LJsonObject := TACBrJSONObject.Create;
    if (ATitulo.DataDesconto > 0) then
    begin
      LJsonObject.AddPair('tipo', Integer(ATitulo.TipoDesconto));
      LJsonObject.AddPair('dataExpiracao', FormatDateBr(ATitulo.DataDesconto, 'DD.MM.YYYY'));

      case integer(ATitulo.TipoDesconto) of
        1 : LJsonObject.AddPair('valor', ATitulo.ValorDesconto);
        2 : LJsonObject.AddPair('porcentagem', ATitulo.ValorDesconto);
      end;

      AJson.AddPair('desconto',LJsonObject);

    end;
  end;
end;

procedure TBoletoW_PenseBank_API.AlteraDataVencimento(AJson: TACBrJSONObject);
begin
  if Assigned(ATitulo) and Assigned(AJson) and (ATitulo.Vencimento > 0) then
  begin
    AJson.AddPair('idboleto', ATitulo.NumeroDocumento);
    AJson.AddPair('dataVencimento', FormatDateBr(ATitulo.Vencimento, 'DD/MM/YYYY'));
    AJson.AddPair('numeroTituloCliente', Boleto.Banco.MontarCampoNossoNumero(ATitulo));
  end;

end;

procedure TBoletoW_PenseBank_API.AlteraPedidoNegativacao(AJson: TACBrJSONObject);
begin
  if Assigned(ATitulo) then
  begin
    if Assigned(AJson) then
    begin
      AJson.AddPair('idboleto', ATitulo.NumeroDocumento);
      AJson.AddPair('numeroTituloCliente', Boleto.Banco.MontarCampoNossoNumero(ATitulo));

      case ATitulo.OcorrenciaOriginal.Tipo of
        ToRemessaPedidoNegativacao: // Inclusão
          begin
            AJson.AddPair('quantidadeDiasNegativacao', ATitulo.DiasDeNegativacao);
            AJson.AddPair('tipoNegativacao', 1);
          end;
        ToRemessaNegativacaoSerasa: // Alteração
          begin
            AJson.AddPair('quantidadeDiasNegativacao', ATitulo.DiasDeNegativacao);
            AJson.Addpair('tipoNegativacao', 2);
          end;
        ToRemessaExcluirNegativacaoBaixar: // Cacelamento
          AJson.AddPair('tipoNegativacao', 3);
        ToRemessaExcluirNegativacaoSerasaBaixar: // Exclusão
          AJson.AddPair('tipoNegativacao', 4);
      end;
    end;
  end;
end;

procedure TBoletoW_PenseBank_API.AlterarProtesto(AJson: TACBrJSONObject);
begin
  if Assigned(ATitulo) and Assigned(AJson) and (ATitulo.DiasDeProtesto > 0) then
  begin
    AJson.AddPair('idboleto', ATitulo.NumeroDocumento);
    AJson.AddPair('numeroTituloCliente', Boleto.Banco.MontarCampoNossoNumero(ATitulo));
    AJson.AddPair('quantidadeDiasProtesto', ATitulo.DiasDeProtesto);
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
