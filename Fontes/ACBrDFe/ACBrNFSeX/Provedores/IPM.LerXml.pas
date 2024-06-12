{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

{$I ACBr.inc}

unit IPM.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml, ACBrNFSeXLerXml_ABRASFv2;

type
  { Provedor com layout próprio }
  { TNFSeR_IPM }

  TNFSeR_IPM = class(TNFSeRClass)
  private

  protected
    function RemoverGrupo_conteudohtml(const aXML: string): string;

    procedure LerRps(const ANode: TACBrXmlNode);
    procedure LerNota(const ANode: TACBrXmlNode);
    procedure LerPrestador(const ANode: TACBrXmlNode);
    procedure LerTomador(const ANode: TACBrXmlNode);
    procedure LerItens(const ANode: TACBrXmlNode);
    procedure LerFormaPagamento(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

  { TNFSeR_IPM101 }

  TNFSeR_IPM101 = class(TNFSeR_IPM)

  end;

  { TNFSeR_IPM204 }

  TNFSeR_IPM204 = class(TNFSeR_ABRASFv2)
  protected

  public
    function NormatizarXml(const aXml: string): string; override;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean; override;

  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     IPM
//==============================================================================

{ TNFSeR_IPM }

function TNFSeR_IPM.RemoverGrupo_conteudohtml(const aXML: string): string;
var
  i: Integer;
begin
  i := Pos('<codigo_html>', aXML);

  if i > 0 then
    Result := Copy(aXML, 1, i -1) + '</retorno>'
  else
    Result := aXML;
end;

procedure TNFSeR_IPM.LerFormaPagamento(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('forma_pagamento');

  if AuxNode <> nil then
    NFSe.CondicaoPagamento.Condicao := FpAOwner.StrToCondicaoPag(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tipo_pagamento'), tcStr));
end;

procedure TNFSeR_IPM.LerItens(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
  aValor: string;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('itens');

  if AuxNode <> nil then
  begin
    with NFSe.Servico do
    begin
      Valores.ValorIssRetido := 0;
      Valores.BaseCalculo := 0;
      Valores.ValorIss := 0;
      Valores.ValorInss := 0;
      Valores.Aliquota := 0;

      ANodes := AuxNode.Childrens.FindAllAnyNs('lista');

      if ANodes = nil then
        ANodes := AuxNode.Childrens.FindAllAnyNs('item');

      for i := 0 to Length(ANodes) - 1 do
      begin
        ItemServico.New;

        ItemServico[i].TribMunPrestador := FpAOwner.StrToSimNao(Ok, ObterConteudo(ANodes[i].Childrens.FindAnyNs('tributa_municipio_prestador'), tcStr));
        ItemServico[i].CodMunPrestacao := CodTOMToCodIBGE(ObterConteudo(ANodes[i].Childrens.FindAnyNs('codigo_local_prestacao_servico'), tcStr));

        aValor := ObterConteudo(ANodes[i].Childrens.FindAnyNs('codigo_item_lista_servico'), tcStr);
        ItemServico[i].ItemListaServico := PadLeft(aValor, 4, '0');
        ItemServico[i].ItemListaServico := NormatizarItemListaServico(ItemServico[i].ItemListaServico);

        ItemServico[i].xItemListaServico := ItemListaServicoDescricao(ItemServico[i].ItemListaServico);

        aValor := ObterConteudo(ANodes[i].Childrens.FindAnyNs('unidade_codigo'), tcStr);
        ItemServico[i].TipoUnidade := StrToUnidade(Ok, aValor);

        aValor := ObterConteudo(ANodes[i].Childrens.FindAnyNs('codigo_atividade'), tcStr);
        ItemServico[i].CodigoCnae := PadLeft(aValor, 9, '0');

        ItemServico[i].Quantidade := ObterConteudo(ANodes[i].Childrens.FindAnyNs('unidade_quantidade'), tcDe3);
        ItemServico[i].ValorUnitario := ObterConteudo(ANodes[i].Childrens.FindAnyNs('unidade_valor_unitario'), tcDe2);
        ItemServico[i].Descricao := ObterConteudo(ANodes[i].Childrens.FindAnyNs('descritivo'), tcStr);
        ItemServico[i].Descricao := StringReplace(ItemServico[i].Descricao, FpQuebradeLinha,
                                    sLineBreak, [rfReplaceAll, rfIgnoreCase]);
        ItemServico[i].Aliquota := ObterConteudo(ANodes[i].Childrens.FindAnyNs('aliquota_item_lista_servico'), tcDe2);

        ItemServico[i].SituacaoTributaria := ObterConteudo(ANodes[i].Childrens.FindAnyNs('situacao_tributaria'), tcInt);

        ItemServico[i].ValorTributavel := ObterConteudo(ANodes[i].Childrens.FindAnyNs('valor_tributavel'), tcDe2);
        ItemServico[i].ValorDeducoes := ObterConteudo(ANodes[i].Childrens.FindAnyNs('valor_deducao'), tcDe2);
        ItemServico[i].BaseCalculo := ObterConteudo(ANodes[i].Childrens.FindAnyNs('valor_tributavel'), tcDe2);
        ItemServico[i].ValorIssRetido := ObterConteudo(ANodes[i].Childrens.FindAnyNs('valor_issrf'), tcDe2);

        if ItemServico[i].ValorTotal = 0 then
          ItemServico[i].ValorTotal := ItemServico[i].Quantidade * ItemServico[i].ValorUnitario;

        ItemServico[i].ValorISS := ItemServico[i].BaseCalculo *
                                   ItemServico[i].Aliquota / 100;

        Valores.Aliquota := ItemServico[i].Aliquota;

        Valores.ValorIssRetido := Valores.ValorIssRetido + ItemServico[i].ValorIssRetido;

        Valores.BaseCalculo := Valores.BaseCalculo + ItemServico[i].BaseCalculo;
        Valores.ValorIss := Valores.ValorIss + ItemServico[i].ValorISS;

        Valores.ValorInss := Valores.ValorInss +
            ObterConteudo(ANodes[i].Childrens.FindAnyNs('valor_inss'), tcDe2);

        ItemServico[i].CodCNO := ObterConteudo(ANodes[i].Childrens.FindAnyNs('cno'), tcStr);
      end;
    end;
  end;
end;

procedure TNFSeR_IPM.LerNota(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  aValor, aValorD, aValorH: string;
  Ok: Boolean;
  i: Integer;
begin
  AuxNode := ANode.Childrens.FindAnyNs('nf');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('numero'), tcStr);
      CodigoVerificacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('codigo_autenticidade'), tcStr);

      if Trim(CodigoVerificacao) = '' then
        CodigoVerificacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('cod_verificador_autenticidade'), tcStr);

      Link := ObterConteudo(AuxNode.Childrens.FindAnyNs('link_nfse'), tcStr);
      Link := StringReplace(Link, '&amp;', '&', [rfReplaceAll]);

      Competencia := ObterConteudo(AuxNode.Childrens.FindAnyNs('data_fato'), tcDatVcto);

      // campos presentes ao baixar do site da prefeitura
      if Numero = '' then
      begin
        Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('numero_nfse'), tcStr);
        SeriePrestacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('serie_nfse'), tcStr);

        aValorD := ObterConteudo(AuxNode.Childrens.FindAnyNs('data_nfse'), tcStr);
        aValorH := ObterConteudo(AuxNode.Childrens.FindAnyNs('hora_nfse'), tcStr);

        i := Pos('-', aValorH);

        if i > 0 then
          aValorH := Copy(aValorH, 1, i-1);

        aValorD := aValorD + ' ' + aValorH;

        DataEmissao := EncodeDataHora(aValorD, 'DD/MM/YYYY hh:nn:ss');
      end;

      //XML cancelado não tem a tag "situacao_codigo_nfse" se baixado do site da prefeitura
      //somente a tag "situacao" = "C"
      aValor:=  ObterConteudo(AuxNode.Childrens.FindAnyNs('situacao'), tcStr);
      if aValor = 'C' then
        SituacaoNfse := snCancelado
      else
        SituacaoNfse := StrToStatusNFSe(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('situacao_codigo_nfse'), tcStr));

      OutrasInformacoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('observacao'), tcStr);
      OutrasInformacoes := StringReplace(OutrasInformacoes, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);

      Servico.Valores.ValorServicos := ObterConteudo(AuxNode.Childrens.FindAnyNs('valor_total'), tcDe2);
      Servico.Valores.ValorIr       := ObterConteudo(AuxNode.Childrens.FindAnyNs('valor_ir'), tcDe2);
      Servico.Valores.ValorInss     := ObterConteudo(AuxNode.Childrens.FindAnyNs('valor_inss'), tcDe2);
      Servico.Valores.ValorCsll     := ObterConteudo(AuxNode.Childrens.FindAnyNs('valor_contribuicao_social'), tcDe2);
      Servico.Valores.ValorPis      := ObterConteudo(AuxNode.Childrens.FindAnyNs('valor_pis'), tcDe2);
      Servico.Valores.ValorCofins   := ObterConteudo(AuxNode.Childrens.FindAnyNs('valor_cofins'), tcDe2);

      Servico.Valores.DescontoIncondicionado := ObterConteudo(AuxNode.Childrens.FindAnyNs('valor_desconto'), tcDe2);

      Servico.Valores.RetencoesFederais := Servico.Valores.ValorPis +
        Servico.Valores.ValorCofins + Servico.Valores.ValorInss +
        Servico.Valores.ValorIr + Servico.Valores.ValorCsll;

      Servico.Valores.ValorLiquidoNfse := Servico.Valores.ValorServicos -
        (Servico.Valores.RetencoesFederais + Servico.Valores.ValorDeducoes +
         Servico.Valores.DescontoCondicionado +
         Servico.Valores.DescontoIncondicionado + Servico.Valores.ValorIssRetido);

      Servico.Valores.ValorTotalNotaFiscal := Servico.Valores.ValorServicos -
        Servico.Valores.DescontoCondicionado -
        Servico.Valores.DescontoIncondicionado;
    end;
  end;
end;

procedure TNFSeR_IPM.LerPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  xUF: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('prestador');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      Prestador.IdentificacaoPrestador.CpfCnpj := OnlyNumber(ObterConteudo(AuxNode.Childrens.FindAnyNs('cpfcnpj'), tcStr));
      Prestador.IdentificacaoPrestador.CpfCnpj := PadLeft(Prestador.IdentificacaoPrestador.CpfCnpj, 14, '0');

      Prestador.Endereco.CodigoMunicipio := CodTOMToCodIBGE(ObterConteudo(AuxNode.Childrens.FindAnyNs('cidade'), tcStr));
      Prestador.Endereco.xMunicipio := ObterNomeMunicipioUF(StrToIntDef(Prestador.Endereco.CodigoMunicipio, 0), xUF);

      if Prestador.Endereco.UF = '' then
        Prestador.Endereco.UF := xUF;
    end;
  end;
end;

procedure TNFSeR_IPM.LerRps(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  aValorD, aValorH: string;
  i: Integer;
begin
  AuxNode := ANode.Childrens.FindAnyNs('rps');

  if AuxNode <> nil then
  begin
    aValorD := ObterConteudo(AuxNode.Childrens.FindAnyNs('data_emissao_recibo_provisorio'), tcStr);
    aValorH := ObterConteudo(AuxNode.Childrens.FindAnyNs('hora_emissao_recibo_provisorio'), tcStr);

    i := Pos('-', aValorH);

    if i > 0 then
      aValorH := Copy(aValorH, 1, i-1);

    aValorD := aValorD + ' ' + aValorH;

    NFSe.DataEmissao := EncodeDataHora(aValorD, 'DD/MM/YYYY hh:nn:ss');
    NFSe.DataEmissaoRps := NFSe.DataEmissao;

    with NFSe.IdentificacaoRps do
    begin
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('nro_recibo_provisorio'), tcStr);
      Serie  := ObterConteudo(AuxNode.Childrens.FindAnyNs('serie_recibo_provisorio'), tcStr);
    end;
  end;
end;

procedure TNFSeR_IPM.LerTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  xUF, aValor: string;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('tomador');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador do
    begin
      RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('nome_razao_social'), tcStr);
      NomeFantasia := ObterConteudo(AuxNode.Childrens.FindAnyNs('sobrenome_nome_fantasia'), tcStr);

      Endereco.EnderecoInformado := FpAOwner.StrToSimNaoOpc(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('endereco_informado'), tcStr));
      Endereco.Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('logradouro'), tcStr);
      Endereco.Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('numero_residencia'), tcStr);
      Endereco.Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('complemento'), tcStr);
      Endereco.Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('bairro'), tcStr);

      IdentificacaoTomador.CpfCnpj := OnlyNumber(ObterConteudo(AuxNode.Childrens.FindAnyNs('cpfcnpj'), tcStr));
      aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('tipo'), tcStr);

      if aValor = 'E' then
      begin
        IdentificacaoTomador.DocEstrangeiro := ObterConteudo(AuxNode.Childrens.FindAnyNs('identificador'), tcStr);
        Endereco.UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('estado'), tcStr);
        Endereco.xPais := ObterConteudo(AuxNode.Childrens.FindAnyNs('pais'), tcStr);
        Endereco.xMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('cidade'), tcStr);
      end
      else
      begin
        Endereco.CodigoMunicipio := CodTOMToCodIBGE(ObterConteudo(AuxNode.Childrens.FindAnyNs('cidade'), tcStr));
        Endereco.CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('cep'), tcStr);
        Endereco.xMunicipio := ObterNomeMunicipioUF(StrToIntDef(Endereco.CodigoMunicipio, 0), xUF);

        if Endereco.UF = '' then
          Endereco.UF := xUF;

        if ((aValor = 'J') or (aValor = '2')) then
        begin
          IdentificacaoTomador.CpfCnpj := PadLeft(IdentificacaoTomador.CpfCnpj, 14, '0');

          if Endereco.CodigoMunicipio = NFSe.Prestador.Endereco.CodigoMunicipio then
            IdentificacaoTomador.Tipo := tpPJdoMunicipio
          else
            IdentificacaoTomador.Tipo := tpPJforaMunicipio;
        end
        else
        begin
          IdentificacaoTomador.CpfCnpj := PadLeft(IdentificacaoTomador.CpfCnpj, 11, '0');
          IdentificacaoTomador.Tipo := tpPF;
        end;

        IdentificacaoTomador.InscricaoEstadual := ObterConteudo(AuxNode.Childrens.FindAnyNs('ie'), tcStr);
      end;

      aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('ddd_fone_comercial'), tcStr);
      aValor := aValor +
                ObterConteudo(AuxNode.Childrens.FindAnyNs('fone_comercial'), tcStr);

      Contato.Telefone := aValor;
      Contato.Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('email'), tcStr);
    end;
  end;
end;

function TNFSeR_IPM.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  FpQuebradeLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  LerParamsTabIni(True);

  Arquivo := RemoverGrupo_conteudohtml(Arquivo);

  Arquivo := NormatizarXml(Arquivo);

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if (Pos('nfse', Arquivo) > 0) then
    tpXML := txmlNFSe
  else
    tpXML := txmlRPS;

  XmlNode := Document.Root;

  if XmlNode = nil then
    raise Exception.Create('Arquivo xml vazio.');

  if tpXML = txmlNFSe then
    Result := LerXmlNfse(XmlNode)
  else
    Result := LerXmlRps(XmlNode);

  FreeAndNil(FDocument);
end;

function TNFSeR_IPM.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
begin
  Result := True;

  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('nfse');

  if AuxNode = nil then
    AuxNode := ANode;

  LerRps(AuxNode);
  LerItens(AuxNode);
  LerNota(AuxNode);
  LerPrestador(AuxNode);
  LerTomador(AuxNode);
  LerFormaPagamento(AuxNode);
end;

function TNFSeR_IPM.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
begin
  Result := LerXmlNfse(ANode);
end;

{ TNFSeR_IPM204 }

function TNFSeR_IPM204.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
begin
  Result := True;

  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('item');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.FindAnyNs('Nfse')
  else
    AuxNode := AuxNode.Childrens.FindAnyNs('Nfse');

  if AuxNode = nil then
    AuxNode := ANode;

  LerInfNfse(AuxNode);

  LerNfseCancelamento(ANode);
  LerNfseSubstituicao(ANode);

  LerCampoLink;
end;

function TNFSeR_IPM204.NormatizarXml(const aXml: string): string;
begin
  Result := inherited NormatizarXML(aXml);

  Result := Trim(StringReplace(Result, '&amp;#13;', sLineBreak, [rfReplaceAll]));
end;

end.
