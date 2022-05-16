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

unit SigISS.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { TNFSeR_SigISS }

  TNFSeR_SigISS = class(TNFSeRClass)
  protected
    procedure LerDadosPrestador(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoRps(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

  { TNFSeR_SigISS103 }

  TNFSeR_SigISS103 = class(TNFSeR_SigISS)
  protected

  public

  end;

implementation

uses
  ACBrUtil.Base;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     SigISS
//==============================================================================

{ TNFSeR_SigISS }

procedure TNFSeR_SigISS.LerDadosPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('DadosPrestador');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      with Prestador do
      begin
        crc := ObterConteudo(AuxNode.Childrens.FindAnyNs('crc'), tcStr);
        crc_estado := ObterConteudo(AuxNode.Childrens.FindAnyNs('crc_estado'), tcStr);

        with IdentificacaoPrestador do
        begin
          CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('cnpj'), tcStr);
        end;
      end;

      with Servico.Valores do
      begin
        AliquotaSN := ObterConteudo(AuxNode.Childrens.FindAnyNs('aliquota_simples'), tcDe2);
      end;
    end;
  end;
end;

procedure TNFSeR_SigISS.LerIdentificacaoRps(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Dia, Mes, Ano: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('DescricaoRps');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      with Prestador do
      begin
        crc := ObterConteudo(AuxNode.Childrens.FindAnyNs('crc'), tcStr);
        crc_estado := ObterConteudo(AuxNode.Childrens.FindAnyNs('crc_estado'), tcStr);

        with IdentificacaoPrestador do
        begin
          CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('cnpj'), tcStr);
        end;
      end;

      with Servico.Valores do
      begin
        AliquotaSN := ObterConteudo(AuxNode.Childrens.FindAnyNs('aliquota_simples'), tcDe2);
      end;

      id_sis_legado := ObterConteudo(AuxNode.Childrens.FindAnyNs('id_sis_legado'), tcStr);
      SituacaoTrib := ObterConteudo(AuxNode.Childrens.FindAnyNs('situacao'), tcStr);

      with Servico do
      begin
        Discriminacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('descricaoNF'), tcStr);
        CodigoTributacaoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('servico'), tcStr);
        MunicipioIncidencia := ObterConteudo(AuxNode.Childrens.FindAnyNs('codigo_cidade_local_servico'), tcInt);

        with Valores do
        begin
          ValorServicos := ObterConteudo(AuxNode.Childrens.FindAnyNs('valor'), tcDe4);
          BaseCalculo := ObterConteudo(AuxNode.Childrens.FindAnyNs('base'), tcDe4);
          ValorInss := ObterConteudo(AuxNode.Childrens.FindAnyNs('valor_inss'), tcDe4);
          ValorIr := ObterConteudo(AuxNode.Childrens.FindAnyNs('valor_ir'), tcDe4);
          ValorPis := ObterConteudo(AuxNode.Childrens.FindAnyNs('valor_pis'), tcDe4);
          ValorCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('valor_cofins'), tcDe4);
          ValorCsll := ObterConteudo(AuxNode.Childrens.FindAnyNs('valor_csll'), tcDe4);
        end;
      end;

      with Tomador do
      begin
        with IdentificacaoTomador do
        begin
          Tipo := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_tipo'), tcStr);
          CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_cnpj'), tcStr);
          InscricaoEstadual := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_ie'), tcStr);
          InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_im'), tcStr);
        end;

        with Contato do
        begin
          Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_email'), tcStr);
          Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_fone'), tcStr);
        end;

        RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_razao'), tcStr);
        NomeFantasia := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_fantasia'), tcStr);

        with Endereco do
        begin
          Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_endereco'), tcStr);
          Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_numero'), tcStr);
          Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_complemento'), tcStr);
          Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_bairro'), tcStr);
          CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_CEP'), tcStr);
          CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('tomador_cod_cidade'), tcStr);
        end;

        with IdentificacaoRps do
        begin
          Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('rps_num'), tcStr);
          Serie := ObterConteudo(AuxNode.Childrens.FindAnyNs('rps_serie'), tcStr);
        end;
      end;

      Dia := ObterConteudo(AuxNode.Childrens.FindAnyNs('rps_dia'), tcStr);
      Mes := ObterConteudo(AuxNode.Childrens.FindAnyNs('rps_mes'), tcStr);
      Ano := ObterConteudo(AuxNode.Childrens.FindAnyNs('rps_ano'), tcStr);

      DataEmissaoRps := StrToDateDef(Dia + '/' + Mes + '/' + Ano, Date);
    end;
  end;
end;

function TNFSeR_SigISS.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  Arquivo := NormatizarXml(Arquivo);

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if (Pos('Nota', Arquivo) > 0) then
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

function TNFSeR_SigISS.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  aValor: string;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('DadosNota');

  if AuxNode = nil then Exit;

  with NFSe do
  begin
    SituacaoNfse := snNormal;
    dhRecebimento := Now;
    id_sis_legado := ObterConteudo(AuxNode.Childrens.FindAnyNs('id_sis_legado'), tcInt);
    CodigoVerificacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('autenticidade'), tcStr);
    Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('nota'), tcStr);
    DataEmissao := ObterConteudo(AuxNode.Childrens.FindAnyNs('dt_conversao'), tcDat);
    DataEmissaoRps := ObterConteudo(AuxNode.Childrens.FindAnyNs('emissao_rps'), tcDat);
    Link := ObterConteudo(AuxNode.Childrens.FindAnyNs('LinkImpressao'), tcStr);

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('StatusNFe'), tcStr);

    if aValor = 'Cancelada' then
      SituacaoNfse := snCancelado;

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('OpcaoSimples'), tcStr);

    if aValor = 'NAO' then
      OptanteSimplesNacional := snNao
    else
      OptanteSimplesNacional := snSim;

    with ValoresNfse do
    begin
      ValorLiquidoNfse := ObterConteudo(AuxNode.Childrens.FindAnyNs('valor'), tcDe2);
      BaseCalculo      := ValorLiquidoNfse;
      Aliquota         := ObterConteudo(AuxNode.Childrens.FindAnyNs('aliquota_atividade'), tcDe2);
      ValorIss         := ObterConteudo(AuxNode.Childrens.FindAnyNs('iss'), tcDe2);
    end;

    with Servico do
    begin
      ItemListaServico := ObterConteudo(AuxNode.Childrens.FindAnyNs('servico'), tcStr);
      Discriminacao    := ObterConteudo(AuxNode.Childrens.FindAnyNs('descricao'), tcStr);

      with Valores do
      begin
        ValorServicos := NFSe.ValoresNfse.ValorLiquidoNfse;
        BaseCalculo   := ValorServicos;
        Aliquota      := NFSe.ValoresNfse.Aliquota;
        ValorIss      := NFSe.ValoresNfse.ValorIss;

        aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('ISSRetido'), tcStr);

        if aValor = 'NAO' then
           IssRetido := stNormal
        else
        begin
          IssRetido := stRetencao;
          ValorIssRetido := StrToFloatDef(ObterConteudo(AuxNode.Childrens.FindAnyNs('ISSRetido'), tcStr), 0);
        end;

        ValorLiquidoNfse := ValorServicos -
                            (ValorPis + ValorCofins + ValorInss + ValorIr +
                             ValorCsll + ValorDeducoes + DescontoCondicionado +
                             DescontoIncondicionado + ValorIssRetido);
      end;
    end;

    with Prestador do
    begin
      RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('prestador_razao'), tcStr);

      with Endereco do
      begin
        Endereco    := ObterConteudo(AuxNode.Childrens.FindAnyNs('prestador_endereco'), tcStr);
        Numero      := ObterConteudo(AuxNode.Childrens.FindAnyNs('prestador_numero'), tcStr);
        Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('prestador_complemento'), tcStr);
        Bairro      := ObterConteudo(AuxNode.Childrens.FindAnyNs('prestador_bairro'), tcStr);
        xMunicipio  := ObterConteudo(AuxNode.Childrens.FindAnyNs('prestador_cidade'), tcStr);
        UF          := ObterConteudo(AuxNode.Childrens.FindAnyNs('prestador_estado'), tcStr);
        CEP         := ObterConteudo(AuxNode.Childrens.FindAnyNs('prestador_cep'), tcStr);

        Contato.Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('prestador_email'), tcStr);
      end;
    end;

    with Tomador do
    begin
      RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('razao_tomador'), tcStr);

      with IdentificacaoTomador do
        CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('cnpj_tomador'), tcStr);

      with Endereco do
      begin
        Endereco    := ObterConteudo(AuxNode.Childrens.FindAnyNs('endereco_tomador'), tcStr);
        Numero      := ObterConteudo(AuxNode.Childrens.FindAnyNs('numero_tomador'), tcStr);
        Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('complemento_tomador'), tcStr);
        Bairro      := ObterConteudo(AuxNode.Childrens.FindAnyNs('bairro_tomador'), tcStr);
        xMunicipio  := ObterConteudo(AuxNode.Childrens.FindAnyNs('cidade_tomador'), tcStr);
        UF          := ObterConteudo(AuxNode.Childrens.FindAnyNs('estado_tomador'), tcStr);
        CEP         := ObterConteudo(AuxNode.Childrens.FindAnyNs('cep_tomador'), tcStr);

        Contato.Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('email_tomador'), tcStr);
      end;
    end;
  end;
end;

function TNFSeR_SigISS.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
begin
  Result := True;

  with NFSe do
  begin
    LerDadosPrestador(ANode);
    LerIdentificacaoRps(ANode);
  end;
end;

end.
