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
    function LerXmlEspelho(const ANode: TACBrXmlNode): Boolean;
  end;

  { TNFSeR_SigISS103 }

  TNFSeR_SigISS103 = class(TNFSeR_SigISS)
  protected

  public

  end;

implementation

uses
  synautil,
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
  Dia, Mes, Ano, xUF: string;
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
        Discriminacao := StringReplace(Discriminacao, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);
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
          xMunicipio := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);

          if UF = '' then
            UF := xUF;
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
  FpQuebradeLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  LerParamsTabIni(True);

  Arquivo := NormatizarXml(Arquivo);

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if (Pos('Nota', Arquivo) > 0) then
    tpXML := txmlNFSe
  else
    if (Pos('EspelhoNfse', Arquivo) > 0) then
      tpXML := txmlEspelho
    else
      tpXML := txmlRPS;

  XmlNode := Document.Root;

  if XmlNode = nil then
    raise Exception.Create('Arquivo xml vazio.');

  if tpXML = txmlNFSe then
    Result := LerXmlNfse(XmlNode)
  else
    if tpXML = txmlEspelho then
      Result := LerXmlEspelho(XmlNode)
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

  if AuxNode = nil then
    AuxNode := ANode;

  with NFSe do
  begin
    SituacaoNfse := snNormal;
    dhRecebimento := Now;
    id_sis_legado := ObterConteudo(AuxNode.Childrens.FindAnyNs('id_sis_legado'), tcInt);
    CodigoVerificacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('autenticidade'), tcStr);
    Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('nota'), tcStr);

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('dt_conversao'), tcStr);
    aValor := Copy(aValor, 1, 11);
    DataEmissao := DecodeRfcDateTime(aValor);

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('emissao_rps'), tcStr);
    aValor := Copy(aValor, 1, 11);
    DataEmissaoRps := DecodeRfcDateTime(aValor);

    Link := ObterConteudo(AuxNode.Childrens.FindAnyNs('LinkImpressao'), tcStr);
    Link := StringReplace(Link, '&amp;', '&', [rfReplaceAll]);

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
      BaseCalculo      := ObterConteudo(AuxNode.Childrens.FindAnyNs('base'), tcDe2);
      Aliquota         := ObterConteudo(AuxNode.Childrens.FindAnyNs('aliquota_atividade'), tcDe2);
      ValorIss         := ObterConteudo(AuxNode.Childrens.FindAnyNs('iss'), tcDe2);
    end;

    with Servico do
    begin
      ItemListaServico := ObterConteudo(AuxNode.Childrens.FindAnyNs('servico'), tcStr);
      Discriminacao    := ObterConteudo(AuxNode.Childrens.FindAnyNs('descricao'), tcStr);
      Discriminacao := StringReplace(Discriminacao, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);

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

        RetencoesFederais := ValorPis + ValorCofins + ValorInss + ValorIr + ValorCsll;

        ValorLiquidoNfse := ValorServicos -
                            (RetencoesFederais + ValorDeducoes + ValorIssRetido +
                             DescontoCondicionado + DescontoIncondicionado);
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

function TNFSeR_SigISS.LerXmlEspelho(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode, IdentificacaoNfseNode, DadosNfseNode: TACBrXmlNode;
  aValor, xUF: string;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Nfse');

  if AuxNode = nil then Exit;

  IdentificacaoNfseNode := AuxNode.Childrens.FindAnyNs('IdentificacaoNfse');
  DadosNfseNode := AuxNode.Childrens.FindAnyNs('DadosNfse');

  with NFSe do
  begin
    dhRecebimento := ObterConteudo(IdentificacaoNfseNode.Childrens.FindAnyNs('DataEmissao'), tcDat);
    Numero := ObterConteudo(IdentificacaoNfseNode.Childrens.FindAnyNs('Numero'), tcStr);
    CodigoVerificacao := ObterConteudo(IdentificacaoNfseNode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
    Link := ObterConteudo(IdentificacaoNfseNode.Childrens.FindAnyNs('LinkImpressao'), tcStr);
    Link := StringReplace(Link, '&amp;', '&', [rfReplaceAll]);
    DataEmissao := ObterConteudo(IdentificacaoNfseNode.Childrens.FindAnyNs('DataEmissao'), tcDat);

    aValor := ObterConteudo(IdentificacaoNfseNode.Childrens.FindAnyNs('StatusNfse'), tcStr);

    if aValor = '1' then
      SituacaoNfse := snNormal
    else
      SituacaoNfse := snCancelado;

    with Prestador do
    begin
      RazaoSocial  := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorRazaoSocial'), tcStr);
      NomeFantasia := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorNomeFantasia'), tcStr);

      with IdentificacaoPrestador do
      begin
        InscricaoMunicipal := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorInscricaoMunicipal'), tcStr);
        CpfCnpj            := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorCnpj'), tcStr);
        InscricaoEstadual  := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorInscricaoEstadual'), tcStr);
      end;

      with Endereco do
      begin
        Endereco        := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorEndereco'), tcStr);
        Numero          := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorNumero'), tcStr);
        Complemento     := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorComplemento'), tcStr);
        Bairro          := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorBairro'), tcStr);
        CodigoMunicipio := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorCodigoMunicipio'), tcStr);
        UF              := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorUf'), tcStr);
        CEP             := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('PrestadorCep'), tcStr);
        xMunicipio      := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);

        if UF = '' then
          UF := xUF;
      end;
    end;

    with ValoresNfse do
    begin
      ValorLiquidoNfse := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('ValorLiquidoNfse'), tcDe2);
      BaseCalculo      := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('BaseCalculo'), tcDe2);
      Aliquota         := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('Aliquota'), tcDe2);
      ValorIss         := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('ValorIss'), tcDe2);
    end;

    with Servico do
    begin
      Discriminacao    := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('Discriminacao'), tcStr);
      Discriminacao := StringReplace(Discriminacao, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);

      with Valores do
      begin
        ValorServicos := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('ValorServicos'), tcDe2);
        BaseCalculo   := NFSe.ValoresNfse.BaseCalculo;
        Aliquota      := NFSe.ValoresNfse.Aliquota;
        ValorIss      := NFSe.ValoresNfse.ValorIss;

        ValorIr     := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('ValorIr'), tcDe2);
        ValorPis    := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('ValorPis'), tcDe2);
        ValorCofins := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('ValorCofins'), tcDe2);
        ValorCsll   := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('ValorCsll'), tcDe2);
        ValorInss   := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('ValorInss'), tcDe2);

        aValor := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('IssRetido'), tcStr);

        if aValor = 'false' then
          IssRetido := stNormal
        else
          IssRetido := stRetencao;

        ValorLiquidoNfse := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('ValorLiquidoNfse'), tcDe2);
      end;
    end;

    with Tomador do
    begin
      RazaoSocial  := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorRazaoSocial'), tcStr);
      NomeFantasia := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorRazaoSocial'), tcStr);

      with IdentificacaoTomador do
      begin
        CpfCnpj            := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorCpfCnpj'), tcStr);
        InscricaoEstadual  := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorInscricaoEstadual'), tcStr);
        InscricaoMunicipal := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorInscricaoMunicipal'), tcStr);
      end;

      with Endereco do
      begin
        Endereco        := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorEndereco'), tcStr);
        Numero          := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorNumero'), tcStr);
        Complemento     := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorComplemento'), tcStr);
        Bairro          := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorBairro'), tcStr);
        CEP             := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorCep'), tcStr);
        CodigoMunicipio := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorCodigoMunicipio'), tcStr);
        UF              := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('TomadorUf'), tcStr);
        xMunicipio      := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);

        if UF = '' then
          UF := xUF;
      end;
    end;

    OutrasInformacoes := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('Observacoes'), tcStr);
    OutrasInformacoes := StringReplace(OutrasInformacoes, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);

    aValor := ObterConteudo(DadosNfseNode.Childrens.FindAnyNs('OptanteSimplesNacional'), tcStr);

    if aValor = 'true' then
      OptanteSimplesNacional := snSim
    else
      OptanteSimplesNacional := snNao;
  end;
end;


end.
