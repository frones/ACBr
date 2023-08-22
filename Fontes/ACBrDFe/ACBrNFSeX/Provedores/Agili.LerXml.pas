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

unit Agili.LerXml;

interface

uses
  SysUtils, Classes, StrUtils, MaskUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { Provedor com layout próprio }
  { TNFSeR_Agili }

  TNFSeR_Agili = class(TNFSeRClass)
  protected
    FpCodCNAE: string;
    FpCodLCServ: string;

    procedure LerListaServico(const ANode: TACBrXmlNode);
    procedure LerResponsavelISSQN(const ANode: TACBrXmlNode);
    procedure LerRegimeEspecialTributacao(const ANode: TACBrXmlNode);
    procedure LerExigibilidadeISSQN(const ANode: TACBrXmlNode);
    procedure LerMunicipioIncidencia(const ANode: TACBrXmlNode);
    procedure LerRpsDeclaracaoPrestacaoServico(const ANode: TACBrXmlNode);
    procedure LerNfseDeclaracaoPrestacaoServico(const ANode: TACBrXmlNode);
    procedure LerDeclaracaoPrestacaoServico(const ANode: TACBrXmlNode);
    procedure LerRps(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoRps(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoPrestador(const ANode: TACBrXmlNode);
    procedure LerDadosTomador(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoTomador(const ANode: TACBrXmlNode);
    procedure LerEnderecoTomador(const ANode: TACBrXmlNode);
    procedure LerEnderecoPrestador(const ANode: TACBrXmlNode);
    procedure LerContatoTomador(const ANode: TACBrXmlNode);
    procedure LerContatoPrestador(const ANode: TACBrXmlNode);
    procedure LerCpfCnpjTomador(const ANode: TACBrXmlNode);
    procedure LerCnpjPrestador(const ANode: TACBrXmlNode);
    procedure LerSituacaoNfse(const ANode: TACBrXmlNode);
    procedure LerDadosPrestador(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoOrgaoGerador(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     Agili
//==============================================================================

{ TNFSeR_Agili }

procedure TNFSeR_Agili.LerContatoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Contato');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Contato do
    begin
      Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('Telefone'), tcStr);
      Email    := ObterConteudo(AuxNode.Childrens.FindAnyNs('Email'), tcStr);
    end;
  end;
end;

procedure TNFSeR_Agili.LerContatoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Contato');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Contato do
    begin
      Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('Telefone'), tcStr);
      Email    := ObterConteudo(AuxNode.Childrens.FindAnyNs('Email'), tcStr);
    end;
  end;
end;

procedure TNFSeR_Agili.LerCnpjPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('CpfCnpj');

  if AuxNode <> nil then
  begin
    NFSe.Prestador.IdentificacaoPrestador.CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cnpj'), tcStr);

    if NFSe.Prestador.IdentificacaoPrestador.CpfCnpj = '' then
      NFSe.Prestador.IdentificacaoPrestador.CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cpf'), tcStr);
  end;
end;

procedure TNFSeR_Agili.LerCpfCnpjTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('CpfCnpj');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.IdentificacaoTomador do
    begin
      CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cpf'), tcStr);

      if CpfCnpj = '' then
        CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cnpj'), tcStr);
    end;
  end;
end;

procedure TNFSeR_Agili.LerDadosPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('DadosPrestador');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador do
    begin
      RazaoSocial  := ObterConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);
      NomeFantasia := ObterConteudo(AuxNode.Childrens.FindAnyNs('NomeFantasia'), tcStr);
    end;

    LerEnderecoPrestador(AuxNode);
    LerContatoPrestador(AuxNode);
  end;
end;

procedure TNFSeR_Agili.LerDadosTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('DadosTomador');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador do
    begin
      RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);

      IdentificacaoTomador.InscricaoEstadual := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoEstadual'), tcStr);
    end;

    LerIdentificacaoTomador(AuxNode);
    LerEnderecoTomador(AuxNode);
    LerContatoTomador(AuxNode);
  end;
end;

procedure TNFSeR_Agili.LerEnderecoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode, AuxMun, AuxPais: TACBrXmlNode;
  xUF: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Endereco');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Endereco do
    begin
      TipoLogradouro := ObterConteudo(AuxNode.Childrens.FindAnyNs('TipoLogradouro'), tcStr);
      Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('Logradouro'), tcStr);
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('Complemento'), tcStr);
      Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);

      AuxMun := AuxNode.Childrens.FindAnyNs('Municipio');

      if AuxMun <> nil then
      begin
        CodigoMunicipio := ObterConteudo(AuxMun.Childrens.FindAnyNs('CodigoMunicipioIBGE'), tcStr);

        if length(CodigoMunicipio) < 7 then
          CodigoMunicipio := Copy(CodigoMunicipio, 1, 2) +
              FormatFloat('00000', StrToIntDef(Copy(CodigoMunicipio, 3, 5), 0));

        UF := ObterConteudo(AuxMun.Childrens.FindAnyNs('Uf'), tcStr);

        xMunicipio := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);

        if UF = '' then
          UF := xUF;
      end;

      AuxPais := AuxNode.Childrens.FindAnyNs('Pais');

      if AuxPais <> nil then
      begin
        CodigoPais := ObterConteudo(AuxPais.Childrens.FindAnyNs('CodigoPaisBacen'), tcStr);
      end;

      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cep'), tcStr);
    end;
  end;
end;

procedure TNFSeR_Agili.LerEnderecoTomador(const ANode: TACBrXmlNode);
var
  AuxNode, AuxMun, AuxPais: TACBrXmlNode;
  xUF: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Endereco');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      TipoLogradouro := ObterConteudo(AuxNode.Childrens.FindAnyNs('TipoLogradouro'), tcStr);
      Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('Logradouro'), tcStr);
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('Complemento'), tcStr);
      Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);

      AuxMun := AuxNode.Childrens.FindAnyNs('Municipio');

      if AuxMun <> nil then
      begin
        CodigoMunicipio := ObterConteudo(AuxMun.Childrens.FindAnyNs('CodigoMunicipioIBGE'), tcStr);

        if length(CodigoMunicipio) < 7 then
          CodigoMunicipio := Copy(CodigoMunicipio, 1, 2) +
              FormatFloat('00000', StrToIntDef(Copy(CodigoMunicipio, 3, 5), 0));

        UF := ObterConteudo(AuxMun.Childrens.FindAnyNs('Uf'), tcStr);

        xMunicipio := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);

        if UF = '' then
          UF := xUF;
      end;

      AuxPais := AuxNode.Childrens.FindAnyNs('Pais');

      if AuxPais <> nil then
      begin
        CodigoPais := ObterConteudo(AuxPais.Childrens.FindAnyNs('CodigoPaisBacen'), tcStr);
      end;

      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cep'), tcStr);
    end;
  end;
end;

procedure TNFSeR_Agili.LerExigibilidadeISSQN(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('ExigibilidadeISSQN');

  if AuxNode <> nil then
  begin
    with NFSe.Servico do
    begin
      ExigibilidadeISS := FpAOwner.StrToExigibilidadeISS(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('Codigo'), tcStr));
    end;
  end;
end;

procedure TNFSeR_Agili.LerIdentificacaoPrestador(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoPrestador');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.IdentificacaoPrestador do
    begin
      // A tag ChaveDigital não é lida do XML pois é para constar na propriedade
      // de configuração ChaveAcesso
      LerCnpjPrestador(AuxNode);
      InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
    end;
  end;
end;

procedure TNFSeR_Agili.LerIdentificacaoRps(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoRps');

  if AuxNode <> nil then
  begin
    with NFSe.IdentificacaoRps do
    begin
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Serie  := ObterConteudo(AuxNode.Childrens.FindAnyNs('Serie'), tcStr);

      Tipo := FpAOwner.StrToTipoRPS(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('Tipo'), tcStr));

      NFSe.InfID.ID := OnlyNumber(Numero) + Serie;
    end;
  end;
end;

procedure TNFSeR_Agili.LerIdentificacaoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoTomador');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.IdentificacaoTomador do
    begin
      InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);

      LerCpfCnpjTomador(AuxNode);
    end;
  end;
end;

procedure TNFSeR_Agili.LerIdentificacaoOrgaoGerador(
  const ANode: TACBrXmlNode);
var
  AuxNode, AuxMun: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoOrgaoGerador');

  if AuxNode <> nil then
  begin
    AuxMun := AuxNode.Childrens.FindAnyNs('Municipio');

    if AuxMun <> nil then
    begin
      with NFSe.OrgaoGerador do
      begin
        CodigoMunicipio := ObterConteudo(AuxMun.Childrens.FindAnyNs('CodigoMunicipioIBGE'), tcStr);
        UF := ObterConteudo(AuxMun.Childrens.FindAnyNs('Uf'), tcStr);
      end;
    end;
  end;
end;

procedure TNFSeR_Agili.LerDeclaracaoPrestacaoServico(
  const ANode: TACBrXmlNode);
var
  aValor: string;
  Ok: Boolean;
begin
  LerIdentificacaoPrestador(ANode);
  LerRps(ANode);
  LerDadosTomador(ANode);
  LerRegimeEspecialTributacao(ANode);

  NFSe.OptanteSimplesNacional := FpAOwner.StrToSimNao(Ok, ObterConteudo(ANode.Childrens.FindAnyNs('OptanteSimplesNacional'), tcStr));
  NFSe.NfseSubstituida := ObterConteudo(ANode.Childrens.FindAnyNs('NfseSubstituida'), tcStr);
  NFSe.OptanteMEISimei := FpAOwner.StrToSimNao(Ok, ObterConteudo(ANode.Childrens.FindAnyNs('OptanteMEISimei'), tcStr));

  LerResponsavelISSQN(ANode);
  LerExigibilidadeISSQN(ANode);
  LerListaServico(ANode);

  with NFSe.Servico do
  begin
    CodigoTributacaoMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('CodigoAtividadeEconomica'), tcStr);

    if CodigoTributacaoMunicipio = '' then
      CodigoTributacaoMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('CodigoCnaeAtividadeEconomica'), tcStr);

    if CodigoTributacaoMunicipio = '' then
      CodigoTributacaoMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('ItemLei116AtividadeEconomica'), tcStr);

    CodigoCnae := FpCodCNAE;
    ItemListaServico := FpCodLCServ;

    xItemListaServico := ItemListaServicoDescricao(ItemListaServico);

    NumeroProcesso := ObterConteudo(ANode.Childrens.FindAnyNs('BeneficioProcesso'), tcStr);

    with Valores do
    begin
      ValorServicos := ObterConteudo(ANode.Childrens.FindAnyNs('ValorServicos'), tcDe2);
      DescontoIncondicionado := ObterConteudo(ANode.Childrens.FindAnyNs('ValorDescontos'), tcDe2);
      ValorPis := ObterConteudo(ANode.Childrens.FindAnyNs('ValorPis'), tcDe2);
      ValorCofins := ObterConteudo(ANode.Childrens.FindAnyNs('ValorCofins'), tcDe2);
      ValorInss := ObterConteudo(ANode.Childrens.FindAnyNs('ValorInss'), tcDe2);
      ValorIr := ObterConteudo(ANode.Childrens.FindAnyNs('ValorIrrf'), tcDe2);
      ValorCsll := ObterConteudo(ANode.Childrens.FindAnyNs('ValorCsll'), tcDe2);
      valorOutrasRetencoes := ObterConteudo(ANode.Childrens.FindAnyNs('ValorOutrasRetencoes'), tcDe2);
      BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('ValorBaseCalculoISSQN'), tcDe2);
      Aliquota := ObterConteudo(ANode.Childrens.FindAnyNs('AliquotaISSQN'), tcDe3);
      ValorIss := ObterConteudo(ANode.Childrens.FindAnyNs('ValorISSQNCalculado'), tcDe2);

      aValor := ObterConteudo(ANode.Childrens.FindAnyNs('ISSQNRetido'), tcStr);

      case FpAOwner.StrToSimNao(Ok, aValor) of
        snSim:
          begin
            ValorIssRetido := ValorIss;
            IssRetido := stRetencao;
          end;
        snNao:
          begin
            ValorIssRetido := 0;
            IssRetido := stNormal;
          end;
      end;

      ValorLiquidoNfse := ObterConteudo(ANode.Childrens.FindAnyNs('ValorLiquido'), tcDe2);
    end;

    NFSe.OutrasInformacoes := ObterConteudo(ANode.Childrens.FindAnyNs('Observacao'), tcStr);
    NFSe.OutrasInformacoes := StringReplace(NFSe.OutrasInformacoes, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);
  end;

  LerMunicipioIncidencia(ANode);
end;

procedure TNFSeR_Agili.LerListaServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i, Item: Integer;
begin
  AuxNode := ANode.Childrens.FindAnyNs('ListaServico');

  if AuxNode <> nil then
  begin
    ANodes := AuxNode.Childrens.FindAllAnyNs('DadosServico');

    for i := 0 to Length(ANodes) - 1 do
    begin
      NFSe.Servico.ItemServico.New;
      with NFSe.Servico.ItemServico[i] do
      begin
        Descricao  := ObterConteudo(ANodes[i].Childrens.FindAnyNs('Discriminacao'), tcStr);
        Descricao := StringReplace(Descricao, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);
        Quantidade := ObterConteudo(ANodes[i].Childrens.FindAnyNs('Quantidade'), tcDe6);
        ValorUnitario := ObterConteudo(ANodes[i].Childrens.FindAnyNs('ValorServico'), tcDe2);
        ValorTotal := ValorUnitario * Quantidade;

        DescontoIncondicionado := ObterConteudo(ANodes[i].Childrens.FindAnyNs('ValorDesconto'), tcDe2);

        FpCodCNAE := ObterConteudo(ANodes[i].Childrens.FindAnyNs('CodigoCnae'), tcStr);
        FpCodLCServ := ObterConteudo(ANodes[i].Childrens.FindAnyNs('ItemLei116'), tcStr);

        Item := StrToIntDef(OnlyNumber(FpCodLCServ), 0);
        if Item < 100 then
          Item := Item * 100 + 1;

        FpCodLCServ := FormatFloat('0000', Item);
        FpCodLCServ := Copy(FpCodLCServ, 1, 2) + '.' + Copy(FpCodLCServ, 3, 2);
      end;
    end;
  end;
end;

procedure TNFSeR_Agili.LerMunicipioIncidencia(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('MunicipioIncidencia');

  if AuxNode <> nil then
  begin
    with NFSe.Servico do
    begin
      CodigoMunicipio     := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipioIBGE'), tcStr);
      CodigoPais          := 1058;
      MunicipioIncidencia := StrToIntDef(CodigoMunicipio, 0);
    end;
  end;
end;

procedure TNFSeR_Agili.LerNfseDeclaracaoPrestacaoServico(const ANode: TACBrXmlNode);
var
  AuxNode:TACBrXmlNode;
begin

  AuxNode := ANode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');

  if Assigned(AuxNode) then
    LerDeclaracaoPrestacaoServico(AuxNode);
end;

procedure TNFSeR_Agili.LerRegimeEspecialTributacao(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('RegimeEspecialTributacao');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      RegimeEspecialTributacao := FpAOwner.StrToRegimeEspecialTributacao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('Codigo'), tcStr));
    end;
  end;
end;

procedure TNFSeR_Agili.LerResponsavelISSQN(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('ResponsavelISSQN');

  if AuxNode <> nil then
  begin
    with NFSe.Servico do
    begin
      ResponsavelRetencao := FpAOwner.StrToResponsavelRetencao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('Codigo'), tcStr));
    end;
  end;
end;

procedure TNFSeR_Agili.LerRps(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Rps');

  if AuxNode <> nil then
  begin
    NFSe.DataEmissaoRps := ObterConteudo(AuxNode.Childrens.FindAnyNs('DataEmissao'), tcDat);

    LerIdentificacaoRps(AuxNode);
  end;
end;

procedure TNFSeR_Agili.LerRpsDeclaracaoPrestacaoServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin

  AuxNode := ANode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');

  if Assigned(AuxNode) then
  begin
    LerDeclaracaoPrestacaoServico(AuxNode);
    Exit;
  end;

  AuxNode := ANode.Childrens.FindAnyNs('InfDeclaracaoPrestacaoServico');

  if Assigned(AuxNode) then
  begin
    LerDeclaracaoPrestacaoServico(AuxNode);
    Exit;
  end;
end;

procedure TNFSeR_Agili.LerSituacaoNfse(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('SituacaoNfse');

  if AuxNode <> nil then
  begin
    NFSe.Situacao := StrToIntDef(ObterConteudo(AuxNode.Childrens.FindAnyNs('Codigo'), tcStr), 0);

    case NFSe.Situacao of
      -2:
        begin
          NFSe.SituacaoNfse := snCancelado;
          NFSe.MotivoCancelamento := ObterConteudo(AuxNode.Childrens.FindAnyNs('MotivoCancelamento'), tcStr);
        end;
      -8: NFSe.SituacaoNfse := snNormal;
    end;
  end;
end;

function TNFSeR_Agili.LerXml: Boolean;
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

  if (Pos('Nfse', Arquivo) > 0) then
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

function TNFSeR_Agili.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
begin
  Result := True;

  FpCodCNAE := '';
  FpcodLCServ := '';

  if not Assigned(ANode) or (ANode = nil) then Exit;

  NFSe.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('Numero'), tcStr);
  NFSe.CodigoVerificacao := ObterConteudo(ANode.Childrens.FindAnyNs('CodigoAutenticidade'), tcStr);
  NFSe.DataEmissao := ObterConteudo(ANode.Childrens.FindAnyNs('DataEmissao'), tcDatHor);

  LerSituacaoNfse(ANode);
  LerIdentificacaoOrgaoGerador(ANode);
  LerDadosPrestador(ANode);
  LerNfseDeclaracaoPrestacaoServico(ANode);

  LerCampoLink;
end;

function TNFSeR_Agili.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  LerRpsDeclaracaoPrestacaoServico(ANode);
end;

end.
