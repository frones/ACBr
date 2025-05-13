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
  SysUtils, Classes, StrUtils, MaskUtils, IniFiles,
  ACBrNFSeXClass,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { Provedor com layout próprio }
  { TNFSeR_Agili }

  TNFSeR_Agili = class(TNFSeRClass)
  protected
    FpCodCNAE: string;
    FpCodLCServ: string;
    FpTipoXML: string;

    // Ler o arquivo XML
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
    procedure LerIdentificacaoParceiro(const ANode: TACBrXmlNode; Indice: Integer);

    //====== Ler o Arquivo INI===========================================
    procedure LerINIIdentificacaoNFSe(AINIRec: TMemIniFile);
    procedure LerINIIdentificacaoPrestador(AINIRec: TMemIniFile);
    procedure LerINIIdentificacaoRps(AINIRec: TMemIniFile);
    procedure LerINIDadosTomador(AINIRec: TMemIniFile);
    procedure LerINIDadosIntermediario(AINIRec: TMemIniFile);
    procedure LerINIConstrucaoCivil(AINIRec: TMemIniFile);
    procedure LerINIDadosServico(AINIRec: TMemIniFile);
    procedure LerINIDadosValores(AINIRec: TMemIniFile);
    procedure LerINIListaServico(AINIRec: TMemIniFile);
    procedure LerINIDadosProfissionalParceiro(AINIRec: TMemIniFile;
      DadosParceiro: TDadosProfissionalParceiro; Indice: Integer);

    procedure LerIniRps(AINIRec: TMemIniFile);
    procedure LerIniNfse(AINIRec: TMemIniFile);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;

    function LerIni: Boolean; override;
  end;

implementation

uses
  ACBrUtil.Base,
  ACBrUtil.XMLHTML,
  ACBrUtil.DateTime,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrDFeUtil;

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
        CodigoPais := ObterConteudo(AuxPais.Childrens.FindAnyNs('CodigoPaisBacen'), tcInt);
        xPais := ObterConteudo(AuxPais.Childrens.FindAnyNs('Descricao'), tcStr);
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

procedure TNFSeR_Agili.LerIdentificacaoParceiro(const ANode: TACBrXmlNode;
  Indice: Integer);
var
  AuxNode, AuxNodeCpfCnpj: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoProfissionalParceiro');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.ItemServico[indice].DadosProfissionalParceiro.IdentificacaoParceiro do
    begin
      CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cnpj'), tcStr);

      if CpfCnpj = '' then
      begin
        AuxNodeCpfCnpj := AuxNode.Childrens.FindAnyNs('CpfCnpj');

        if AuxNodeCpfCnpj <> nil then
        begin
          CpfCnpj := ObterConteudo(AuxNodeCpfCnpj.Childrens.FindAnyNs('Cpf'), tcStr);

          if CpfCnpj = '' then
            CpfCnpj := ObterConteudo(AuxNodeCpfCnpj.Childrens.FindAnyNs('Cnpj'), tcStr);
        end;
      end;

      if Length(CpfCnpj) > 11 then
        CpfCnpj := Poem_Zeros(CpfCnpj, 14);

      InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
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

    Valores.ValorServicos := ObterConteudo(ANode.Childrens.FindAnyNs('ValorServicos'), tcDe2);
    Valores.DescontoIncondicionado := ObterConteudo(ANode.Childrens.FindAnyNs('ValorDescontos'), tcDe2);
    Valores.ValorPis := ObterConteudo(ANode.Childrens.FindAnyNs('ValorPis'), tcDe2);
    Valores.ValorCofins := ObterConteudo(ANode.Childrens.FindAnyNs('ValorCofins'), tcDe2);
    Valores.ValorInss := ObterConteudo(ANode.Childrens.FindAnyNs('ValorInss'), tcDe2);
    Valores.ValorIr := ObterConteudo(ANode.Childrens.FindAnyNs('ValorIrrf'), tcDe2);
    Valores.ValorCsll := ObterConteudo(ANode.Childrens.FindAnyNs('ValorCsll'), tcDe2);
    Valores.valorOutrasRetencoes := ObterConteudo(ANode.Childrens.FindAnyNs('ValorOutrasRetencoes'), tcDe2);
    Valores.BaseCalculo := ObterConteudo(ANode.Childrens.FindAnyNs('ValorBaseCalculoISSQN'), tcDe2);
    Valores.Aliquota := ObterConteudo(ANode.Childrens.FindAnyNs('AliquotaISSQN'), tcDe3);
    Valores.ValorIss := ObterConteudo(ANode.Childrens.FindAnyNs('ValorISSQNCalculado'), tcDe2);

    Valores.RetencoesFederais := Valores.ValorPis + Valores.ValorCofins +
      Valores.ValorInss + Valores.ValorIr + Valores.ValorCsll;

    aValor := ObterConteudo(ANode.Childrens.FindAnyNs('ISSQNRetido'), tcStr);

    case FpAOwner.StrToSimNao(Ok, aValor) of
      snSim:
        begin
          Valores.ValorIssRetido := Valores.ValorIss;
          Valores.IssRetido := stRetencao;
        end;
      snNao:
        begin
          Valores.ValorIssRetido := 0;
          Valores.IssRetido := stNormal;
        end;
    end;

    Valores.ValorLiquidoNfse := ObterConteudo(ANode.Childrens.FindAnyNs('ValorLiquido'), tcDe2);

    Valores.ValorTotalNotaFiscal := Valores.ValorServicos -
      Valores.DescontoCondicionado - Valores.DescontoIncondicionado;

    NFSe.OutrasInformacoes := ObterConteudo(ANode.Childrens.FindAnyNs('Observacao'), tcStr);
    NFSe.OutrasInformacoes := StringReplace(NFSe.OutrasInformacoes, FpQuebradeLinha,
                                                    sLineBreak, [rfReplaceAll]);
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
                                                    sLineBreak, [rfReplaceAll]);

        FpCodCNAE := ObterConteudo(ANodes[i].Childrens.FindAnyNs('CodigoCnae'), tcStr);
        FpCodLCServ := ObterConteudo(ANodes[i].Childrens.FindAnyNs('ItemLei116'), tcStr);

        if NaoEstaVazio(FpCodLCServ) then
        begin
          Item := StrToIntDef(OnlyNumber(FpCodLCServ), 0);
          if Item < 100 then
            Item := Item * 100 + 1;

          FpCodLCServ := FormatFloat('0000', Item);
          FpCodLCServ := Copy(FpCodLCServ, 1, 2) + '.' + Copy(FpCodLCServ, 3, 2);
        end;

        Quantidade := ObterConteudo(ANodes[i].Childrens.FindAnyNs('Quantidade'), tcDe6);
        ValorUnitario := ObterConteudo(ANodes[i].Childrens.FindAnyNs('ValorServico'), tcDe2);

        ValorTotal := ValorUnitario * Quantidade;

        DescontoIncondicionado := ObterConteudo(ANodes[i].Childrens.FindAnyNs('ValorDesconto'), tcDe2);

        AuxNode := ANodes[i].Childrens.FindAnyNs('DadosProfissionalParceiro');

        if AuxNode <> nil then
        begin
          LerIdentificacaoParceiro(AuxNode, i);

          DadosProfissionalParceiro.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);
          DadosProfissionalParceiro.PercentualProfissionalParceiro := ObterConteudo(ANodes[i].Childrens.FindAnyNs('PercentualProfissionalParceiro'), tcDe2);
        end;
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

  NFSe.tpXML := tpXml;

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

  if not Assigned(ANode) then Exit;

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

  if not Assigned(ANode) then Exit;

  LerRpsDeclaracaoPrestacaoServico(ANode);
end;

//====== Ler o Arquivo INI===========================================
function TNFSeR_Agili.LerIni: Boolean;
var
  INIRec: TMemIniFile;
begin
  INIRec := TMemIniFile.Create('');

  // Usar o FpAOwner em vez de  FProvider

  try
    LerIniArquivoOuString(Arquivo, INIRec);

    FpTipoXML := INIRec.ReadString('IdentificacaoNFSe', 'TipoXML', '');

    if FpTipoXML = 'NFSE' then
      LerIniNfse(INIRec)
    else
      LerIniRps(INIRec);

  finally
    INIRec.Free;
  end;

  Result := True;
end;

procedure TNFSeR_Agili.LerIniNfse(AINIRec: TMemIniFile);
begin
  LerINIIdentificacaoNFSe(AINIRec);

//  LerIdentificacaoOrgaoGerador(ANode);
//  LerDadosPrestador(ANode);
//  LerNfseDeclaracaoPrestacaoServico(ANode);

//  LerCampoLink;
end;

procedure TNFSeR_Agili.LerIniRps(AINIRec: TMemIniFile);
begin
  LerINIIdentificacaoNFSe(AINIRec);
  LerINIIdentificacaoPrestador(AINIRec);
  LerINIIdentificacaoRps(AINIRec);
  LerINIDadosTomador(AINIRec);
  LerINIDadosIntermediario(AINIRec);
  LerINIConstrucaoCivil(AINIRec);
  LerINIDadosServico(AINIRec);
  LerINIDadosValores(AINIRec);
  LerINIListaServico(AINIRec);
end;

procedure TNFSeR_Agili.LerINIIdentificacaoNFSe(AINIRec: TMemIniFile);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'IdentificacaoNFSe';

  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.NfseSubstituida := AINIRec.ReadString(sSecao, 'NfseSubstituida', '');

    if FpTipoXML = 'NFSE' then
    begin
      NFSe.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
      NFSe.CodigoVerificacao := AINIRec.ReadString(sSecao, 'CodigoVerificacao', '');
      NFSe.SituacaoNFSe := StrToStatusNFSe(Ok, AINIRec.ReadString(sSecao, 'StatusNFSe', ''));
      NFSe.MotivoCancelamento := AINIRec.ReadString(sSecao, 'MotivoCancelamento', '');
    end;
  end;
end;

procedure TNFSeR_Agili.LerINIIdentificacaoPrestador(AINIRec: TMemIniFile);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'Prestador';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.RegimeEspecialTributacao := FpAOwner.StrToRegimeEspecialTributacao(Ok, AINIRec.ReadString(sSecao, 'Regime', '0'));
    NFSe.OptanteSimplesNacional := FpAOwner.StrToSimNao(Ok, AINIRec.ReadString(sSecao, 'OptanteSN', '1'));
    NFSe.OptanteMEISimei := FpAOwner.StrToSimNao(Ok, AINIRec.ReadString(sSecao, 'OptanteMEISimei', ''));

    NFSe.Prestador.IdentificacaoPrestador.CpfCnpj := AINIRec.ReadString(sSecao, 'CNPJ', '');
    NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal := AINIRec.ReadString(sSecao, 'InscricaoMunicipal', '');
  end;
end;

procedure TNFSeR_Agili.LerINIIdentificacaoRps(AINIRec: TMemIniFile);
var
  sSecao, sData: string;
  Ok: Boolean;
begin
  sSecao := 'IdentificacaoRps';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.OutrasInformacoes := AINIRec.ReadString(sSecao, 'OutrasInformacoes', '');
    NFSe.OutrasInformacoes := StringReplace(NFSe.OutrasInformacoes, FpAOwner.ConfigGeral.QuebradeLinha, sLineBreak, [rfReplaceAll]);

    NFSe.IdentificacaoRps.Numero := AINIRec.ReadString(sSecao, 'Numero', '0');
    NFSe.IdentificacaoRps.Serie := AINIRec.ReadString(sSecao, 'Serie', '0');
    NFSe.IdentificacaoRps.Tipo := FpAOwner.StrToTipoRPS(Ok, AINIRec.ReadString(sSecao, 'Tipo', '1'));

    sData := AINIRec.ReadString(sSecao, 'DataEmissao', '');
    if sData <> '' then
      NFSe.DataEmissao := StringToDateTimeDef(sData, 0);
  end;
end;

procedure TNFSeR_Agili.LerINIDadosTomador(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'Tomador';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Tomador.IdentificacaoTomador.CpfCnpj := AINIRec.ReadString(sSecao, 'CNPJCPF', '');
    NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := AINIRec.ReadString(sSecao, 'InscricaoMunicipal', '');
    NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual := AINIRec.ReadString(sSecao, 'InscricaoEstadual', '');

    NFSe.Tomador.RazaoSocial := AINIRec.ReadString(sSecao, 'RazaoSocial', '');

    NFSe.Tomador.Endereco.TipoLogradouro := AINIRec.ReadString(sSecao, 'TipoLogradouro', '');
    NFSe.Tomador.Endereco.Endereco := AINIRec.ReadString(sSecao, 'Logradouro', '');
    NFSe.Tomador.Endereco.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
    NFSe.Tomador.Endereco.Complemento := AINIRec.ReadString(sSecao, 'Complemento', '');
    NFSe.Tomador.Endereco.Bairro := AINIRec.ReadString(sSecao, 'Bairro', '');
    NFSe.Tomador.Endereco.CodigoMunicipio := AINIRec.ReadString(sSecao, 'CodigoMunicipio', '');
    NFSe.Tomador.Endereco.xMunicipio := AINIRec.ReadString(sSecao, 'xMunicipio', '');
    NFSe.Tomador.Endereco.UF := AINIRec.ReadString(sSecao, 'UF', '');
    NFSe.Tomador.Endereco.CodigoPais := AINIRec.ReadInteger(sSecao, 'CodigoPais', 0);
    NFSe.Tomador.Endereco.CEP := AINIRec.ReadString(sSecao, 'CEP', '');
    NFSe.Tomador.Endereco.xPais := AINIRec.ReadString(sSecao, 'xPais', '');

    NFSe.Tomador.Contato.Telefone := AINIRec.ReadString(sSecao, 'Telefone', '');
    NFSe.Tomador.Contato.Email := AINIRec.ReadString(sSecao, 'Email', '');
  end;
end;

procedure TNFSeR_Agili.LerINIDadosIntermediario(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'Intermediario';
  if AINIRec.SectionExists(sSecao)then
  begin
    NFSe.Intermediario.Identificacao.CpfCnpj := AINIRec.ReadString(sSecao, 'CNPJCPF', '');
    NFSe.Intermediario.Identificacao.InscricaoMunicipal := AINIRec.ReadString(sSecao, 'InscricaoMunicipal', '');
    NFSe.Intermediario.RazaoSocial := AINIRec.ReadString(sSecao, 'RazaoSocial', '');
  end;
end;

procedure TNFSeR_Agili.LerINIConstrucaoCivil(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'ConstrucaoCivil';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.ConstrucaoCivil.CodigoObra := AINIRec.ReadString(sSecao, 'CodigoObra', '');
    NFSe.ConstrucaoCivil.Art := AINIRec.ReadString(sSecao, 'Art', '');
  end;
end;

procedure TNFSeR_Agili.LerINIDadosServico(AINIRec: TMemIniFile);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'Servico';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Servico.CodigoCnae := AINIRec.ReadString(sSecao, 'CodigoCnae', '');
    NFSe.Servico.CodigoTributacaoMunicipio := AINIRec.ReadString(sSecao, 'CodigoTributacaoMunicipio', '');
    NFSe.Servico.ExigibilidadeISS := FpAOwner.StrToExigibilidadeISS(Ok, AINIRec.ReadString(sSecao, 'ExigibilidadeISS', '1'));
    NFSe.Servico.MunicipioIncidencia := AINIRec.ReadInteger(sSecao, 'MunicipioIncidencia', 0);
    NFSe.Servico.NumeroProcesso := AINIRec.ReadString(sSecao, 'NumeroProcesso', '');
    NFSe.Servico.ResponsavelRetencao := FpAOwner.StrToResponsavelRetencao(Ok, AINIRec.ReadString(sSecao, 'ResponsavelRetencao', ''));
  end;
end;

procedure TNFSeR_Agili.LerINIDadosValores(AINIRec: TMemIniFile);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'Valores';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Servico.Valores.ValorServicos := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorServicos', ''), 0);
    NFSe.Servico.Valores.ValorPis := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorPis', ''), 0);
    NFSe.Servico.Valores.ValorCofins := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorCofins', ''), 0);
    NFSe.Servico.Valores.ValorInss := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorInss', ''), 0);
    NFSe.Servico.Valores.ValorIr := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorIr', ''), 0);
    NFSe.Servico.Valores.ValorCsll := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorCsll', ''), 0);
    NFSe.Servico.Valores.ISSRetido := FpAOwner.StrToSituacaoTributaria(Ok, AINIRec.ReadString(sSecao, 'ISSRetido', '0'));
    NFSe.Servico.Valores.valorOutrasRetencoes := StringToFloatDef(AINIRec.ReadString(sSecao, 'valorOutrasRetencoes', ''), 0);
    NFSe.Servico.Valores.DescontoIncondicionado := StringToFloatDef(AINIRec.ReadString(sSecao, 'DescontoIncondicionado', ''), 0);
    NFSe.Servico.Valores.BaseCalculo := StringToFloatDef(AINIRec.ReadString(sSecao, 'BaseCalculo', ''), 0);
    NFSe.Servico.Valores.Aliquota := StringToFloatDef(AINIRec.ReadString(sSecao, 'Aliquota', ''), 0);
    NFSe.Servico.Valores.ValorIss := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorIss', ''), 0);
    NFSe.Servico.Valores.ValorLiquidoNfse := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorLiquidoNfse', ''), 0);
  end;
end;

procedure TNFSeR_Agili.LerINIListaServico(AINIRec: TMemIniFile);
var
  i: Integer;
  sSecao, sFim: string;
  Item: TItemServicoCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'Itens' + IntToStrZero(i, 3);
    sFim := AINIRec.ReadString(sSecao, 'Descricao'  ,'FIM');

    if (sFim = 'FIM') then
      break;

    Item := NFSe.Servico.ItemServico.New;

    Item.CodServ := AINIRec.ReadString(sSecao, 'CodServico', '');
    Item.Descricao := StringReplace(sFim, FpAOwner.ConfigGeral.QuebradeLinha, sLineBreak, [rfReplaceAll]);
    Item.CodigoCnae := AINIRec.ReadString(sSecao, 'CodigoCnae', '');
    Item.Quantidade := StringToFloatDef(AINIRec.ReadString(sSecao, 'Quantidade', ''), 0);
    Item.ValorUnitario := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorUnitario', ''), 0);
    Item.DescontoIncondicionado := StringToFloatDef(AINIRec.ReadString(sSecao, 'DescontoIncondicionado', ''), 0);

    LerINIDadosProfissionalParceiro(AINIRec, Item.DadosProfissionalParceiro, i);
    Inc(i);
  end;
end;

procedure TNFSeR_Agili.LerINIDadosProfissionalParceiro(AINIRec: TMemIniFile;
  DadosParceiro: TDadosProfissionalParceiro; Indice: Integer);
var
  sSecao: string;
begin
  sSecao := 'DadosProssionalParceiro' + IntToStrZero(Indice, 3);
  if AINIRec.SectionExists(sSecao) then
  begin
    DadosParceiro.IdentificacaoParceiro.CpfCnpj := AINIRec.ReadString(sSecao, 'CpfCnpj', '');
    DadosParceiro.IdentificacaoParceiro.InscricaoMunicipal := AINIRec.ReadString(sSecao, 'InscricaoMunicipal', '');
    DadosParceiro.RazaoSocial := AINIRec.ReadString(sSecao, 'RazaoSocial', '');
    DadosParceiro.PercentualProfissionalParceiro := StrToFloatDef(AINIRec.ReadString(sSecao, 'PercentualProfissionalParceiro', ''), 0);
  end;
end;

end.
