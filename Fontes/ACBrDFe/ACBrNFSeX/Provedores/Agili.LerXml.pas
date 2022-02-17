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
  ACBrUtil,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { Provedor com layout próprio }
  { TNFSeR_Agili }

  TNFSeR_Agili = class(TNFSeRClass)
  protected
    FpcodCNAE: string;
    FpCodLCServ: string;

    procedure LerListaServico(const ANode: TACBrXmlNode);
    procedure LerResponsavelISSQN(const ANode: TACBrXmlNode);
    procedure LerRegimeEspecialTributacao(const ANode: TACBrXmlNode);
    procedure LerExigibilidadeISSQN(const ANode: TACBrXmlNode);
    procedure LerMunicipioIncidencia(const ANode: TACBrXmlNode);
    procedure LerInfDeclaracaoPrestacaoServico(const ANode: TACBrXmlNode);
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

        xMunicipio := CodIBGEToCidade(StrToIntDef(CodigoMunicipio, 0));
        UF := ObterConteudo(AuxMun.Childrens.FindAnyNs('Uf'), tcStr);
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

        xMunicipio := CodIBGEToCidade(StrToIntDef(CodigoMunicipio, 0));
        UF := ObterConteudo(AuxMun.Childrens.FindAnyNs('Uf'), tcStr);
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
  aValor: string;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('ExigibilidadeISSQN');

  if AuxNode <> nil then
  begin
    with NFSe.Servico do
    begin
      aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('Codigo'), tcStr);

      ExigibilidadeISS := StrToEnumerado(Ok, aValor, ['-1','-2','-3','-4','-5','-6','-7'],
        [exiExigivel, exiNaoIncidencia, exiIsencao, exiExportacao, exiImunidade,
         exiSuspensaDecisaoJudicial, exiSuspensaProcessoAdministrativo]);
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
  aValor: string;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoRps');

  if AuxNode <> nil then
  begin
    with NFSe.IdentificacaoRps do
    begin
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Serie  := ObterConteudo(AuxNode.Childrens.FindAnyNs('Serie'), tcStr);

      aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('Tipo'), tcStr);

      Tipo := StrToEnumerado(ok, aValor, ['-2','-4','-5'], [trRPS, trNFConjugada, trCupom]);

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

procedure TNFSeR_Agili.LerInfDeclaracaoPrestacaoServico(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  aValor: string;
  Ok: Boolean;

  function _StrToSimNao(out ok: boolean; const s: String): TnfseSimNao;
  begin
    result := StrToEnumerado(ok, s,
                             ['1','0'],
                             [snSim, snNao]);
  end;

begin
  AuxNode := ANode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');

  if AuxNode <> nil then
  begin
    LerIdentificacaoPrestador(AuxNode);
    LerRps(AuxNode);
    LerDadosTomador(AuxNode);
    LerRegimeEspecialTributacao(AuxNode);

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('OptanteSimplesNacional'), tcStr);

    NFSe.OptanteSimplesNacional := _StrToSimNao(Ok, aValor);

    NFSe.NfseSubstituida := ObterConteudo(AuxNode.Childrens.FindAnyNs('NfseSubstituida'), tcStr);

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('OptanteMEISimei'), tcStr);
    NFSe.OptanteMEISimei := _StrToSimNao(Ok, aValor);

    LerResponsavelISSQN(AuxNode);
    LerExigibilidadeISSQN(AuxNode);
    LerListaServico(AuxNode);

    with NFSe.Servico do
    begin
      CodigoTributacaoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoAtividadeEconomica'), tcStr);

      if CodigoTributacaoMunicipio = '' then
        CodigoTributacaoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoCnaeAtividadeEconomica'), tcStr);

      if CodigoTributacaoMunicipio = '' then
        CodigoTributacaoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('ItemLei116AtividadeEconomica'), tcStr);

      CodigoCnae := FpcodCNAE;
      ItemListaServico := FpCodLCServ;

      if FpAOwner.ConfigGeral.TabServicosExt then
        xItemListaServico := ObterDescricaoServico(OnlyNumber(ItemListaServico))
      else
        xItemListaServico := CodItemServToDesc(OnlyNumber(ItemListaServico));

      NumeroProcesso := ObterConteudo(AuxNode.Childrens.FindAnyNs('BeneficioProcesso'), tcStr);

      with Valores do
      begin
        ValorServicos := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorServicos'), tcDe2);
        DescontoIncondicionado := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorDescontos'), tcDe2);
        ValorPis := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorPis'), tcDe2);
        ValorCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorCofins'), tcDe2);
        ValorInss := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorInss'), tcDe2);
        ValorIr := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorIrrf'), tcDe2);
        ValorCsll := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorCsll'), tcDe2);
        valorOutrasRetencoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorOutrasRetencoes'), tcDe2);
        BaseCalculo := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorBaseCalculoISSQN'), tcDe2);
        Aliquota := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaISSQN'), tcDe3);
        ValorIss := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorISSQNCalculado'), tcDe2);

        aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('ISSQNRetido'), tcStr);

        case _StrToSimNao(Ok, aValor) of
          snSim: ValorIssRetido := ValorIss;
          snNao: ValorIssRetido := 0;
        end;

        ValorLiquidoNfse := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorLiquido'), tcDe2);
      end;

      NFSe.OutrasInformacoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('Observacao'), tcStr);

    end;

    LerMunicipioIncidencia(AuxNode);
  end;
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
        Quantidade := ObterConteudo(ANodes[i].Childrens.FindAnyNs('Quantidade'), tcDe6);
        ValorTotal := ObterConteudo(ANodes[i].Childrens.FindAnyNs('ValorServico'), tcDe2);

        DescontoIncondicionado := ObterConteudo(ANodes[i].Childrens.FindAnyNs('ValorDesconto'), tcDe2);

        FpcodCNAE := ObterConteudo(ANodes[i].Childrens.FindAnyNs('CodigoCnae'), tcStr);
        CodServ   := ObterConteudo(ANodes[i].Childrens.FindAnyNs('ItemLei116'), tcStr);

        Item := StrToIntDef(OnlyNumber(CodServ), 0);
        if Item < 100 then
          Item := Item * 100 + 1;

        CodServ := FormatFloat('0000', Item);
        CodServ := Copy(CodServ, 1, 2) + '.' + Copy(CodServ, 3, 2);
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

procedure TNFSeR_Agili.LerRegimeEspecialTributacao(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  aValor: string;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('RegimeEspecialTributacao');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('Codigo'), tcStr);

      RegimeEspecialTributacao := StrToEnumerado(Ok, aValor, ['-1','-2','-4','-5','-6'],
                   [retNenhum, retEstimativa, retCooperativa,
                    retMicroempresarioIndividual, retMicroempresarioEmpresaPP]);
    end;
  end;
end;

procedure TNFSeR_Agili.LerResponsavelISSQN(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  aValor: string;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('ResponsavelISSQN');

  if AuxNode <> nil then
  begin
    with NFSe.Servico do
    begin
      aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('Codigo'), tcStr);

      ResponsavelRetencao := StrToEnumerado(Ok, aValor, ['-1', '-2', '-3'],
                                         [rtTomador, rtIntermediario, rtPrestador]);
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

procedure TNFSeR_Agili.LerSituacaoNfse(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('SituacaoNfse');

  if AuxNode <> nil then
  begin
    NFSe.Situacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('Codigo'), tcInt);
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
  xRetorno: string;
begin
//italo  xRetorno := TratarXmlRetorno(Arquivo);
  xRetorno := Arquivo;

  if EstaVazio(xRetorno) then
    raise Exception.Create('Arquivo xml não carregado.');

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(xRetorno);

  if (Pos('Nfse', xRetorno) > 0) then
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
  LerInfDeclaracaoPrestacaoServico(ANode);
end;

function TNFSeR_Agili.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  LerInfDeclaracaoPrestacaoServico(ANode);
end;

end.
