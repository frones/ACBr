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
    procedure LerContatoTomador(const ANode: TACBrXmlNode);
    procedure LerCpfCnpj(const ANode: TACBrXmlNode);
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

procedure TNFSeR_Agili.LerContatoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Contato');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Contato do
    begin
      Telefone := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Telefone'), tcStr);
      Email    := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Email'), tcStr);
    end;
  end;
end;

procedure TNFSeR_Agili.LerCpfCnpj(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('CpfCnpj');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.IdentificacaoTomador do
    begin
      CpfCnpj := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Cpf'), tcStr);

      if CpfCnpj = '' then
        CpfCnpj := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Cnpj'), tcStr);
    end;
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
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);

      with IdentificacaoTomador do
      begin
        InscricaoEstadual := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('InscricaoEstadual'), tcStr);
      end;

      with Endereco do
      begin
        TipoLogradouro := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TipoLogradouro'), tcStr);
        Endereco := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Logradouro'), tcStr);
        Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
        Complemento := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Complemento'), tcStr);
        Bairro := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);
        CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipioIBGE'), tcStr);
        UF := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Uf'), tcStr);
        CEP := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Cep'), tcStr);

        if length(CodigoMunicipio) < 7 then
          CodigoMunicipio := Copy(CodigoMunicipio, 1, 2) +
              FormatFloat('00000', StrToIntDef(Copy(CodigoMunicipio, 3, 5), 0));

        if UF = '' then
          UF := NFSe.Prestador.Endereco.UF;

        xMunicipio := CodIBGEToCidade(StrToIntDef(CodigoMunicipio, 0));
      end;
    end;

    LerIdentificacaoTomador(AuxNode);
    LerEnderecoTomador(AuxNode);
    LerContatoTomador(AuxNode);
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
      TipoLogradouro := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TipoLogradouro'), tcStr);
      Endereco := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Logradouro'), tcStr);
      Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Complemento := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Complemento'), tcStr);
      Bairro := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);

      AuxMun := AuxNode.Childrens.FindAnyNs('Municipio');

      if AuxMun <> nil then
      begin
        CodigoMunicipio := ProcessarConteudo(AuxMun.Childrens.FindAnyNs('CodigoMunicipioIBGE'), tcStr);

        if length(CodigoMunicipio) < 7 then
          CodigoMunicipio := Copy(CodigoMunicipio, 1, 2) +
              FormatFloat('00000', StrToIntDef(Copy(CodigoMunicipio, 3, 5), 0));

        xMunicipio := CodIBGEToCidade(StrToIntDef(CodigoMunicipio, 0));
        UF := ProcessarConteudo(AuxMun.Childrens.FindAnyNs('Uf'), tcStr);

        if UF = '' then
          UF := NFSe.Prestador.Endereco.UF;
      end;

      AuxPais := AuxNode.Childrens.FindAnyNs('Pais');

      if AuxPais <> nil then
      begin
        CodigoPais := ProcessarConteudo(AuxPais.Childrens.FindAnyNs('CodigoPaisBacen'), tcStr);
      end;

      CEP := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Cep'), tcStr);
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
      aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Codigo'), tcStr);

      ExigibilidadeISS := StrToEnumerado(Ok, aValor, ['-1','-2','-3','-4','-5','-6','-7'],
        [exiExigivel, exiNaoIncidencia, exiIsencao, exiExportacao, exiImunidade,
         exiSuspensaDecisaoJudicial, exiSuspensaProcessoAdministrativo]);
    end;
  end;
end;

procedure TNFSeR_Agili.LerIdentificacaoPrestador(
  const ANode: TACBrXmlNode);
var
  AuxNode, AuxNodeCpfCnpj: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoPrestador');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.IdentificacaoPrestador do
    begin
      // A tag ChaveDigital não é lida do XML pois é para constar na propriedade
      // de configuração ChaveAcesso

      Cnpj := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Cnpj'), tcStr);

      if Cnpj = '' then
      begin
        AuxNodeCpfCnpj := AuxNode.Childrens.FindAnyNs('CpfCnpj');

        with NFSe.Tomador.IdentificacaoTomador do
        begin
          if AuxNodeCpfCnpj <> nil then
          begin
            Cnpj := ProcessarConteudo(AuxNodeCpfCnpj.Childrens.FindAnyNs('Cpf'), tcStr);

            if Cnpj = '' then
              Cnpj := ProcessarConteudo(AuxNodeCpfCnpj.Childrens.FindAnyNs('Cnpj'), tcStr);
          end;
        end;
      end;

      InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
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
      Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Serie  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Serie'), tcStr);

      aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Tipo'), tcStr);

      if provedor = proAgili then
        Tipo := StrToEnumerado(ok, aValor, ['-2','-4','-5'],
                                                [trRPS, trNFConjugada, trCupom])
      else
        Tipo := StrToTipoRPS(Ok, aValor);

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
      InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);

      LerCpfCnpj(AuxNode);
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
  AuxNode := ANode.Childrens.FindAnyNs('InfDeclaracaoPrestacaoServico');

  if AuxNode <> nil then
  begin
    aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('OptanteSimplesNacional'), tcStr);

    NFSe.OptanteSimplesNacional := _StrToSimNao(Ok, aValor);

    if Provedor = proAgili then
    begin
      NFSe.NfseSubstituida := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NfseSubstituida'), tcStr);

      aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('OptanteMEISimei'), tcStr);
      NFSe.OptanteMEISimei := _StrToSimNao(Ok, aValor);
    end
    else
    begin
      aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('RegimeEspecialTributacao'), tcStr);
      NFSe.RegimeEspecialTributacao := StrToRegimeEspecialTributacao(Ok, aValor);

      with NFSe.Servico do
      begin
        aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ResponsavelRetencao'), tcStr);
        ResponsavelRetencao := StrToResponsavelRetencao(Ok, aValor);
        aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ExigibilidadeIss'), tcStr);
        ExigibilidadeISS    := StrToExigibilidadeISS(Ok, aValor);
        CodigoMunicipio     := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('MunicipioIncidencia'), tcStr);
        MunicipioIncidencia := StrToIntDef(CodigoMunicipio, 0);
      end;
    end;

    aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Producao'), tcStr);
    NFSe.Producao := _StrToSimNao(Ok, aValor);

    with NFSe.Servico do
    begin
      CodigoTributacaoMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CodigoAtividadeEconomica'), tcStr);
      CodigoCnae                := FpcodCNAE;
      ItemListaServico          := FpCodLCServ;

      if FAOwner.ConfigGeral.TabServicosExt then
        xItemListaServico := ObterDescricaoServico(OnlyNumber(ItemListaServico))
      else
        xItemListaServico := CodItemServToDesc(OnlyNumber(ItemListaServico));

      if Provedor = proAgili then
      begin
        NumeroProcesso := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('BeneficioProcesso'), tcStr);

        with Valores do
        begin
          ValorServicos := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorServicos'), tcDe2);
          DescontoIncondicionado := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorDescontos'), tcDe2);
          ValorPis := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorPis'), tcDe2);
          ValorCofins := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorCofins'), tcDe2);
          ValorInss := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorInss'), tcDe2);
          ValorIr := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorIrrf'), tcDe2);
          ValorCsll := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorCsll'), tcDe2);
          valorOutrasRetencoes := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorOutrasRetencoes'), tcDe2);
          BaseCalculo := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorBaseCalculoISSQN'), tcDe2);
          Aliquota := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('AliquotaISSQN'), tcDe3);
          ValorIss := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorISSQNCalculado'), tcDe2);

          aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ISSQNRetido'), tcStr);

          case _StrToSimNao(Ok, aValor) of
            snSim: ValorIssRetido := ValorIss;
            snNao: ValorIssRetido := 0;
          end;

          ValorLiquidoNfse := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorLiquido'), tcDe2);
        end;

        NFSe.OutrasInformacoes := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Observacao'), tcStr);
      end
      else
      begin
        NumeroProcesso := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NumeroProcesso'), tcStr);

        with Valores do
        begin
          ValorServicos := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorServicos'), tcDe2);
          DescontoIncondicionado := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorDescontos'), tcDe2);
          ValorPis := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorPis'), tcDe2);
          ValorCofins := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorCofins'), tcDe2);
          ValorInss := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorInss'), tcDe2);
          ValorIr := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorIr'), tcDe2);
          ValorCsll := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorCsll'), tcDe2);
          valorOutrasRetencoes := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorOutrasRetencoes'), tcDe2);
          BaseCalculo := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorBaseCalculoIss'), tcDe2);
          Aliquota := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Aliquota'), tcDe3);
          ValorIss := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorIss'), tcDe2);

          aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('IssRetido'), tcStr);

          case _StrToSimNao(Ok, aValor) of
            snSim: ValorIssRetido := ValorIss;
            snNao: ValorIssRetido := 0;
          end;

          ValorLiquidoNfse := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorLiquido'), tcDe2);
        end;
      end;
    end;

    LerRps(AuxNode);
    LerIdentificacaoPrestador(AuxNode);
    LerDadosTomador(AuxNode);
    LerListaServico(AuxNode);
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
        Descricao  := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('Discriminacao'), tcStr);
        Quantidade := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('Quantidade'), tcDe6);
        ValorTotal := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('ValorServico'), tcDe2);

        DescontoIncondicionado := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('ValorDesconto'), tcDe2);

        FpcodCNAE := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('CodigoCnae'), tcStr);
        CodServ   := ProcessarConteudo(ANodes[i].Childrens.FindAnyNs('ItemLei116'), tcStr);

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
      CodigoMunicipio     := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipioIBGE'), tcStr);
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
      aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Codigo'), tcStr);

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
      aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Codigo'), tcStr);

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
    NFSe.DataEmissao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('DataEmissao'), tcDat);

    LerIdentificacaoRps(AuxNode);
  end;
end;

function TNFSeR_Agili.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
  xRetorno: string;
begin
  xRetorno := TratarXmlRetorno(Arquivo);

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

  LerListaServico(ANode);
  LerResponsavelISSQN(ANode);
  LerRegimeEspecialTributacao(ANode);
  LerExigibilidadeISSQN(ANode);
  LerMunicipioIncidencia(ANode);

  LerInfDeclaracaoPrestacaoServico(ANode);
end;

function TNFSeR_Agili.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  LerInfDeclaracaoPrestacaoServico(ANode);
end;

end.
