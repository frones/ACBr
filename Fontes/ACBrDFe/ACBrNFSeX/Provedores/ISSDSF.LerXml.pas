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

unit ISSDSF.LerXml;

interface

uses
  SysUtils, Classes, StrUtils, synacode,
  ACBrXmlBase, ACBrXmlDocument,
  pcnAuxiliar,
  ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { Provedor com layout próprio }
  { TNFSeR_ISSDSF }

  TNFSeR_ISSDSF = class(TNFSeRClass)
  protected

    procedure LerDeducoes(const ANode: TACBrXmlNode);
    procedure LerItens(const ANode: TACBrXmlNode);
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
//     ISSDSF
//==============================================================================

{ TNFSeR_ISSDSF }

procedure TNFSeR_ISSDSF.LerDeducoes(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
  aValor: string;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Deducoes');

  if AuxNode <> nil then
  begin
    ANodes := AuxNode.Childrens.FindAllAnyNs('Deducao');

    //Se existem deducoes exclui da nota fiscal antes de adicionar
    //para evitar duplicidade
    if Length(ANodes) > 0 then
    begin
      NFSe.Servico.Deducao.Clear;
      NFSe.Servico.Valores.ValorDeducoes := 0;
    end;

    for i := 0 to Length(ANodes) - 1 do
    begin
      NFSe.Servico.Deducao.New;
      with NFSe.Servico.Deducao[i] do
      begin
        aValor := ObterConteudo(ANodes[i].Childrens.FindAnyNs('DeducaoPor'), tcStr);

        DeducaoPor := StrToEnumerado(Ok, aValor, ['','Percentual','Valor'],
                                             [dpNenhum, dpPercentual, dpValor]);

        aValor := ObterConteudo(ANodes[i].Childrens.FindAnyNs('TipoDeducao'), tcStr);

        TipoDeducao := StrToEnumerado(Ok, aValor,
                  ['', 'Despesas com Materiais', 'Despesas com Sub-empreitada',
                   'Deducao de Valor', 'Servicos de Veiculacao e Divulgacao'],
                  [tdNenhum, tdMateriais, tdSubEmpreitada, tdValor, tdVeiculacao]);

        CpfCnpjReferencia := ObterConteudo(ANodes[i].Childrens.FindAnyNs('CPFCNPJReferencia'), tcStr);
        NumeroNFReferencia := ObterConteudo(ANodes[i].Childrens.FindAnyNs('NumeroNFReferencia'), tcStr);
        ValorTotalReferencia := ObterConteudo(ANodes[i].Childrens.FindAnyNs('ValorTotalReferencia'), tcDe2);
        PercentualDeduzir := ObterConteudo(ANodes[i].Childrens.FindAnyNs('PercentualDeduzir'), tcDe2);
        ValorDeduzir := ObterConteudo(ANodes[i].Childrens.FindAnyNs('ValorDeduzir'), tcDe2);
        NFSe.Servico.Valores.ValorDeducoes := (NFSe.Servico.Valores.ValorDeducoes + ValorDeduzir);
      end;
    end;
  end;
end;

procedure TNFSeR_ISSDSF.LerItens(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
  aValor: string;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('Itens');

  if AuxNode <> nil then
  begin
    ANodes := AuxNode.Childrens.FindAllAnyNs('Item');

    //Se existem servicos exclui da nota fiscal antes de adicionar
    //para evitar duplicidade
    if Length(ANodes) > 0 then
    begin
      NFSe.Servico.ItemServico.Clear;
      NFSe.Servico.Valores.ValorServicos := 0;
    end;
      
    for i := 0 to Length(ANodes) - 1 do
    begin
      NFSe.Servico.ItemServico.New;
      with NFSe.Servico.ItemServico[i] do
      begin
        Descricao := ObterConteudo(ANodes[i].Childrens.FindAnyNs('DiscriminacaoServico'), tcStr);
        Descricao := StringReplace(Descricao, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);
        Quantidade := ObterConteudo(ANodes[i].Childrens.FindAnyNs('Quantidade'), tcDe2);
        ValorUnitario := ObterConteudo(ANodes[i].Childrens.FindAnyNs('ValorUnitario'), tcDe2);
        ValorTotal := ObterConteudo(ANodes[i].Childrens.FindAnyNs('ValorTotal'), tcDe2);

        aValor := ObterConteudo(ANodes[i].Childrens.FindAnyNs('Tributavel'), tcStr);

        Tributavel := StrToEnumerado(Ok, aValor, ['N','S'], [snNao, snSim]);

        NFSe.Servico.Valores.ValorServicos := (NFSe.Servico.Valores.ValorServicos + ValorTotal);
      end;
    end;
  end;
end;

function TNFSeR_ISSDSF.LerXml: Boolean;
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

  if (Pos('NumeroNota', Arquivo) > 0) or (Pos('NumeroNFe', Arquivo) > 0) then
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

function TNFSeR_ISSDSF.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  aValor, xUF: string;
  Ok :Boolean;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  NFSe.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('NumeroNota'), tcStr);

  if (NFSe.Numero = '') then
    NFSe.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('NumeroNFe'), tcStr);

  NFSe.NumeroLote := ObterConteudo(ANode.Childrens.FindAnyNs('NumeroLote'), tcStr);

  NFSe.CodigoVerificacao := ObterConteudo(ANode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
  if NFSe.CodigoVerificacao = '' then
    NFSe.CodigoVerificacao := ObterConteudo(ANode.Childrens.FindAnyNs('CodigoVerificao'), tcStr);

  NFSe.DataEmissaoRps := ObterConteudo(ANode.Childrens.FindAnyNs('DataEmissaoRPS'), tcDat);

  NFSe.DataEmissao := ObterConteudo(ANode.Childrens.FindAnyNs('DataProcessamento'), tcDatHor);
  if (NFSe.DataEmissao = 0) then
    NFSe.DataEmissao := NFSe.DataEmissaoRps;

  NFSe.Competencia := NFSe.DataEmissaoRps;

  aValor := ObterConteudo(ANode.Childrens.FindAnyNs('SituacaoRPS'), tcStr);
  if aValor <> '' then 
    NFSe.StatusRps := StrToEnumerado(ok, aValor, ['N','C'], [srNormal, srCancelado]);

  aValor := ObterConteudo(ANode.Childrens.FindAnyNs('RazaoSocialPrestador'), tcStr);

  if aValor <> '' then
  begin
    with NFSe.Prestador do
    begin
      RazaoSocial := aValor;

      with IdentificacaoPrestador do
      begin
        InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('InscricaoMunicipalPrestador'), tcStr);
      end;

      with Contato do
      begin
        Telefone := ObterConteudo(ANode.Childrens.FindAnyNs('DDDPrestador'), tcStr) + ObterConteudo(ANode.Childrens.FindAnyNs('TelefonePrestador'), tcStr);
      end;
    end;
  end;

  with NFSe.IdentificacaoRps do
  begin
    Numero := ObterConteudo(ANode.Childrens.FindAnyNs('NumeroRPS'), tcStr);
    Serie := ObterConteudo(ANode.Childrens.FindAnyNs('SerieRPS'), tcStr);
    Tipo := trRPS;
  end;

  if NFSe.InfID.ID = '' then
    NFSe.InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero);

  aValor := ObterConteudo(ANode.Childrens.FindAnyNs('SeriePrestacao'), tcStr);
  if aValor <> '' then
    NFSe.SeriePrestacao := aValor;

  aValor := ObterConteudo(ANode.Childrens.FindAnyNs('RazaoSocialTomador'), tcStr);

  if aValor <> '' then
  begin
    with NFSe.Tomador do
    begin
      RazaoSocial := aValor;

      with IdentificacaoTomador do
      begin
        InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('InscricaoMunicipalTomador'), tcStr);
        CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('CPFCNPJTomador'), tcStr);
      end;

      with Endereco do
      begin
        TipoLogradouro := ObterConteudo(ANode.Childrens.FindAnyNs('TipoLogradouroTomador'), tcStr);
        Endereco := ObterConteudo(ANode.Childrens.FindAnyNs('LogradouroTomador'), tcStr);
        Numero := ObterConteudo(ANode.Childrens.FindAnyNs('NumeroEnderecoTomador'), tcStr);
        Complemento := ObterConteudo(ANode.Childrens.FindAnyNs('ComplementoEnderecoTomador'), tcStr);
        TipoBairro := ObterConteudo(ANode.Childrens.FindAnyNs('TipoBairroTomador'), tcStr);

        //Existe um bug nesse provedor a consulta de nota nao retorna o bairro
        //entao deixamos o bairro carregado do RPS
        AValor := ObterConteudo(ANode.Childrens.FindAnyNs('BairroTomador'), tcStr);
        if aValor <> '' then
          Bairro := AValor;

        CEP := ObterConteudo(ANode.Childrens.FindAnyNs('CEPTomador'), tcStr);

        aValor := ObterConteudo(ANode.Childrens.FindAnyNs('CidadeTomador'), tcStr);

        if aValor <> '' then
        begin
          CodigoMunicipio := CodTOMToCodIBGE(aValor);

          xMunicipio := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);

          if UF = '' then
            UF := xUF;
        end;
      end;

      with Contato do
      begin
        Telefone := ObterConteudo(ANode.Childrens.FindAnyNs('DDDTomador'), tcStr) + ObterConteudo(ANode.Childrens.FindAnyNs('TelefoneTomador'), tcStr);
        Email := ObterConteudo(ANode.Childrens.FindAnyNs('EmailTomador'), tcStr);
      end;
    end;
  end;

  NFSe.TipoRecolhimento := AnsiUpperCase(ObterConteudo(ANode.Childrens.FindAnyNs('TipoRecolhimento'), tcStr));

  aValor := ObterConteudo(ANode.Childrens.FindAnyNs('CodigoAtividade'), tcStr);
  if aValor <> '' then
  begin
    with NFSe.Servico do
    begin
      CodigoCnae := aValor;
      CodigoTributacaoMunicipio := aValor;

      aValor := ObterConteudo(ANode.Childrens.FindAnyNs('MunicipioPrestacao'), tcStr);
      if aValor <> '' then
        CodigoMunicipio := CodTOMToCodIBGE(aValor);

      with Valores do
      begin
        Aliquota := ObterConteudo(ANode.Childrens.FindAnyNs('AliquotaAtividade'), tcDe3);
        ValorPis := ObterConteudo(ANode.Childrens.FindAnyNs('ValorPIS'), tcDe2);
        ValorCofins := ObterConteudo(ANode.Childrens.FindAnyNs('ValorCOFINS'), tcDe2);
        ValorInss := ObterConteudo(ANode.Childrens.FindAnyNs('ValorINSS'), tcDe2);
        ValorIr := ObterConteudo(ANode.Childrens.FindAnyNs('ValorIR'), tcDe2);
        ValorCsll := ObterConteudo(ANode.Childrens.FindAnyNs('ValorCSLL'), tcDe2);
        AliquotaPIS := ObterConteudo(ANode.Childrens.FindAnyNs('AliquotaPIS'), tcDe2);
        AliquotaCOFINS := ObterConteudo(ANode.Childrens.FindAnyNs('AliquotaCOFINS'), tcDe2);
        AliquotaINSS := ObterConteudo(ANode.Childrens.FindAnyNs('AliquotaINSS'), tcDe2);
        AliquotaIR := ObterConteudo(ANode.Childrens.FindAnyNs('AliquotaIR'), tcDe2);
        AliquotaCSLL := ObterConteudo(ANode.Childrens.FindAnyNs('AliquotaCSLL'), tcDe2);

        aValor := ObterConteudo(ANode.Childrens.FindAnyNs('TipoRecolhimento'), tcStr);
        IssRetido := StrToEnumerado(Ok, aValor, ['A','R'], [stNormal, stRetencao]);

        RetencoesFederais := ValorPis + ValorCofins + ValorInss + ValorIr + ValorCsll;
      end;
    end;

    aValor := ObterConteudo(ANode.Childrens.FindAnyNs('Operacao'), tcStr);
    NFSe.Servico.Operacao := StrToOperacao(Ok, aValor);
    NFSe.DeducaoMateriais := StrToEnumerado(Ok, aValor, ['A','B'], [snNao, snSim]);

    aValor := ObterConteudo(ANode.Childrens.FindAnyNs('Tributacao'), tcStr);
    NFSe.Servico.Tributacao := FpAOwner.StrToTributacao(Ok, aValor);
    NFSe.NaturezaOperacao := StrToEnumerado(Ok, aValor, ['T','K'], [NFSe.NaturezaOperacao, no5]);
    NFSe.OptanteSimplesNacional := StrToEnumerado(Ok, aValor, ['T','H'], [snNao, snSim]);
    NFse.RegimeEspecialTributacao := StrToEnumerado(Ok, aValor, ['T','M'], [retNenhum, retMicroempresarioIndividual]);
  end;

  aValor := ObterConteudo(ANode.Childrens.FindAnyNs('DescricaoRPS'), tcStr);
  if aValor <> '' then
    NFSe.OutrasInformacoes := aValor;

  NFSe.OutrasInformacoes := StringReplace(NFSe.OutrasInformacoes, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);

  aValor := ObterConteudo(ANode.Childrens.FindAnyNs('MotCancelamento'), tcStr);
  if aValor <> '' then
    NFSE.MotivoCancelamento := aValor;

  aValor := ObterConteudo(ANode.Childrens.FindAnyNs('CPFCNPJIntermediario'), tcStr);
  if aValor <> '' then
    NFSe.Intermediario.Identificacao.CpfCnpj := aValor;

  NFSe.Link := ObterConteudo(ANode.Childrens.FindAnyNs('URLNotaFiscal'), tcStr);
  NFSe.Link := StringReplace(NFSe.Link, '&amp;', '&', [rfReplaceAll]);

  LerDeducoes(ANode);
  LerItens(ANode);

  with NFSe.Servico do
  begin
    if (Operacao in [toSemDeducao, toComDeducaoMateriais]) then
    begin
      if CodigoMunicipio = NFSe.Prestador.Endereco.CodigoMunicipio then
        NFSe.NaturezaOperacao := no1
      else
        NFSe.NaturezaOperacao := no2;
    end
    else
    begin
      if (Operacao = toImuneIsenta) and (Tributacao = ttIsentaISS) then
        NFSe.NaturezaOperacao := no3
      else
      begin
        if (Operacao = toImuneIsenta) and (Tributacao = ttImune) then
          NFSe.NaturezaOperacao := no4
        else
        begin
          if (Operacao = toSemDeducao) and (Tributacao = ttNaoTributavel) then
            NFSe.NaturezaOperacao := no7;
        end;
      end;
    end;
  end;

  with NFSe.Servico.Valores do
  begin
    if ((ValorIssRetido = 0) or (ValorIss = 0)) and (Aliquota > 0) then
    begin
      if IssRetido = stRetencao then
         ValorIssRetido := (ValorServicos * (Aliquota / 100));

      ValorIss := (ValorServicos * (Aliquota / 100));
    end;

    ValorLiquidoNfse := ValorServicos -
                        (RetencoesFederais + ValorDeducoes + DescontoCondicionado+
                         DescontoIncondicionado + ValorIssRetido);

    BaseCalculo := ValorServicos - (ValorDeducoes + DescontoIncondicionado);

    ValorTotalNotaFiscal := ValorServicos - DescontoCondicionado -
                            DescontoIncondicionado;
  end;
end;

function TNFSeR_ISSDSF.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  aValor, xUF: string;
  Ok: Boolean;
begin
  Result := True;

  with NFSe do
  begin
    with Prestador do
    begin
      RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('RazaoSocialPrestador'), tcStr);

      with IdentificacaoPrestador do
      begin
        InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('InscricaoMunicipalPrestador'), tcStr);
      end;

      with Contato do
      begin
        aValor := ObterConteudo(ANode.Childrens.FindAnyNs('DDDPrestador'), tcStr);
        Telefone := aValor + ObterConteudo(ANode.Childrens.FindAnyNs('TelefonePrestador'), tcStr);
      end;
    end;

    with IdentificacaoRps do
    begin
      Serie := ObterConteudo(ANode.Childrens.FindAnyNs('SerieRPS'), tcStr);
      Numero := ObterConteudo(ANode.Childrens.FindAnyNs('NumeroRPS'), tcStr);
      Tipo := trRPS;
    end;

    DataEmissaoRps := ObterConteudo(ANode.Childrens.FindAnyNs('DataEmissaoRPS'), tcDatHor);
    aValor := ObterConteudo(ANode.Childrens.FindAnyNs('SituacaoRPS'), tcStr);

    if aValor = 'N' then
      StatusRps := srNormal
    else
      StatusRps := srCancelado;

    with RpsSubstituido do
    begin
      Serie := ObterConteudo(ANode.Childrens.FindAnyNs('SerieRPSSubstituido'), tcStr);
      Numero := ObterConteudo(ANode.Childrens.FindAnyNs('NumeroRPSSubstituido'), tcStr);
    end;

    NfseSubstituida := ObterConteudo(ANode.Childrens.FindAnyNs('NumeroNFSeSubstituida'), tcStr);
    //DataEmissaoNFSeSubstituida := ObterConteudo(ANode.Childrens.FindAnyNs('DataEmissaoNFSeSubstituida'), tcDat);

    SeriePrestacao := ObterConteudo(ANode.Childrens.FindAnyNs('SeriePrestacao'), tcStr);

    with Tomador do
    begin
      with IdentificacaoTomador do
      begin
        InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('InscricaoMunicipalTomador'), tcStr);
        CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('CPFCNPJTomador'), tcStr);
        DocEstrangeiro := ObterConteudo(ANode.Childrens.FindAnyNs('DocTomadorEstrangeiro'), tcStr);
      end;

      RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('RazaoSocialTomador'), tcStr);

      with Endereco do
      begin
        TipoLogradouro := ObterConteudo(ANode.Childrens.FindAnyNs('TipoLogradouroTomador'), tcStr);
        Endereco := ObterConteudo(ANode.Childrens.FindAnyNs('LogradouroTomador'), tcStr);
        Numero := ObterConteudo(ANode.Childrens.FindAnyNs('NumeroEnderecoTomador'), tcStr);
        Complemento := ObterConteudo(ANode.Childrens.FindAnyNs('ComplementoEnderecoTomador'), tcStr);
        TipoBairro := ObterConteudo(ANode.Childrens.FindAnyNs('TipoBairroTomador'), tcStr);
        Bairro := ObterConteudo(ANode.Childrens.FindAnyNs('BairroTomador'), tcStr);
        aValor := ObterConteudo(ANode.Childrens.FindAnyNs('CidadeTomador'), tcStr);

        if aValor <> '' then
          CodigoMunicipio := CodTOMToCodIBGE(aValor);

        xMunicipio := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);

        if UF = '' then
          UF := xUF;

        CEP := ObterConteudo(ANode.Childrens.FindAnyNs('CEPTomador'), tcStr);
      end;

      with Contato do
      begin
        Email := ObterConteudo(ANode.Childrens.FindAnyNs('EmailTomador'), tcStr);
        aValor := ObterConteudo(ANode.Childrens.FindAnyNs('DDDTomador'), tcStr);
        Telefone := aValor + ObterConteudo(ANode.Childrens.FindAnyNs('TelefoneTomador'), tcStr);
      end;
    end;

    TipoRecolhimento := AnsiUpperCase(ObterConteudo(ANode.Childrens.FindAnyNs('TipoRecolhimento'), tcStr));

    with ConstrucaoCivil.Endereco do
    begin
      CodigoMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('InscricaoMunicipalObra'), tcStr);
    end;

    with Servico do
    begin
      CodigoCnae := ObterConteudo(ANode.Childrens.FindAnyNs('CodigoAtividade'), tcStr);
      aValor := ObterConteudo(ANode.Childrens.FindAnyNs('MunicipioPrestacao'), tcStr);
      if aValor <> '' then
        CodigoMunicipio := CodTOMToCodIBGE(aValor);
      aValor := ObterConteudo(ANode.Childrens.FindAnyNs('Operacao'), tcStr);
      Operacao := StrToOperacao(Ok, aValor);
      aValor := ObterConteudo(ANode.Childrens.FindAnyNs('Tributacao'), tcStr);
      Tributacao := FpAOwner.StrToTributacao(Ok, aValor);

      with Valores do
      begin
        Aliquota := ObterConteudo(ANode.Childrens.FindAnyNs('AliquotaAtividade'), tcDe4);

        if TipoRecolhimento = 'A' then
          IssRetido := stNormal
        else
          IssRetido := stRetencao;

        ValorPis := ObterConteudo(ANode.Childrens.FindAnyNs('ValorPIS'), tcDe2);
        ValorCofins := ObterConteudo(ANode.Childrens.FindAnyNs('ValorCOFINS'), tcDe2);
        ValorInss := ObterConteudo(ANode.Childrens.FindAnyNs('ValorINSS'), tcDe2);
        ValorIr := ObterConteudo(ANode.Childrens.FindAnyNs('ValorIR'), tcDe2);
        ValorCsll := ObterConteudo(ANode.Childrens.FindAnyNs('ValorCSLL'), tcDe2);
        AliquotaPIS := ObterConteudo(ANode.Childrens.FindAnyNs('AliquotaPIS'), tcDe4);
        AliquotaCOFINS := ObterConteudo(ANode.Childrens.FindAnyNs('AliquotaCOFINS'), tcDe4);
        AliquotaINSS := ObterConteudo(ANode.Childrens.FindAnyNs('AliquotaINSS'), tcDe4);
        AliquotaIR := ObterConteudo(ANode.Childrens.FindAnyNs('AliquotaIR'), tcDe4);
        AliquotaCSLL := ObterConteudo(ANode.Childrens.FindAnyNs('AliquotaCSLL'), tcDe4);
      end;
    end;

    OutrasInformacoes := ObterConteudo(ANode.Childrens.FindAnyNs('DescricaoRPS'), tcStr);
    OutrasInformacoes := StringReplace(OutrasInformacoes, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);

    MotivoCancelamento := ObterConteudo(ANode.Childrens.FindAnyNs('MotCancelamento'), tcStr);
    Intermediario.Identificacao.CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('CPFCNPJIntermediario'), tcStr);

    LerDeducoes(ANode);
    LerItens(ANode);
  end;
end;

end.
