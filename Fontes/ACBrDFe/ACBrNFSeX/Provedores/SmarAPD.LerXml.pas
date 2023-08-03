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

unit SmarAPD.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml, ACBrNFSeXLerXml_ABRASFv2;

type
  { TNFSeR_SmarAPD }

  TNFSeR_SmarAPD = class(TNFSeRClass)
  protected

    procedure LerItens(const ANode: TACBrXmlNode);
    procedure LerFatura(const ANode: TACBrXmlNode);
    procedure LerFaturas(const ANode: TACBrXmlNode);
    procedure LerServico(const ANode: TACBrXmlNode);
    procedure LerServicos(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

  { TNFSeR_SmarAPD203 }

  TNFSeR_SmarAPD203 = class(TNFSeR_ABRASFv2)
  protected

  public

  end;

  { TNFSeR_SmarAPD204 }

  TNFSeR_SmarAPD204 = class(TNFSeR_ABRASFv2)
  protected

  public

  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     SmarAPD
//==============================================================================

{ TNFSeR_SmarAPD }

procedure TNFSeR_SmarAPD.LerFatura(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('tbfatura');

  if AuxNode = nil then Exit;

  LerFaturas(AuxNode);
end;

procedure TNFSeR_SmarAPD.LerFaturas(const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  ANodes := ANode.Childrens.FindAllAnyNs('fatura');

  NFSe.CondicaoPagamento.Parcelas.Clear;

  for i := 0 to Length(ANodes) - 1 do
  begin
    NFSe.CondicaoPagamento.Parcelas.New;

    with NFSe.CondicaoPagamento.Parcelas[i] do
    begin
      Parcela := ObterConteudo(ANodes[i].Childrens.FindAnyNs('numfatura'), tcStr);
      DataVencimento := ObterConteudo(ANodes[i].Childrens.FindAnyNs('vencimentofatura'), tcDatVcto);
      Valor := ObterConteudo(ANodes[i].Childrens.FindAnyNs('valorfatura'), tcDe2);
    end;
  end;
end;

procedure TNFSeR_SmarAPD.LerItens(const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  i: Integer;
  aValor: string;
begin
  ANodes := ANode.Childrens.FindAllAnyNs('ITENS');

  NFSe.Servico.ItemServico.Clear;
  NFSe.Servico.Discriminacao := '';
  NFSe.Servico.Valores.ValorServicos := 0;

  for i := 0 to Length(ANodes) - 1 do
  begin
    NFSe.Servico.ItemServico.New;

    with NFSe.Servico.ItemServico[i] do
    begin
      Quantidade := ObterConteudo(ANodes[i].Childrens.FindAnyNs('Quantidade'), tcDe2);
      ItemListaServico := ObterConteudo(ANodes[i].Childrens.FindAnyNs('CodigoAtividade'), tcStr);
      Descricao := ObterConteudo(ANodes[i].Childrens.FindAnyNs('Servico'), tcStr);
      Descricao := StringReplace(Descricao, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);

      if NFSe.Servico.Discriminacao <> '' then
        NFSe.Servico.Discriminacao := NFSe.Servico.Discriminacao + ';';

      NFSe.Servico.Discriminacao := NFSe.Servico.Discriminacao + Descricao;

      ValorUnitario := ObterConteudo(ANodes[i].Childrens.FindAnyNs('ValorUnitario'), tcDe2);
      ValorTotal    := ObterConteudo(ANodes[i].Childrens.FindAnyNs('ValorTotal'), tcDe2);
      Aliquota      := ObterConteudo(ANodes[i].Childrens.FindAnyNs('Aliquota'), tcDe2);
      Tributavel    := snSim;

      aValor := ObterConteudo(ANodes[i].Childrens.FindAnyNs('ImpostoRetido'), tcStr);

      if aValor = 'true' then
      begin
        NFSe.Servico.Valores.IssRetido := stRetencao;
        NFSe.Servico.Valores.ValorIssRetido := ObterConteudo(ANode.Childrens.FindAnyNs('ISSQNCliente'), tcDe2);
      end
      else
        NFSe.Servico.Valores.IssRetido := stNormal;

      NFSe.Servico.Valores.Aliquota := Aliquota;

      NFSe.Servico.Valores.ValorServicos := (NFSe.Servico.Valores.ValorServicos +
                                                                  ValorTotal);

      NFSe.Servico.Discriminacao := StringReplace(NFSe.Servico.Discriminacao,
                     FpQuebradeLinha, sLineBreak, [rfReplaceAll, rfIgnoreCase]);
    end;
  end;
end;

procedure TNFSeR_SmarAPD.LerServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('tbservico');

  if AuxNode = nil then Exit;

  LerServicos(AuxNode);
end;

procedure TNFSeR_SmarAPD.LerServicos(const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  i: Integer;
  aValor: string;
begin
  ANodes := ANode.Childrens.FindAllAnyNs('servico');

  NFSe.Servico.ItemServico.Clear;

  for i := 0 to Length(ANodes) - 1 do
  begin
    NFSe.Servico.ItemServico.New;

    with NFSe.Servico.ItemServico[i] do
    begin
      Quantidade := ObterConteudo(ANodes[i].Childrens.FindAnyNs('quantidade'), tcDe2);
      Descricao := ObterConteudo(ANodes[i].Childrens.FindAnyNs('descricao'), tcStr);
      Descricao := StringReplace(Descricao, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);
      CodServ := ObterConteudo(ANodes[i].Childrens.FindAnyNs('codatividade'), tcStr);
      ValorUnitario := ObterConteudo(ANodes[i].Childrens.FindAnyNs('valorunitario'), tcDe2);
      Aliquota := ObterConteudo(ANodes[i].Childrens.FindAnyNs('aliquota'), tcDe2);
      aValor := ObterConteudo(ANodes[i].Childrens.FindAnyNs('impostoretido'), tcStr);

      if aValor = 'True' then
        NFSe.Servico.Valores.IssRetido := stRetencao
      else
        NFSe.Servico.Valores.IssRetido := stNormal;
    end;
  end;
end;

function TNFSeR_SmarAPD.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  FpQuebradeLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  Arquivo := NormatizarXml(Arquivo);

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if (Pos('nfdok', Arquivo) > 0) then
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

function TNFSeR_SmarAPD.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  aValor: string;
  Ok: Boolean;
begin
  Result := True;
  NFSe.SituacaoNfse := snNormal;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('nfdok');

  if AuxNode <> nil then
    AuxNode := AuxNode.Childrens.FindAnyNs('NewDataSet');

  if AuxNode <> nil then
    AuxNode := AuxNode.Childrens.FindAnyNs('NOTA_FISCAL');

  if AuxNode = nil then Exit;

  with NFSe do
  begin
    Numero            := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumeroNota'), tcStr);
    CodigoVerificacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('ChaveValidacao'), tcStr);
    DataEmissaoRps    := ObterConteudo(AuxNode.Childrens.FindAnyNs('DataEmissao'), tcDat);
    Competencia       := DataEmissaoRps;
    DataEmissao       := DataEmissaoRps;
    dhRecebimento     := DataEmissaoRps;

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('NaturezaOperacao'), tcStr);
    NaturezaOperacao := StrToNaturezaOperacao(Ok, aValor);

    OutrasInformacoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('Observacao'), tcStr);
    OutrasInformacoes := StringReplace(OutrasInformacoes, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);

    MotivoCancelamento := '';
    Intermediario.Identificacao.CpfCnpj := '';

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('SituacaoNf'), tcStr);

    if aValor = 'Cancelada' then
    begin
      NFSe.SituacaoNfse := snCancelado;
      NfseCancelamento.DataHora := DataEmissao;
    end;

    IdentificacaoRps.Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumeroRps'), tcStr);
    IdentificacaoRps.Tipo   := trRPS;
    InfID.ID                := OnlyNumber(NFSe.Numero);

    with Prestador do
    begin
      RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('TimbreContribuinteLinha1'), tcStr);

      with Endereco do
      begin
        aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('TimbreContribuinteLinha2'), tcStr);
        Endereco := Trim(copy(aValor, 1, pos(',', aValor) -1));
        Numero   := Trim(copy(aValor, pos(',', aValor) +1,
                                     (pos('-', aValor) - pos(',', aValor)) -1));
        Bairro   := Trim(copy(aValor, pos('-', aValor) +1, length(aValor) -1));
        Complemento := '';
        aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('TimbreContribuinteLinha3'), tcStr);
        aValor := Trim(copy(aValor, pos('-', aValor) +1, length(aValor) -1));
        xMunicipio := Trim(copy(aValor, 1, pos('-', aValor) -1));
        UF         := Trim(copy(aValor, pos('-', aValor) +1, length(aValor) -1));

        with Contato do
        begin
          Email    := '';
          Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('TelefoneTomador'), tcStr);
        end;
      end;

      aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('TimbreContribuinteLinha4'), tcStr);

      with IdentificacaoPrestador do
      begin
        InscricaoMunicipal := Trim(copy(aValor, 22, (pos('CPF/CNPJ:', aValor) -23)));
        CpfCnpj := Trim(copy(aValor, pos('CPF/CNPJ:', aValor) +10,
                                                            length(aValor) -1));
      end;
    end;

    with Tomador do
    begin
      RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('ClienteNomeRazaoSocial'), tcStr);

      with IdentificacaoTomador do
      begin
       InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('ClienteInscricaoMunicipal'), tcStr);
       CpfCnpj            := ObterConteudo(AuxNode.Childrens.FindAnyNs('ClienteCNPJCPF'), tcStr);
      end;

      with Endereco do
      begin
        TipoLogradouro  := '';
        Endereco        := ObterConteudo(AuxNode.Childrens.FindAnyNs('ClienteEndereco'), tcStr);
        Numero          := ObterConteudo(AuxNode.Childrens.FindAnyNs('ClienteNumeroLogradouro'), tcStr);
        Complemento     := '';
        TipoBairro      := '';
        Bairro          := ObterConteudo(AuxNode.Childrens.FindAnyNs('ClienteBairro'), tcStr);
        CodigoMunicipio := '0';
        xMunicipio      := ObterConteudo(AuxNode.Childrens.FindAnyNs('ClienteCidade'), tcStr);
        UF              := ObterConteudo(AuxNode.Childrens.FindAnyNs('ClienteUF'), tcStr);
        CEP             := ObterConteudo(AuxNode.Childrens.FindAnyNs('ClienteCEP'), tcStr);
      end;

      with Contato do
      begin
        Email    := ObterConteudo(AuxNode.Childrens.FindAnyNs('ClienteEmail'), tcStr);
        Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('ClienteFone'), tcStr);
      end;
    end;

    with Servico do
    begin
      CodigoCnae := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoAtividade'), tcStr);
      CodigoMunicipio := '';

      with Valores do
      begin
        Aliquota := ObterConteudo(AuxNode.Childrens.FindAnyNs('Aliquota'), tcDe3);

        aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('ImpostoRetido'), tcStr);

        if aValor = 'true' then
        begin
          IssRetido := stRetencao;
          ValorIssRetido := ObterConteudo(AuxNode.Childrens.FindAnyNs('ISSQNCliente'), tcDe2);
        end
        else
          IssRetido := stNormal;

        ValorPis       := ObterConteudo(AuxNode.Childrens.FindAnyNs('Pis'), tcDe2);
        ValorCofins    := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cofins'), tcDe2);
        ValorInss      := ObterConteudo(AuxNode.Childrens.FindAnyNs('Inss'), tcDe2);
        ValorIr        := ObterConteudo(AuxNode.Childrens.FindAnyNs('Irrf'), tcDe2);
        ValorCsll      := ObterConteudo(AuxNode.Childrens.FindAnyNs('Csll'), tcDe2);
        AliquotaPIS    := 0;
        AliquotaCOFINS := 0;
        AliquotaINSS   := 0;
        AliquotaIR     := 0;
        AliquotaCSLL   := 0;
      end;

      Discriminacao             := '';
      CodigoTributacaoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoAtividade'), tcStr);
    end;

    LerItens(AuxNode);

    with Servico.Valores do
    begin
      ValorIss         := (ValorServicos * Aliquota) /100;
      ValorLiquidoNfse := ValorServicos -
                                      (ValorDeducoes + DescontoCondicionado +
                                       DescontoIncondicionado + ValorIssRetido +
                                       ValorPis + ValorCofins + ValorInss +
                                       ValorIr + ValorCsll);
      BaseCalculo      := ValorLiquidoNfse;
    end;
  end;

  LerCampoLink;
end;

function TNFSeR_SmarAPD.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  aValor: string;
begin
  Result := True;

  with NFSe do
  begin
    with IdentificacaoRps do
    begin
      Serie := ObterConteudo(ANode.Childrens.FindAnyNs('codseriedocumento'), tcStr);
      Numero := ObterConteudo(ANode.Childrens.FindAnyNs('numerort'), tcStr);
    end;

    DataEmissaoRps := ObterConteudo(ANode.Childrens.FindAnyNs('dataemissaort'), tcDatVcto);
    Competencia := ObterConteudo(ANode.Childrens.FindAnyNs('fatorgerador'), tcDatVcto);
    NaturezaOperacao := ObterConteudo(ANode.Childrens.FindAnyNs('codnaturezaoperacao'), tcStr);

    with Prestador do
    begin
      with IdentificacaoPrestador do
      begin
        InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('inscricaomunicipalemissor'), tcStr);
      end;
    end;

    DataEmissao := ObterConteudo(ANode.Childrens.FindAnyNs('dataemissao'), tcDatVcto);

    with Tomador do
    begin
      RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('razaotomador'), tcStr);
      NomeFantasia := ObterConteudo(ANode.Childrens.FindAnyNs('nomefantasiatomador'), tcStr);

      with IdentificacaoTomador do
      begin
        CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('cpfcnpjtomador'), tcStr);
        InscricaoEstadual := ObterConteudo(ANode.Childrens.FindAnyNs('inscricaoestadualtomador'), tcStr);
        InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('inscricaomunicipaltomador'), tcStr);
      end;

      with Endereco do
      begin
        aValor := ObterConteudo(ANode.Childrens.FindAnyNs('tppessoa'), tcStr);

        if aValor = 'O' then
          CodigoMunicipio := '9999999'
        else
          CodigoMunicipio := '0'; // precisa implementar uma função que retorna o
                                  // codigo da cidade informando o nome dela.

        Endereco := ObterConteudo(ANode.Childrens.FindAnyNs('enderecotomador'), tcStr);
        Numero := ObterConteudo(ANode.Childrens.FindAnyNs('numeroendereco'), tcStr);
        xMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('cidadetomador'), tcStr);
        UF := ObterConteudo(ANode.Childrens.FindAnyNs('estadotomador'), tcStr);
        xPais := ObterConteudo(ANode.Childrens.FindAnyNs('paistomador'), tcStr);
        CEP := ObterConteudo(ANode.Childrens.FindAnyNs('ceptomador'), tcStr);
        Bairro := ObterConteudo(ANode.Childrens.FindAnyNs('bairrotomador'), tcStr);
      end;

      with Contato do
      begin
        Telefone := ObterConteudo(ANode.Childrens.FindAnyNs('fonetomador'), tcStr);
        Email := ObterConteudo(ANode.Childrens.FindAnyNs('emailtomador'), tcStr);
      end;
    end;

    OutrasInformacoes := ObterConteudo(ANode.Childrens.FindAnyNs('observacao'), tcStr);
    OutrasInformacoes := StringReplace(OutrasInformacoes, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);

    LerFatura(ANode);
    LerServico(ANode);

    with Transportadora do
    begin
      xNomeTrans := ObterConteudo(ANode.Childrens.FindAnyNs('razaotransportadora'), tcStr);
      xCpfCnpjTrans := ObterConteudo(ANode.Childrens.FindAnyNs('cpfcnpjtransportadora'), tcStr);
      xEndTrans := ObterConteudo(ANode.Childrens.FindAnyNs('enderecotransportadora'), tcStr);
    end;

    with Servico do
    begin
      with Valores do
      begin
        ValorPis := ObterConteudo(ANode.Childrens.FindAnyNs('pis'), tcDe2);
        ValorCofins := ObterConteudo(ANode.Childrens.FindAnyNs('cofins'), tcDe2);
        ValorCsll := ObterConteudo(ANode.Childrens.FindAnyNs('csll'), tcDe2);
        ValorIr := ObterConteudo(ANode.Childrens.FindAnyNs('irrf'), tcDe2);
        ValorInss := ObterConteudo(ANode.Childrens.FindAnyNs('inss'), tcDe2);
        JustificativaDeducao := ObterConteudo(ANode.Childrens.FindAnyNs('descdeducoesconstrucao'), tcStr);
        ValorDeducoes := ObterConteudo(ANode.Childrens.FindAnyNs('totaldeducoesconstrucao'), tcDe2);
      end;
    end;
  end;
end;

end.
