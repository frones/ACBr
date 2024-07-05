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
      xItemListaServico := ItemListaServicoDescricao(ItemListaServico);
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

      ValorTotal := Quantidade * ValorUnitario;
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

  LerParamsTabIni(True);

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

  if NFSe.Tomador.RazaoSocial = '' then
    NFSe.Tomador.RazaoSocial := 'Tomador Não Identificado';

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

  if not Assigned(ANode) then Exit;

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

    Prestador.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('TimbreContribuinteLinha1'), tcStr);

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('TimbreContribuinteLinha2'), tcStr);
    Prestador.Endereco.Endereco := Trim(copy(aValor, 1, pos(',', aValor) -1));
    Prestador.Endereco.Numero   := Trim(copy(aValor, pos(',', aValor) +1,
                                 (pos('-', aValor) - pos(',', aValor)) -1));
    Prestador.Endereco.Bairro   := Trim(copy(aValor, pos('-', aValor) +1, length(aValor) -1));
    Prestador.Endereco.Complemento := '';
    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('TimbreContribuinteLinha3'), tcStr);
    aValor := Trim(copy(aValor, pos('-', aValor) +1, length(aValor) -1));
    Prestador.Endereco.xMunicipio := Trim(copy(aValor, 1, pos('-', aValor) -1));
    Prestador.Endereco.UF         := Trim(copy(aValor, pos('-', aValor) +1, length(aValor) -1));

    Prestador.Contato.Email    := '';
    Prestador.Contato.Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('TelefoneTomador'), tcStr);

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('TimbreContribuinteLinha4'), tcStr);

    Prestador.IdentificacaoPrestador.InscricaoMunicipal := Trim(copy(aValor, 22, (pos('CPF/CNPJ:', aValor) -23)));
    Prestador.IdentificacaoPrestador.CpfCnpj := Trim(copy(aValor, pos('CPF/CNPJ:', aValor) +10,
                                                          length(aValor) -1));

    Tomador.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('ClienteNomeRazaoSocial'), tcStr);

    Tomador.IdentificacaoTomador.InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('ClienteInscricaoMunicipal'), tcStr);
    Tomador.IdentificacaoTomador.CpfCnpj            := ObterConteudo(AuxNode.Childrens.FindAnyNs('ClienteCNPJCPF'), tcStr);

    Tomador.Endereco.TipoLogradouro  := '';
    Tomador.Endereco.Endereco        := ObterConteudo(AuxNode.Childrens.FindAnyNs('ClienteEndereco'), tcStr);
    Tomador.Endereco.Numero          := ObterConteudo(AuxNode.Childrens.FindAnyNs('ClienteNumeroLogradouro'), tcStr);
    Tomador.Endereco.Complemento     := '';
    Tomador.Endereco.TipoBairro      := '';
    Tomador.Endereco.Bairro          := ObterConteudo(AuxNode.Childrens.FindAnyNs('ClienteBairro'), tcStr);
    Tomador.Endereco.CodigoMunicipio := '0';
    Tomador.Endereco.xMunicipio      := ObterConteudo(AuxNode.Childrens.FindAnyNs('ClienteCidade'), tcStr);
    Tomador.Endereco.UF              := ObterConteudo(AuxNode.Childrens.FindAnyNs('ClienteUF'), tcStr);
    Tomador.Endereco.CEP             := ObterConteudo(AuxNode.Childrens.FindAnyNs('ClienteCEP'), tcStr);

    Tomador.Contato.Email    := ObterConteudo(AuxNode.Childrens.FindAnyNs('ClienteEmail'), tcStr);
    Tomador.Contato.Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('ClienteFone'), tcStr);

    Servico.CodigoCnae := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoAtividade'), tcStr);
    Servico.CodigoMunicipio := '';

    Servico.Valores.Aliquota := ObterConteudo(AuxNode.Childrens.FindAnyNs('Aliquota'), tcDe3);

    aValor := ObterConteudo(AuxNode.Childrens.FindAnyNs('ImpostoRetido'), tcStr);

    if aValor = 'true' then
    begin
      Servico.Valores.IssRetido := stRetencao;
      Servico.Valores.ValorIssRetido := ObterConteudo(AuxNode.Childrens.FindAnyNs('ISSQNCliente'), tcDe2);
    end
    else
      Servico.Valores.IssRetido := stNormal;

    Servico.Valores.ValorPis       := ObterConteudo(AuxNode.Childrens.FindAnyNs('Pis'), tcDe2);
    Servico.Valores.ValorCofins    := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cofins'), tcDe2);
    Servico.Valores.ValorInss      := ObterConteudo(AuxNode.Childrens.FindAnyNs('Inss'), tcDe2);
    Servico.Valores.ValorIr        := ObterConteudo(AuxNode.Childrens.FindAnyNs('Irrf'), tcDe2);
    Servico.Valores.ValorCsll      := ObterConteudo(AuxNode.Childrens.FindAnyNs('Csll'), tcDe2);
    Servico.Valores.AliquotaPIS    := 0;
    Servico.Valores.AliquotaCOFINS := 0;
    Servico.Valores.AliquotaINSS   := 0;
    Servico.Valores.AliquotaIR     := 0;
    Servico.Valores.AliquotaCSLL   := 0;

    Servico.Discriminacao             := '';
    Servico.CodigoTributacaoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoAtividade'), tcStr);

    LerItens(AuxNode);

    Servico.Valores.ValorIss := (Servico.Valores.ValorServicos * Servico.Valores.Aliquota) /100;

    Servico.Valores.RetencoesFederais := Servico.Valores.ValorPis +
      Servico.Valores.ValorCofins + Servico.Valores.ValorInss +
      Servico.Valores.ValorIr + Servico.Valores.ValorCsll;

    Servico.Valores.ValorLiquidoNfse := Servico.Valores.ValorServicos -
      (Servico.Valores.ValorDeducoes + Servico.Valores.DescontoCondicionado +
       Servico.Valores.DescontoIncondicionado + Servico.Valores.ValorIssRetido +
       Servico.Valores.RetencoesFederais);

    Servico.Valores.BaseCalculo := Servico.Valores.ValorLiquidoNfse;

    Servico.Valores.ValorTotalNotaFiscal := Servico.Valores.ValorServicos -
      Servico.Valores.DescontoCondicionado -
      Servico.Valores.DescontoIncondicionado;
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
    IdentificacaoRps.Serie := ObterConteudo(ANode.Childrens.FindAnyNs('codseriedocumento'), tcStr);
    IdentificacaoRps.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('numerort'), tcStr);

    DataEmissaoRps := ObterConteudo(ANode.Childrens.FindAnyNs('dataemissaort'), tcDatVcto);
    Competencia := ObterConteudo(ANode.Childrens.FindAnyNs('fatorgerador'), tcDatVcto);
    NaturezaOperacao := ObterConteudo(ANode.Childrens.FindAnyNs('codnaturezaoperacao'), tcStr);

    Prestador.IdentificacaoPrestador.InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('inscricaomunicipalemissor'), tcStr);

    DataEmissao := ObterConteudo(ANode.Childrens.FindAnyNs('dataemissao'), tcDatVcto);

    Tomador.RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('razaotomador'), tcStr);
    Tomador.NomeFantasia := ObterConteudo(ANode.Childrens.FindAnyNs('nomefantasiatomador'), tcStr);

    Tomador.IdentificacaoTomador.CpfCnpj := ObterConteudo(ANode.Childrens.FindAnyNs('cpfcnpjtomador'), tcStr);
    Tomador.IdentificacaoTomador.InscricaoEstadual := ObterConteudo(ANode.Childrens.FindAnyNs('inscricaoestadualtomador'), tcStr);
    Tomador.IdentificacaoTomador.InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('inscricaomunicipaltomador'), tcStr);

    aValor := ObterConteudo(ANode.Childrens.FindAnyNs('tppessoa'), tcStr);

    if aValor = 'O' then
      Tomador.Endereco.CodigoMunicipio := '9999999'
    else
      Tomador.Endereco.CodigoMunicipio := '0'; // precisa implementar uma função que retorna o
                              // codigo da cidade informando o nome dela.

    Tomador.Endereco.Endereco := ObterConteudo(ANode.Childrens.FindAnyNs('enderecotomador'), tcStr);
    Tomador.Endereco.Numero := ObterConteudo(ANode.Childrens.FindAnyNs('numeroendereco'), tcStr);
    Tomador.Endereco.xMunicipio := ObterConteudo(ANode.Childrens.FindAnyNs('cidadetomador'), tcStr);
    Tomador.Endereco.UF := ObterConteudo(ANode.Childrens.FindAnyNs('estadotomador'), tcStr);
    Tomador.Endereco.xPais := ObterConteudo(ANode.Childrens.FindAnyNs('paistomador'), tcStr);
    Tomador.Endereco.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('ceptomador'), tcStr);
    Tomador.Endereco.Bairro := ObterConteudo(ANode.Childrens.FindAnyNs('bairrotomador'), tcStr);

    Tomador.Contato.Telefone := ObterConteudo(ANode.Childrens.FindAnyNs('fonetomador'), tcStr);
    Tomador.Contato.Email := ObterConteudo(ANode.Childrens.FindAnyNs('emailtomador'), tcStr);

    OutrasInformacoes := ObterConteudo(ANode.Childrens.FindAnyNs('observacao'), tcStr);
    OutrasInformacoes := StringReplace(OutrasInformacoes, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);

    LerFatura(ANode);
    LerServico(ANode);

    Transportadora.xNomeTrans := ObterConteudo(ANode.Childrens.FindAnyNs('razaotransportadora'), tcStr);
    Transportadora.xCpfCnpjTrans := ObterConteudo(ANode.Childrens.FindAnyNs('cpfcnpjtransportadora'), tcStr);
    Transportadora.xEndTrans := ObterConteudo(ANode.Childrens.FindAnyNs('enderecotransportadora'), tcStr);

    Servico.Valores.ValorPis := ObterConteudo(ANode.Childrens.FindAnyNs('pis'), tcDe2);
    Servico.Valores.ValorCofins := ObterConteudo(ANode.Childrens.FindAnyNs('cofins'), tcDe2);
    Servico.Valores.ValorCsll := ObterConteudo(ANode.Childrens.FindAnyNs('csll'), tcDe2);
    Servico.Valores.ValorIr := ObterConteudo(ANode.Childrens.FindAnyNs('irrf'), tcDe2);
    Servico.Valores.ValorInss := ObterConteudo(ANode.Childrens.FindAnyNs('inss'), tcDe2);
    Servico.Valores.JustificativaDeducao := ObterConteudo(ANode.Childrens.FindAnyNs('descdeducoesconstrucao'), tcStr);
    Servico.Valores.ValorDeducoes := ObterConteudo(ANode.Childrens.FindAnyNs('totaldeducoesconstrucao'), tcDe2);
  end;
end;

end.
