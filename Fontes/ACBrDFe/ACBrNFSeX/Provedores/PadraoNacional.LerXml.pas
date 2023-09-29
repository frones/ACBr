{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

unit PadraoNacional.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { TNFSeR_PadraoNacional }

  TNFSeR_PadraoNacional = class(TNFSeRClass)
  private
    FpLerPrestador: Boolean;
  protected
    procedure LerinfNFSe(const ANode: TACBrXmlNode);
    procedure LerEmitente(const ANode: TACBrXmlNode);
    procedure LerEnderecoEmitente(const ANode: TACBrXmlNode);

    procedure LerValoresNFSe(const ANode: TACBrXmlNode);
    procedure LerDPS(const ANode: TACBrXmlNode);

    procedure LerinfDPS(const ANode: TACBrXmlNode);
    procedure LerSubstituicao(const ANode: TACBrXmlNode);

    procedure LerPrestador(const ANode: TACBrXmlNode);
    procedure LerEnderecoPrestador(const ANode: TACBrXmlNode);
    procedure LerRegimeTributacaoPrestador(const ANode: TACBrXmlNode);
    procedure LerEnderecoNacionalPrestador(const ANode: TACBrXmlNode);
    procedure LerEnderecoExteriorPrestador(const ANode: TACBrXmlNode);

    procedure LerTomador(const ANode: TACBrXmlNode);
    procedure LerEnderecoTomador(const ANode: TACBrXmlNode);
    procedure LerEnderecoNacionalTomador(const ANode: TACBrXmlNode);
    procedure LerEnderecoExteriorTomador(const ANode: TACBrXmlNode);

    procedure LerIntermediario(const ANode: TACBrXmlNode);
    procedure LerEnderecoItermediario(const ANode: TACBrXmlNode);
    procedure LerEnderecoNacionalIntermediario(const ANode: TACBrXmlNode);
    procedure LerEnderecoExteriorIntermediario(const ANode: TACBrXmlNode);

    procedure LerServico(const ANode: TACBrXmlNode);
    procedure LerLocalPrestacao(const ANode: TACBrXmlNode);
    procedure LerCodigoServico(const ANode: TACBrXmlNode);
    procedure LerComercioExterior(const ANode: TACBrXmlNode);
    procedure LerLocacaoSubLocacao(const ANode: TACBrXmlNode);
    procedure LerObra(const ANode: TACBrXmlNode);
    procedure LerEnderecoObra(const ANode: TACBrXmlNode);
    procedure LerEnderecoExteriorObra(const ANode: TACBrXmlNode);
    procedure LerAtividadeEvento(const ANode: TACBrXmlNode);
    procedure LerEnderecoEvento(const ANode: TACBrXmlNode);
    procedure LerEnderecoExteriorEvento(const ANode: TACBrXmlNode);
    procedure LerExploracaoRodoviaria(const ANode: TACBrXmlNode);
    procedure LerInformacoesComplementares(const ANode: TACBrXmlNode);

    procedure LerValores(const ANode: TACBrXmlNode);
    procedure LerServicoPrestado(const ANode: TACBrXmlNode);
    procedure LerDescontos(const ANode: TACBrXmlNode);
    procedure LerDeducoes(const ANode: TACBrXmlNode);
    procedure LerDocDeducoes(const ANode: TACBrXmlNode);
    procedure LerNFSeMunicipio(const ANode: TACBrXmlNode; Item: Integer);
    procedure LerNFNFS(const ANode: TACBrXmlNode; Item: Integer);
    procedure LerFornecedor(const ANode: TACBrXmlNode; Item: Integer);
    procedure LerEnderecoFornecedor(const ANode: TACBrXmlNode; Item: Integer);
    procedure LerEnderecoNacionalFornecedor(const ANode: TACBrXmlNode; Item: Integer);
    procedure LerEnderecoExteriorFornecedr(const ANode: TACBrXmlNode; Item: Integer);

    procedure LerTributacao(const ANode: TACBrXmlNode);
    procedure LerTributacaoMunicipal(const ANode: TACBrXmlNode);
    procedure LerBeneficioMunicipal(const ANode: TACBrXmlNode);
    procedure LerExigibilidadeSuspensa(const ANode: TACBrXmlNode);
    procedure LerTributacaoNacional(const ANode: TACBrXmlNode);
    procedure LerTributacaoOutrosPisCofins(const ANode: TACBrXmlNode);
    procedure LerTotalTributos(const ANode: TACBrXmlNode);
    procedure LerValorTotalTributos(const ANode: TACBrXmlNode);
    procedure LerPercentualTotalTributos(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.XMLHTML, ACBrUtil.DateTime, ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     PadraoNacional
//==============================================================================

{ TNFSeR_PadraoNacional }

procedure TNFSeR_PadraoNacional.LerAtividadeEvento(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('atvEvento');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Evento do
    begin
      desc := ObterConteudo(AuxNode.Childrens.FindAnyNs('desc'), tcStr);
      dtIni := ObterConteudo(AuxNode.Childrens.FindAnyNs('dtIni'), tcDat);
      dtFim := ObterConteudo(AuxNode.Childrens.FindAnyNs('dtFim'), tcDat);
      id := ObterConteudo(AuxNode.Childrens.FindAnyNs('id'), tcStr);

      LerEnderecoEvento(AuxNode);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerBeneficioMunicipal(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('BM');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores.tribMun do
    begin
      tpBM := StrTotpBM(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tpBM'), tcStr));
      nBM := ObterConteudo(AuxNode.Childrens.FindAnyNs('nBM'), tcStr);
      vRedBCBM := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRedBCBM'), tcDe2);
      pRedBCBM := ObterConteudo(AuxNode.Childrens.FindAnyNs('pRedBCBM'), tcDe2);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerCodigoServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('cServ');

  if AuxNode <> nil then
  begin
    with NFSe.Servico do
    begin
      ItemListaServico := ObterConteudo(AuxNode.Childrens.FindAnyNs('cTribNac'), tcStr);
      xItemListaServico := ItemListaServicoDescricao(ItemListaServico);
      CodigoTributacaoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('cTribMun'), tcStr);
      Discriminacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('xDescServ'), tcStr);
      Discriminacao := StringReplace(Discriminacao, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);
      CodigoNBS := ObterConteudo(AuxNode.Childrens.FindAnyNs('cNBS'), tcStr);
      CodigoInterContr := ObterConteudo(AuxNode.Childrens.FindAnyNs('cIntContrib'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerComercioExterior(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('comExt');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.comExt do
    begin
      mdPrestacao := StrTomdPrestacao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('mdPrestacao'), tcStr));
      vincPrest := StrTovincPrest(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('vincPrest'), tcStr));
      tpMoeda := ObterConteudo(AuxNode.Childrens.FindAnyNs('tpMoeda'), tcInt);
      vServMoeda := ObterConteudo(AuxNode.Childrens.FindAnyNs('vServMoeda'), tcDe2);
      mecAFComexP := StrTomecAFComexP(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('mecAFComexP'), tcStr));
      mecAFComexT := StrTomecAFComexT(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('mecAFComexT'), tcStr));
      movTempBens := StrTomovTempBens(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('movTempBens'), tcStr));
      nDI := ObterConteudo(AuxNode.Childrens.FindAnyNs('nDI'), tcStr);
      nRE := ObterConteudo(AuxNode.Childrens.FindAnyNs('nRE'), tcStr);
      mdic := ObterConteudo(AuxNode.Childrens.FindAnyNs('mdic'), tcInt);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerDeducoes(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('vDedRed');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      AliquotaDeducoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('pDR'), tcDe2);
      ValorDeducoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('vDR'), tcDe2);

      LerDocDeducoes(AuxNode);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerDescontos(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('vDescCondIncond');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      DescontoIncondicionado := ObterConteudo(AuxNode.Childrens.FindAnyNs('vDescIncond'), tcDe2);
      DescontoCondicionado := ObterConteudo(AuxNode.Childrens.FindAnyNs('vDescCond'), tcDe2);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerDocDeducoes(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('documentos');

  if AuxNode <> nil then
  begin
    ANodes := AuxNode.Childrens.FindAllAnyNs('docDedRed');

    for i := 0 to Length(ANodes) - 1 do
    begin
      NFSe.Servico.Valores.DocDeducao.New;
      with NFSe.Servico.Valores.DocDeducao[i] do
      begin
        chNFSe := ObterConteudo(ANodes[i].Childrens.FindAnyNs('chNFSe'), tcStr);
        chNFe := ObterConteudo(ANodes[i].Childrens.FindAnyNs('chNFe'), tcStr);

        LerNFSeMunicipio(ANodes[i].Childrens.FindAnyNs('NFSeMun'), I);
        LerNFNFS(ANodes[i].Childrens.FindAnyNs('NFNFS'), I);

        nDocFisc := ObterConteudo(ANodes[i].Childrens.FindAnyNs('nDocFisc'), tcStr);
        nDoc := ObterConteudo(ANodes[i].Childrens.FindAnyNs('nDoc'), tcStr);
        tpDedRed := StrTotpDedRed(Ok, ObterConteudo(ANodes[i].Childrens.FindAnyNs('tpDedRed'), tcStr));
        xDescOutDed := ObterConteudo(ANodes[i].Childrens.FindAnyNs('xDescOutDed'), tcStr);
        dtEmiDoc := ObterConteudo(ANodes[i].Childrens.FindAnyNs('dtEmiDoc'), tcDat);
        vDedutivelRedutivel := ObterConteudo(ANodes[i].Childrens.FindAnyNs('vDedutivelRedutivel'), tcDe2);
        vDeducaoReducao := ObterConteudo(ANodes[i].Childrens.FindAnyNs('vDeducaoReducao'), tcDe2);

        LerFornecedor(ANodes[i].Childrens.FindAnyNs('fornec'), I);
      end;
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerDPS(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('DPS');

  if AuxNode <> nil then
    LerinfDPS(AuxNode);
end;

procedure TNFSeR_PadraoNacional.LerEmitente(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('emit');

  if AuxNode <> nil then
  begin
    with NFSe.infNFSe.emit do
    begin
      with Identificacao do
      begin
        CpfCnpj := ObterCNPJCPF(AuxNode);

        InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('IM'), tcStr);
      end;

      RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('xNome'), tcStr);
      NomeFantasia := ObterConteudo(AuxNode.Childrens.FindAnyNs('xFant'), tcStr);

      LerEnderecoEmitente(AuxNode);

      Contato.Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('fone'), tcStr);
      Contato.Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('email'), tcStr);
    end;

    with NFSe.Prestador do
    begin
      with IdentificacaoPrestador do
      begin
        CpfCnpj := NFSe.infNFSe.emit.Identificacao.CpfCnpj;
        InscricaoMunicipal := NFSe.infNFSe.emit.Identificacao.InscricaoMunicipal;
      end;

      RazaoSocial := NFSe.infNFSe.emit.RazaoSocial;
      NomeFantasia := NFSe.infNFSe.emit.NomeFantasia;

      with Endereco do
      begin
        Endereco := NFSe.infNFSe.emit.Endereco.Endereco;
        Numero := NFSe.infNFSe.emit.Endereco.Numero;
        Complemento := NFSe.infNFSe.emit.Endereco.Complemento;
        Bairro := NFSe.infNFSe.emit.Endereco.Bairro;
        UF := NFSe.infNFSe.emit.Endereco.UF;
        CEP := NFSe.infNFSe.emit.Endereco.CEP;
      end;

      with Contato do
      begin
        Telefone := NFSe.infNFSe.emit.Contato.Telefone;
        Email := NFSe.infNFSe.emit.Contato.Email;
      end;
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerEnderecoEmitente(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('enderNac');

  if AuxNode <> nil then
  begin
    with NFSe.infNFSe.emit.Endereco do
    begin
      Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLgr'), tcStr);
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('nro'), tcStr);
      Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCpl'), tcStr);
      Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('xBairro'), tcStr);
      UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('UF'), tcStr);
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerEnderecoEvento(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('end');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Evento.Endereco do
    begin
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcStr);

      LerEnderecoExteriorEvento(AuxNode);

      Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLgr'), tcStr);
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('nro'), tcStr);
      Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCpl'), tcStr);
      Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('xBairro'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerEnderecoExteriorEvento(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('endExt');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Evento.Endereco do
    begin
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('cEndPost'), tcStr);
      xMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCidade'), tcStr);
      UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('xEstProvReg'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerEnderecoExteriorFornecedr(
  const ANode: TACBrXmlNode; Item: Integer);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('endExt');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores.DocDeducao.Items[Item].fornec.Endereco do
    begin
      CodigoPais := SiglaISO2ToCodIBGEPais(ObterConteudo(AuxNode.Childrens.FindAnyNs('cPais'), tcStr));
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('cEndPost'), tcStr);
      xMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCidade'), tcStr);
      UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('xEstProvReg'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerEnderecoExteriorIntermediario(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('endExt');

  if AuxNode <> nil then
  begin
    with NFSe.Intermediario.Endereco do
    begin
      CodigoPais := SiglaISO2ToCodIBGEPais(ObterConteudo(AuxNode.Childrens.FindAnyNs('cPais'), tcStr));
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('cEndPost'), tcStr);
      xMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCidade'), tcStr);
      UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('xEstProvReg'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerEnderecoExteriorObra(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('endExt');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Endereco do
    begin
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('cEndPost'), tcStr);
      xMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCidade'), tcStr);
      UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('xEstProvReg'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerEnderecoExteriorPrestador(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('endExt');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Endereco do
    begin
      CodigoPais := SiglaISO2ToCodIBGEPais(ObterConteudo(AuxNode.Childrens.FindAnyNs('cPais'), tcStr));
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('cEndPost'), tcStr);
      xMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCidade'), tcStr);
      UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('xEstProvReg'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerEnderecoExteriorTomador(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('endExt');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      CodigoPais := SiglaISO2ToCodIBGEPais(ObterConteudo(AuxNode.Childrens.FindAnyNs('cPais'), tcStr));
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('cEndPost'), tcStr);
      xMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCidade'), tcStr);
      UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('xEstProvReg'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerEnderecoFornecedor(const ANode: TACBrXmlNode;
  Item: Integer);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('end');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores.DocDeducao.Items[item].fornec.Endereco do
    begin
      LerEnderecoNacionalFornecedor(AuxNode, Item);
      LerEnderecoExteriorFornecedr(AuxNode, Item);

      Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLgr'), tcStr);
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('nro'), tcStr);
      Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCpl'), tcStr);
      Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('xBairro'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerEnderecoItermediario(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('end');

  if AuxNode <> nil then
  begin
    with NFSe.Intermediario.Endereco do
    begin
      LerEnderecoNacionalIntermediario(AuxNode);
      LerEnderecoExteriorIntermediario(AuxNode);

      Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLgr'), tcStr);
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('nro'), tcStr);
      Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCpl'), tcStr);
      Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('xBairro'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerEnderecoNacionalFornecedor(
  const ANode: TACBrXmlNode; Item: Integer);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('endNac');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores.DocDeducao.Items[item].fornec.Endereco do
    begin
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('cMun'), tcStr);
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerEnderecoNacionalIntermediario(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  xUF: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('endNac');

  if AuxNode <> nil then
  begin
    with NFSe.Intermediario.Endereco do
    begin
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('cMun'), tcStr);
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcStr);
      xMunicipio := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);

      if UF = '' then
        UF := xUF;
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerEnderecoNacionalPrestador(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  xUF: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('endNac');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Endereco do
    begin
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('cMun'), tcStr);
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcStr);
      xMunicipio := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);

      if UF = '' then
        UF := xUF;
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerEnderecoNacionalTomador(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  xUF: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('endNac');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('cMun'), tcStr);
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcStr);
      xMunicipio := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);

      if UF = '' then
        UF := xUF;
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerEnderecoObra(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('end');

  if AuxNode <> nil then
  begin
    with NFSe.ConstrucaoCivil.Endereco do
    begin
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcStr);

      LerEnderecoExteriorObra(AuxNode);

      Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLgr'), tcStr);
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('nro'), tcStr);
      Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCpl'), tcStr);
      Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('xBairro'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerEnderecoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('end');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Endereco do
    begin
      LerEnderecoNacionalPrestador(AuxNode);
      LerEnderecoExteriorPrestador(AuxNode);

      Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLgr'), tcStr);
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('nro'), tcStr);
      Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCpl'), tcStr);
      Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('xBairro'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerEnderecoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('end');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      LerEnderecoNacionalTomador(AuxNode);
      LerEnderecoExteriorTomador(AuxNode);

      Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLgr'), tcStr);
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('nro'), tcStr);
      Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCpl'), tcStr);
      Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('xBairro'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerExigibilidadeSuspensa(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('exigSusp');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores.tribMun do
    begin
      tpSusp := StrTotpSusp(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tpSusp'), tcStr));
      nProcesso := ObterConteudo(AuxNode.Childrens.FindAnyNs('nProcesso'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerExploracaoRodoviaria(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('explRod');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.explRod do
    begin
      categVeic := StrTocategVeic(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('categVeic'), tcStr));
      nEixos := ObterConteudo(AuxNode.Childrens.FindAnyNs('nEixos'), tcInt);
      rodagem := ObterConteudo(AuxNode.Childrens.FindAnyNs('rodagem'), tcStr);
      sentido := ObterConteudo(AuxNode.Childrens.FindAnyNs('sentido'), tcStr);
      placa := ObterConteudo(AuxNode.Childrens.FindAnyNs('placa'), tcStr);
      codAcessoPed := ObterConteudo(AuxNode.Childrens.FindAnyNs('codAcessoPed'), tcStr);
      codContrato := ObterConteudo(AuxNode.Childrens.FindAnyNs('codContrato'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerFornecedor(const ANode: TACBrXmlNode;
  Item: Integer);
begin
  if ANode <> nil then
  begin
    with NFSe.Servico.Valores.DocDeducao.Items[item].fornec do
    begin
      with Identificacao do
      begin
        CpfCnpj := ObterCNPJCPF(ANode);

        if CpfCnpj = '' then
          Nif := ObterConteudo(ANode.Childrens.FindAnyNs('NIF'), tcStr);

        CAEPF := ObterConteudo(ANode.Childrens.FindAnyNs('CAEPF'), tcStr);
        InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('IM'), tcStr);
      end;

      RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('xNome'), tcStr);

      LerEnderecoFornecedor(ANode, Item);

      Contato.Telefone := ObterConteudo(ANode.Childrens.FindAnyNs('fone'), tcStr);
      Contato.Email := ObterConteudo(ANode.Childrens.FindAnyNs('email'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerinfDPS(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('infDPS');

  if AuxNode <> nil then
  begin
    NFSe.infID.ID := OnlyNumber(ObterConteudoTag(AuxNode.Attributes.Items['Id']));
    NFSe.DataEmissao := ObterConteudo(AuxNode.Childrens.FindAnyNs('dhEmi'), tcDatHor);
    NFSe.verAplic := ObterConteudo(AuxNode.Childrens.FindAnyNs('verAplic'), tcStr);
    NFSe.IdentificacaoRps.Serie := ObterConteudo(AuxNode.Childrens.FindAnyNs('serie'), tcStr);
    NFSe.IdentificacaoRps.Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('nDPS'), tcStr);
    NFSe.Competencia := ObterConteudo(AuxNode.Childrens.FindAnyNs('dCompet'), tcDat);
    NFSe.tpEmit := StrTotpEmit(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tpEmit'), tcStr));

    LerSubstituicao(AuxNode);
    LerPrestador(AuxNode);
    LerTomador(AuxNode);
    LerIntermediario(AuxNode);
    LerServico(AuxNode);
    LerValores(AuxNode);
  end;
end;

procedure TNFSeR_PadraoNacional.LerinfNFSe(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('infNFSe');

  if AuxNode <> nil then
  begin
    with NFSe.infNFSe do
    begin
      ID := OnlyNumber(ObterConteudoTag(AuxNode.Attributes.Items['Id']));
      xLocEmi := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLocEmi'), tcStr);
      xLocPrestacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLocPrestacao'), tcStr);
      nNFSe := ObterConteudo(AuxNode.Childrens.FindAnyNs('nNFSe'), tcStr);
      cLocIncid := ObterConteudo(AuxNode.Childrens.FindAnyNs('cLocIncid'), tcInt);
      xLocIncid := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLocIncid'), tcStr);
      xTribNac := ObterConteudo(AuxNode.Childrens.FindAnyNs('xTribNac'), tcStr);
      xTribMun := ObterConteudo(AuxNode.Childrens.FindAnyNs('xTribMun'), tcStr);
      xNBS := ObterConteudo(AuxNode.Childrens.FindAnyNs('xNBS'), tcStr);
      verAplic := ObterConteudo(AuxNode.Childrens.FindAnyNs('verAplic'), tcStr);
      ambGer := StrToambGer(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('ambGer'), tcStr));
      tpEmis := StrTotpEmis(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tpEmis'), tcStr));
      procEmi := StrToprocEmi(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('procEmi'), tcStr));
      cStat := ObterConteudo(AuxNode.Childrens.FindAnyNs('cStat'), tcInt);
      dhProc := ObterConteudo(AuxNode.Childrens.FindAnyNs('dhProc'), tcDatHor);
      nDFSe := ObterConteudo(AuxNode.Childrens.FindAnyNs('nDFSe'), tcStr);

      LerEmitente(AuxNode);
      LerValoresNFSe(AuxNode);
      LerDPS(AuxNode);
    end;

    NFSe.Numero := NFSe.infNFSe.nNFSe;
    NFSe.CodigoVerificacao := NFSe.infNFSe.ID;

    with NFSe.Servico.Valores do
    begin
      BaseCalculo := ValorServicos - ValorDeducoes - DescontoIncondicionado;

      RetencoesFederais := ValorPis + ValorCofins + ValorInss + ValorIr + ValorCsll;

      ValorLiquidoNfse := ValorServicos - RetencoesFederais - OutrasRetencoes -
                 ValorIssRetido - DescontoIncondicionado - DescontoCondicionado;

      ValorTotalNotaFiscal := ValorServicos - DescontoCondicionado -
                              DescontoIncondicionado;
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerInformacoesComplementares(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('infoCompl');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.infoCompl do
    begin
      idDocTec := ObterConteudo(AuxNode.Childrens.FindAnyNs('idDocTec'), tcStr);
      docRef := ObterConteudo(AuxNode.Childrens.FindAnyNs('docRef'), tcStr);
      xInfComp := ObterConteudo(AuxNode.Childrens.FindAnyNs('xInfComp'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerIntermediario(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('interm');

  if AuxNode <> nil then
  begin
    with NFSe.Intermediario do
    begin
      with Identificacao do
      begin
        CpfCnpj := ObterCNPJCPF(AuxNode);

        if CpfCnpj = '' then
          Nif := ObterConteudo(AuxNode.Childrens.FindAnyNs('NIF'), tcStr);

        CAEPF := ObterConteudo(AuxNode.Childrens.FindAnyNs('CAEPF'), tcStr);
        InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('IM'), tcStr);
      end;
      RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('xNome'), tcStr);

      LerEnderecoItermediario(AuxNode);

      Contato.Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('fone'), tcStr);
      Contato.Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('email'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerLocacaoSubLocacao(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('lsadppu');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Locacao do
    begin
      categ := StrTocateg(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('categ'), tcStr));
      objeto := StrToobjeto(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('objeto'), tcStr));
      extensao := ObterConteudo(AuxNode.Childrens.FindAnyNs('extensao'), tcStr);
      nPostes := ObterConteudo(AuxNode.Childrens.FindAnyNs('nPostes'), tcInt);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerLocalPrestacao(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  xUF: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('locPrest');

  if AuxNode <> nil then
  begin
    with NFSe.Servico do
    begin
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('cLocPrestacao'), tcStr);
      CodigoPais := SiglaISO2ToCodIBGEPais(ObterConteudo(AuxNode.Childrens.FindAnyNs('cPaisPrestacao'), tcStr));

      MunicipioPrestacaoServico := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);
      MunicipioPrestacaoServico := MunicipioPrestacaoServico + '/' + xUF;
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerNFNFS(const ANode: TACBrXmlNode;
  Item: Integer);
begin
  if ANode <> nil then
  begin
    with NFSe.Servico.Valores.DocDeducao.Items[item].NFNFS do
    begin
      nNFS := ObterConteudo(ANode.Childrens.FindAnyNs('nNFS'), tcStr);
      modNFS := ObterConteudo(ANode.Childrens.FindAnyNs('modNFS'), tcStr);
      serieNFS := ObterConteudo(ANode.Childrens.FindAnyNs('serieNFS'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerNFSeMunicipio(
  const ANode: TACBrXmlNode; Item: Integer);
begin
  if ANode <> nil then
  begin
    with NFSe.Servico.Valores.DocDeducao.Items[item].NFSeMun do
    begin
      cMunNFSeMun := ObterConteudo(ANode.Childrens.FindAnyNs('cMunNFSeMun'), tcStr);
      nNFSeMun := ObterConteudo(ANode.Childrens.FindAnyNs('nNFSeMun'), tcStr);
      cVerifNFSeMun := ObterConteudo(ANode.Childrens.FindAnyNs('cVerifNFSeMun'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerObra(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('obra');

  if AuxNode <> nil then
  begin
    with NFSe.ConstrucaoCivil do
    begin
      CodigoObra := ObterConteudo(AuxNode.Childrens.FindAnyNs('cObra'), tcStr);
      inscImobFisc := ObterConteudo(AuxNode.Childrens.FindAnyNs('inscImobFisc'), tcStr);

      LerEnderecoObra(AuxNode);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerPercentualTotalTributos(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('pTotTrib');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores.totTrib do
    begin
      pTotTribFed := ObterConteudo(AuxNode.Childrens.FindAnyNs('pTotTribFed'), tcDe2);
      pTotTribEst := ObterConteudo(AuxNode.Childrens.FindAnyNs('pTotTribEst'), tcDe2);
      pTotTribMun := ObterConteudo(AuxNode.Childrens.FindAnyNs('pTotTribMun'), tcDe2);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('prest');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador do
    begin
      if FpLerPrestador then
      begin
        with IdentificacaoPrestador do
        begin
          CpfCnpj := ObterCNPJCPF(AuxNode);

          if CpfCnpj = '' then
            Nif := ObterConteudo(AuxNode.Childrens.FindAnyNs('NIF'), tcStr);

          CAEPF := ObterConteudo(AuxNode.Childrens.FindAnyNs('CAEPF'), tcStr);
          InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('IM'), tcStr);
        end;

        RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('xNome'), tcStr);

        LerEnderecoPrestador(AuxNode);

        Contato.Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('fone'), tcStr);
        Contato.Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('email'), tcStr);
      end;

      LerRegimeTributacaoPrestador(AuxNode);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerRegimeTributacaoPrestador(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('regTrib');

  if AuxNode <> nil then
  begin
    NFSe.OptanteSN := StrToOptanteSN(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('opSimpNac'), tcStr));
    NFSe.RegimeApuracaoSN := StrToRegimeApuracaoSN(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('regApTribSN'), tcStr));
    NFSe.RegimeEspecialTributacao := FpAOwner.StrToRegimeEspecialTributacao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('regEspTrib'), tcStr));
  end;
end;

procedure TNFSeR_PadraoNacional.LerServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('serv');

  if AuxNode <> nil then
  begin
    LerLocalPrestacao(AuxNode);
    LerCodigoServico(AuxNode);
    LerComercioExterior(AuxNode);
    LerLocacaoSubLocacao(AuxNode);
    LerObra(AuxNode);
    LerAtividadeEvento(AuxNode);
    LerExploracaoRodoviaria(AuxNode);
    LerInformacoesComplementares(AuxNode);
  end;
end;

procedure TNFSeR_PadraoNacional.LerServicoPrestado(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('vServPrest');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      ValorRecebido := ObterConteudo(AuxNode.Childrens.FindAnyNs('vReceb'), tcDe2);
      ValorServicos := ObterConteudo(AuxNode.Childrens.FindAnyNs('vServ'), tcDe2);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerSubstituicao(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('subst');

  if AuxNode <> nil then
  begin
    NFSe.subst.chSubstda := ObterConteudo(AuxNode.Childrens.FindAnyNs('chSubstda'), tcStr);
    NFSe.subst.cMotivo := StrTocMotivo(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('cMotivo'), tcStr));
    NFSe.subst.xMotivo := ObterConteudo(AuxNode.Childrens.FindAnyNs('xMotivo'), tcStr);
  end;
end;

procedure TNFSeR_PadraoNacional.LerTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('toma');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador do
    begin
      with IdentificacaoTomador do
      begin
        CpfCnpj := ObterCNPJCPF(AuxNode);

        if CpfCnpj = '' then
          Nif := ObterConteudo(AuxNode.Childrens.FindAnyNs('NIF'), tcStr);

        CAEPF := ObterConteudo(AuxNode.Childrens.FindAnyNs('CAEPF'), tcStr);
        InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('IM'), tcStr);
      end;

      RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('xNome'), tcStr);

      LerEnderecoTomador(AuxNode);

      Contato.Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('fone'), tcStr);
      Contato.Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('email'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerTotalTributos(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('totTrib');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores.totTrib do
    begin
      LerValorTotalTributos(AuxNode);
      LerPercentualTotalTributos(AuxNode);

      indTotTrib := StrToindTotTrib(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('indTotTrib'), tcStr));
      pTotTribSN := ObterConteudo(AuxNode.Childrens.FindAnyNs('pTotTribSN'), tcDe2);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerTributacao(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('trib');

  if AuxNode <> nil then
  begin
    LerTributacaoMunicipal(AuxNode);
    LerTributacaoNacional(AuxNode);
    LerTotalTributos(AuxNode);
  end;
end;

procedure TNFSeR_PadraoNacional.LerTributacaoMunicipal(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('tribMun');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores.tribMun do
    begin
      tribISSQN := StrTotribISSQN(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tribISSQN'), tcStr));
      cPaisResult := SiglaISO2ToCodIBGEPais(ObterConteudo(AuxNode.Childrens.FindAnyNs('cPaisResult'), tcStr));

      LerBeneficioMunicipal(AuxNode);
      LerExigibilidadeSuspensa(AuxNode);

      tpImunidade := StrTotpImunidade(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tpImunidade'), tcStr));
      pAliq := ObterConteudo(AuxNode.Childrens.FindAnyNs('pAliq'), tcDe2);
      tpRetISSQN := StrTotpRetISSQN(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tpRetISSQN'), tcStr));
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerTributacaoNacional(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('tribNac');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores.tribFed do
    begin
      LerTributacaoOutrosPisCofins(AuxNode);

      vRetCP := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRetCP'), tcDe2);
      vRetIRRF := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRetIRRF'), tcDe2);
      vRetCSLL := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRetCSLL'), tcDe2);

      NFSe.Servico.Valores.ValorIr := vRetIRRF;
      NFSe.Servico.Valores.ValorCsll := vRetCSLL;
      NFSe.Servico.Valores.ValorCPP := vRetCP;
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerTributacaoOutrosPisCofins(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('piscofins');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores.tribFed do
    begin
      CST := StrToCST(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('CST'), tcStr));
      vBCPisCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('vBCPisCofins'), tcDe2);
      pAliqPis := ObterConteudo(AuxNode.Childrens.FindAnyNs('pAliqPis'), tcDe2);
      pAliqCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('pAliqCofins'), tcDe2);
      vPis := ObterConteudo(AuxNode.Childrens.FindAnyNs('vPis'), tcDe2);
      vCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('vCofins'), tcDe2);
      tpRetPisCofins := StrTotpRetPisCofins(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tpRetPisCofins'), tcStr));

      NFSe.Servico.Valores.ValorPis := vPis;
      NFSe.Servico.Valores.ValorCofins := vCofins;
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerValores(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('valores');

  if AuxNode <> nil then
  begin
    LerServicoPrestado(AuxNode);
    LerDescontos(AuxNode);
    LerDeducoes(AuxNode);
    LerTributacao(AuxNode);
  end;
end;

procedure TNFSeR_PadraoNacional.LerValoresNFSe(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('valores');

  if AuxNode <> nil then
  begin
    with NFSe.infNFSe.valores do
    begin
      vCalcDR := ObterConteudo(AuxNode.Childrens.FindAnyNs('vCalcDR'), tcDe2);
      tpBM := ObterConteudo(AuxNode.Childrens.FindAnyNs('tpBM'), tcStr);
      vCalcBM := ObterConteudo(AuxNode.Childrens.FindAnyNs('vCalcBM'), tcDe2);
      BaseCalculo := ObterConteudo(AuxNode.Childrens.FindAnyNs('vBC'), tcDe2);
      Aliquota := ObterConteudo(AuxNode.Childrens.FindAnyNs('pAliqAplic'), tcDe2);
      ValorIss := ObterConteudo(AuxNode.Childrens.FindAnyNs('vISSQN'), tcDe2);
      vTotalRet := ObterConteudo(AuxNode.Childrens.FindAnyNs('vTotalRet'), tcDe2);
      ValorLiquidoNfse := ObterConteudo(AuxNode.Childrens.FindAnyNs('vLiq'), tcDe2);
    end;

    NFSe.OutrasInformacoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('xOutInf'), tcStr);
    NFSe.OutrasInformacoes := StringReplace(NFSe.OutrasInformacoes, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);
  end;
end;

procedure TNFSeR_PadraoNacional.LerValorTotalTributos(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('vTotTrib');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores.totTrib do
    begin
      vTotTribFed := ObterConteudo(AuxNode.Childrens.FindAnyNs('vTotTribFed'), tcDe2);
      vTotTribEst := ObterConteudo(AuxNode.Childrens.FindAnyNs('vTotTribEst'), tcDe2);
      vTotTribMun := ObterConteudo(AuxNode.Childrens.FindAnyNs('vTotTribMun'), tcDe2);
    end;
  end;
end;

function TNFSeR_PadraoNacional.LerXml: Boolean;
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

  // Não remover o espaço em branco, caso contrario vai encontrar tags que tem
  // NFSe em sua grafia.
  if (Pos('NFSe ', Arquivo) > 0) then
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

function TNFSeR_PadraoNacional.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
begin
  Result := True;
  FpLerPrestador := False;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  LerinfNFSe(ANode);

  LerCampoLink;
end;

function TNFSeR_PadraoNacional.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
begin
  Result := True;
  FpLerPrestador := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  LerinfDPS(ANode);
end;

end.
