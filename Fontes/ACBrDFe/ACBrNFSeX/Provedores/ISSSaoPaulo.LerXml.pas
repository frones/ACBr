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

unit ISSSaoPaulo.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrUtil,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { TNFSeR_ISSSaoPaulo }

  TNFSeR_ISSSaoPaulo = class(TNFSeRClass)
  protected

    procedure SetxItemListaServico(Codigo: string);

    procedure LerChaveNFe(const ANode: TACBrXmlNode);
    procedure LerChaveRPS(const ANode: TACBrXmlNode);
    procedure LerCPFCNPJPrestador(const ANode: TACBrXmlNode);
    procedure LerEnderecoPrestador(const ANode: TACBrXmlNode);
    procedure LerCPFCNPJTomador(const ANode: TACBrXmlNode);
    procedure LerEnderecoTomador(const ANode: TACBrXmlNode);
    procedure LerCPFCNPJIntermediario(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     ISSSaoPaulo
//==============================================================================

{ TNFSeR_ISSSaoPaulo }

procedure TNFSeR_ISSSaoPaulo.LerChaveNFe(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('ChaveNFe');

  if AuxNode <> nil then
  begin
    NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('InscricaoPrestador'), tcStr);

    NFSe.Numero            := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NumeroNFe'), tcStr);
    NFSe.CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
  end;
end;

procedure TNFSeR_ISSSaoPaulo.LerChaveRPS(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('ChaveRPS');

  if AuxNode <> nil then
  begin
    NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('InscricaoPrestador'), tcStr);

    NFSe.IdentificacaoRps.Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NumeroRPS'), tcStr);
    NFSe.IdentificacaoRps.Serie  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('SerieRPS'), tcStr);
    NFSe.IdentificacaoRps.Tipo   := trRPS;

    if NFSe.InfID.ID = '' then
      NFSe.InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero) +
                       NFSe.IdentificacaoRps.Serie;
  end;
end;

procedure TNFSeR_ISSSaoPaulo.LerCPFCNPJIntermediario(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('CPFCNPJIntermediario');

  if AuxNode <> nil then
  begin
    NFSe.IntermediarioServico.CpfCnpj := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CNPJ'), tcStr);

    if NFSe.IntermediarioServico.CpfCnpj = '' then
      NFSe.IntermediarioServico.CpfCnpj := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CPF'), tcStr);
  end;
end;

procedure TNFSeR_ISSSaoPaulo.LerCPFCNPJPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('CPFCNPJPrestador');

  if AuxNode <> nil then
    NFSe.Prestador.IdentificacaoPrestador.Cnpj := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CNPJ'), tcStr);
end;

procedure TNFSeR_ISSSaoPaulo.LerCPFCNPJTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('CPFCNPJTomador');

  if AuxNode <> nil then
    NFSe.Tomador.IdentificacaoTomador.CpfCnpj := ProcessarCNPJCPF(AuxNode);
end;

procedure TNFSeR_ISSSaoPaulo.LerEnderecoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('EnderecoPrestador');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Endereco do
    begin
      TipoLogradouro  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TipoLogradouro'), tcStr);
      Endereco        := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Logradouro'), tcStr);
      Numero          := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NumeroEndereco'), tcStr);
      Complemento     := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ComplementoEndereco'), tcStr);
      Bairro          := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Cidade'), tcStr);
      UF              := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('UF'), tcStr);
      CEP             := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcStr);
      xMunicipio      := CodIBGEToCidade(StrToIntDef(CodigoMunicipio, 0));
    end;
  end;
end;

procedure TNFSeR_ISSSaoPaulo.LerEnderecoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('EnderecoTomador');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      TipoLogradouro  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TipoLogradouro'), tcStr);
      Endereco        := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Logradouro'), tcStr);
      Numero          := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NumeroEndereco'), tcStr);
      Complemento     := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ComplementoEndereco'), tcStr);
      Bairro          := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Cidade'), tcStr);
      UF              := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('UF'), tcStr);
      CEP             := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcStr);
      xMunicipio      := CodIBGEToCidade(StrToIntDef(CodigoMunicipio, 0));
    end;

    NFSe.Servico.CodigoMunicipio := NFSe.Tomador.Endereco.CodigoMunicipio;
  end;
end;

function TNFSeR_ISSSaoPaulo.LerXml: Boolean;
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

  if (Pos('NFe', xRetorno) > 0) then
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

function TNFSeR_ISSSaoPaulo.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  aValor: string;
  Ok :Boolean;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode;

  NFSe.dhRecebimento := Now;
  NFSe.SituacaoNfse := snNormal;
//  NFSe.Protocolo      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NumeroLote'), tcStr);
  NFSe.NumeroLote := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('NumeroLote'), tcStr);
  NFSe.DataEmissao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('DataEmissaoNFe'), tcDatHor);
  NFSe.DataEmissaoRps := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('DataEmissaoRPS'), tcDat);
  NFSe.Competencia := NFSe.DataEmissao;

  aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('StatusNFe'), tcStr);

  if aValor = 'C' then
    NFSe.SituacaoNfse := snCancelado;

  NFSe.TipoTributacaoRPS := StrToTipoTributacaoRPS(Ok, ProcessarConteudo(AuxNode.Childrens.FindAnyNs('TributacaoNFe'), tcStr));

  aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('OpcaoSimples'), tcStr);

  if aValor = '0' then
    NFSe.OptanteSimplesNacional := snNao
  else
    NFSe.OptanteSimplesNacional := snSim;

  with NFSe.ValoresNfse do
  begin
    ValorLiquidoNfse := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorServicos'), tcDe2);
    BaseCalculo      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorServicos'), tcDe2);
    Aliquota         := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('AliquotaServicos'), tcDe2);
    ValorIss         := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorISS'), tcDe2);

    Aliquota := (NFSe.ValoresNfse.Aliquota * 100);
  end;

  aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('CodigoServico'), tcStr);

  SetxItemListaServico(aValor);

  NFSe.Servico.Discriminacao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('Discriminacao'), tcStr);

  aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ISSRetido'), tcStr);

  with NFSe.Servico.Valores do
  begin
    ValorServicos := NFSe.ValoresNfse.ValorLiquidoNfse;
    BaseCalculo   := NFSe.ValoresNfse.BaseCalculo;
    Aliquota      := NFSe.ValoresNfse.Aliquota;
    ValorIss      := NFSe.ValoresNfse.ValorIss;
    ValorPis      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorPis'), tcDe2);
    ValorCofins   := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorCofins'), tcDe2);
    ValorInss     := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorInss'), tcDe2);
    ValorIr       := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorIr'), tcDe2);
    ValorCsll     := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ValorCsll'), tcDe2);

    if aValor = 'false' then
      IssRetido := stNormal
    else
      IssRetido := stRetencao;

    ValorLiquidoNfse := ValorServicos -
                        (ValorPis + ValorCofins + ValorInss + ValorIr + ValorCsll +
                         ValorDeducoes + DescontoCondicionado +
                         DescontoIncondicionado + ValorIssRetido);
  end;

  NFSe.Prestador.RazaoSocial   := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocialPrestador'), tcStr);
  NFSe.Prestador.Contato.Email := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('EmailPrestador'), tcStr);

  NFSe.Tomador.RazaoSocial   := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocialTomador'), tcStr);
  NFSe.Tomador.Contato.Email := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('EmailTomador'), tcStr);

  LerChaveNFe(AuxNode);
  LerChaveRPS(AuxNode);
  LerCPFCNPJPrestador(AuxNode);
  LerEnderecoPrestador(AuxNode);
  LerCPFCNPJTomador(AuxNode);
  LerEnderecoTomador(AuxNode);
end;

function TNFSeR_ISSSaoPaulo.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  aValor: string;
begin
  Result := True;

  with NFSe do
  begin
    Assinatura := ProcessarConteudo(ANode.Childrens.FindAnyNs('Assinatura'), tcStr);
    DataEmissao := ProcessarConteudo(ANode.Childrens.FindAnyNs('DataEmissao'), tcDat);

    aValor := ProcessarConteudo(ANode.Childrens.FindAnyNs('StatusRPS'), tcStr);

    if aValor = 'N' then
      Status := srNormal
    else
      Status := srCancelado;

    TipoTributacaoRPS := ProcessarConteudo(ANode.Childrens.FindAnyNs('TributacaoRPS'), tcStr);

    LerChaveRPS(ANode);

    with Servico do
    begin
      ItemListaServico := ProcessarConteudo(ANode.Childrens.FindAnyNs('CodigoServico'), tcStr);

      Discriminacao := ProcessarConteudo(ANode.Childrens.FindAnyNs('Discriminacao'), tcStr);

      ValorCargaTributaria := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorCargaTributaria'), tcDe2);

      PercentualCargaTributaria := ProcessarConteudo(ANode.Childrens.FindAnyNs('PercentualCargaTributaria'), tcDe4);

      FonteCargaTributaria := ProcessarConteudo(ANode.Childrens.FindAnyNs('FonteCargaTributaria'), tcStr);

      MunicipioIncidencia := ProcessarConteudo(ANode.Childrens.FindAnyNs('MunicipioPrestacao'), tcInt);

      with Valores do
      begin
        ValorServicos := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorServicos'), tcDe2);

        ValorDeducoes := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorDeducoes'), tcDe2);

        ValorPis := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorPIS'), tcDe2);

        ValorCofins := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorCOFINS'), tcDe2);

        ValorInss := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorINSS'), tcDe2);

        ValorIr := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorIR'), tcDe2);

        ValorCsll := ProcessarConteudo(ANode.Childrens.FindAnyNs('ValorCSLL'), tcDe2);

        Aliquota := ProcessarConteudo(ANode.Childrens.FindAnyNs('AliquotaServicos'), tcDe4);

        aValor := ProcessarConteudo(ANode.Childrens.FindAnyNs('ISSRetido'), tcStr);

        if aValor = 'true' then
          IssRetido := stRetencao
        else
          IssRetido := stNormal;
      end;
    end;

    LerCPFCNPJTomador(ANode);

    with Tomador do
    begin
      RazaoSocial := ProcessarConteudo(ANode.Childrens.FindAnyNs('RazaoSocialTomador'), tcStr);

      with IdentificacaoTomador do
      begin
        InscricaoMunicipal := ProcessarConteudo(ANode.Childrens.FindAnyNs('InscricaoMunicipalTomador'), tcStr);

        InscricaoEstadual := ProcessarConteudo(ANode.Childrens.FindAnyNs('InscricaoEstadualTomador'), tcStr);
      end;

      with Contato do
      begin
        Email := ProcessarConteudo(ANode.Childrens.FindAnyNs('EmailTomador'), tcStr);
      end;
    end;

    LerEnderecoTomador(ANode);
    LerCPFCNPJIntermediario(ANode);

    with IntermediarioServico do
    begin
      InscricaoMunicipal := ProcessarConteudo(ANode.Childrens.FindAnyNs('InscricaoMunicipalIntermediario'), tcStr);

      EMail := ProcessarConteudo(ANode.Childrens.FindAnyNs('EmailIntermediario'), tcStr);

      aValor := ProcessarConteudo(ANode.Childrens.FindAnyNs('ISSRetidoIntermediario'), tcStr);

      if aValor = 'true' then
        IssRetido := stRetencao
      else
        IssRetido := stNormal;
    end;

    with ConstrucaoCivil do
    begin
      nCei := ProcessarConteudo(ANode.Childrens.FindAnyNs('CodigoCEI'), tcStr);

      nMatri := ProcessarConteudo(ANode.Childrens.FindAnyNs('MatriculaObra'), tcStr);

      nNumeroEncapsulamento := ProcessarConteudo(ANode.Childrens.FindAnyNs('NumeroEncapsulamento'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ISSSaoPaulo.SetxItemListaServico(Codigo: string);
var
  Item: Integer;
  ItemServico: string;
begin
  NFSe.Servico.ItemListaServico := Codigo;

  Item := StrToIntDef(OnlyNumber(Nfse.Servico.ItemListaServico), 0);
  if Item < 100 then
    Item := Item * 100 + 1;

  ItemServico := FormatFloat('0000', Item);

  NFSe.Servico.ItemListaServico := Copy(ItemServico, 1, 2) + '.' +
                                     Copy(ItemServico, 3, 2);

  if FAOwner.ConfigGeral.TabServicosExt then
    NFSe.Servico.xItemListaServico := ObterDescricaoServico(ItemServico)
  else
    NFSe.Servico.xItemListaServico := CodItemServToDesc(ItemServico);
end;

end.
