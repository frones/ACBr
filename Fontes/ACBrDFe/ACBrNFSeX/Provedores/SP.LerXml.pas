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

unit SP.LerXml;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ENDIF}
  SysUtils, Classes, StrUtils,
  ACBrUtil,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { TNFSeR_SP }

  TNFSeR_SP = class(TNFSeRClass)
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
//     SP
//==============================================================================

{ TNFSeR_SP }

procedure TNFSeR_SP.LerChaveNFe(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('ChaveNFe');

  if AuxNode <> nil then
  begin
    NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.Find('InscricaoPrestador'), tcStr);

    NFSe.Numero            := ProcessarConteudo(AuxNode.Childrens.Find('NumeroNFe'), tcStr);
    NFSe.CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.Find('CodigoVerificacao'), tcStr);
  end;
end;

procedure TNFSeR_SP.LerChaveRPS(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('ChaveRPS');

  if AuxNode <> nil then
  begin
    NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.Find('InscricaoPrestador'), tcStr);

    NFSe.IdentificacaoRps.Numero := ProcessarConteudo(AuxNode.Childrens.Find('NumeroRPS'), tcStr);
    NFSe.IdentificacaoRps.Serie  := ProcessarConteudo(AuxNode.Childrens.Find('SerieRPS'), tcStr);
    NFSe.IdentificacaoRps.Tipo   := trRPS;

    if NFSe.InfID.ID = '' then
      NFSe.InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero) +
                       NFSe.IdentificacaoRps.Serie;
  end;
end;

procedure TNFSeR_SP.LerCPFCNPJIntermediario(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('CPFCNPJIntermediario');

  if AuxNode <> nil then
  begin
    NFSe.IntermediarioServico.CpfCnpj := ProcessarConteudo(AuxNode.Childrens.Find('CNPJ'), tcStr);

    if NFSe.IntermediarioServico.CpfCnpj = '' then
      NFSe.IntermediarioServico.CpfCnpj := ProcessarConteudo(AuxNode.Childrens.Find('CPF'), tcStr);
  end;
end;

procedure TNFSeR_SP.LerCPFCNPJPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('CPFCNPJPrestador');

  if AuxNode <> nil then
    NFSe.Prestador.IdentificacaoPrestador.Cnpj := ProcessarConteudo(AuxNode.Childrens.Find('CNPJ'), tcStr);
end;

procedure TNFSeR_SP.LerCPFCNPJTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('CPFCNPJTomador');

  if AuxNode <> nil then
    NFSe.Tomador.IdentificacaoTomador.CpfCnpj := ProcessarCNPJCPF(AuxNode);
end;

procedure TNFSeR_SP.LerEnderecoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('EnderecoPrestador');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Endereco do
    begin
      TipoLogradouro  := ProcessarConteudo(AuxNode.Childrens.Find('TipoLogradouro'), tcStr);
      Endereco        := ProcessarConteudo(AuxNode.Childrens.Find('Logradouro'), tcStr);
      Numero          := ProcessarConteudo(AuxNode.Childrens.Find('NumeroEndereco'), tcStr);
      Complemento     := ProcessarConteudo(AuxNode.Childrens.Find('ComplementoEndereco'), tcStr);
      Bairro          := ProcessarConteudo(AuxNode.Childrens.Find('Bairro'), tcStr);
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('Cidade'), tcStr);
      UF              := ProcessarConteudo(AuxNode.Childrens.Find('UF'), tcStr);
      CEP             := ProcessarConteudo(AuxNode.Childrens.Find('CEP'), tcStr);
      xMunicipio      := CodIBGEToCidade(StrToIntDef(CodigoMunicipio, 0));
    end;
  end;
end;

procedure TNFSeR_SP.LerEnderecoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('EnderecoTomador');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      TipoLogradouro  := ProcessarConteudo(AuxNode.Childrens.Find('TipoLogradouro'), tcStr);
      Endereco        := ProcessarConteudo(AuxNode.Childrens.Find('Logradouro'), tcStr);
      Numero          := ProcessarConteudo(AuxNode.Childrens.Find('NumeroEndereco'), tcStr);
      Complemento     := ProcessarConteudo(AuxNode.Childrens.Find('ComplementoEndereco'), tcStr);
      Bairro          := ProcessarConteudo(AuxNode.Childrens.Find('Bairro'), tcStr);
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('Cidade'), tcStr);
      UF              := ProcessarConteudo(AuxNode.Childrens.Find('UF'), tcStr);
      CEP             := ProcessarConteudo(AuxNode.Childrens.Find('CEP'), tcStr);
      xMunicipio      := CodIBGEToCidade(StrToIntDef(CodigoMunicipio, 0));
    end;
  end;
end;

function TNFSeR_SP.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if (Pos('NFe', Arquivo) > 0) then
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
end;

function TNFSeR_SP.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  aValor: string;
  Ok :Boolean;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.Find('NFe');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.Find('CompNfse');

  if AuxNode = nil then Exit;

  NFSe.dhRecebimento  := Now;
  NFSe.Protocolo      := ProcessarConteudo(AuxNode.Childrens.Find('NumeroLote'), tcStr);
  NFSe.NumeroLote     := ProcessarConteudo(AuxNode.Childrens.Find('NumeroLote'), tcStr);
  NFSe.DataEmissao    := ProcessarConteudo(AuxNode.Childrens.Find('DataEmissaoNFe'), tcDatHor);
  NFSe.DataEmissaoRps := ProcessarConteudo(AuxNode.Childrens.Find('DataEmissaoRPS'), tcDat);

  aValor := ProcessarConteudo(AuxNode.Childrens.Find('StatusNFe'), tcStr);

  if aValor = 'C' then
  begin
    NFSe.Status    := srCancelado;
    NFSe.Cancelada := snSim;
  end
  else
  begin
    NFSe.Status    := srNormal;
    NFSe.Cancelada := snNao;
  end;

  NFSe.TipoTributacaoRPS := StrToTipoTributacaoRPS(Ok, ProcessarConteudo(AuxNode.Childrens.Find('TributacaoNFe'), tcStr));

  aValor := ProcessarConteudo(AuxNode.Childrens.Find('OpcaoSimples'), tcStr);

  if aValor = '0' then
    NFSe.OptanteSimplesNacional := snNao
  else
    NFSe.OptanteSimplesNacional := snSim;

  with NFSe.ValoresNfse do
  begin
    ValorLiquidoNfse := ProcessarConteudo(AuxNode.Childrens.Find('ValorServicos'), tcDe2);
    BaseCalculo      := ProcessarConteudo(AuxNode.Childrens.Find('ValorServicos'), tcDe2);
    Aliquota         := ProcessarConteudo(AuxNode.Childrens.Find('AliquotaServicos'), tcDe2);
    ValorIss         := ProcessarConteudo(AuxNode.Childrens.Find('ValorISS'), tcDe2);

    Aliquota := (NFSe.ValoresNfse.Aliquota * 100);
  end;

  aValor := ProcessarConteudo(AuxNode.Childrens.Find('CodigoServico'), tcStr);

  SetxItemListaServico(aValor);

  NFSe.Servico.Discriminacao := ProcessarConteudo(AuxNode.Childrens.Find('Discriminacao'), tcStr);

  aValor := ProcessarConteudo(AuxNode.Childrens.Find('ISSRetido'), tcStr);

  with NFSe.Servico.Valores do
  begin
    ValorServicos := NFSe.ValoresNfse.ValorLiquidoNfse;
    BaseCalculo   := NFSe.ValoresNfse.BaseCalculo;
    Aliquota      := NFSe.ValoresNfse.Aliquota;
    ValorIss      := NFSe.ValoresNfse.ValorIss;
    ValorPis      := ProcessarConteudo(AuxNode.Childrens.Find('ValorPis'), tcDe2);
    ValorCofins   := ProcessarConteudo(AuxNode.Childrens.Find('ValorCofins'), tcDe2);
    ValorInss     := ProcessarConteudo(AuxNode.Childrens.Find('ValorInss'), tcDe2);
    ValorIr       := ProcessarConteudo(AuxNode.Childrens.Find('ValorIr'), tcDe2);
    ValorCsll     := ProcessarConteudo(AuxNode.Childrens.Find('ValorCsll'), tcDe2);

    if aValor = 'false' then
      IssRetido := stNormal
    else
      IssRetido := stRetencao;

    ValorLiquidoNfse := ValorServicos -
                        (ValorPis + ValorCofins + ValorInss + ValorIr + ValorCsll +
                         ValorDeducoes + DescontoCondicionado +
                         DescontoIncondicionado + ValorIssRetido);
  end;

  NFSe.Prestador.RazaoSocial   := ProcessarConteudo(AuxNode.Childrens.Find('RazaoSocialPrestador'), tcStr);
  NFSe.Prestador.Contato.Email := ProcessarConteudo(AuxNode.Childrens.Find('EmailPrestador'), tcStr);

  NFSe.Tomador.RazaoSocial   := ProcessarConteudo(AuxNode.Childrens.Find('RazaoSocialTomador'), tcStr);
  NFSe.Tomador.Contato.Email := ProcessarConteudo(AuxNode.Childrens.Find('EmailTomador'), tcStr);

  LerChaveNFe(AuxNode);
  LerChaveRPS(AuxNode);
  LerCPFCNPJPrestador(AuxNode);
  LerEnderecoPrestador(AuxNode);
  LerCPFCNPJTomador(AuxNode);
  LerEnderecoTomador(AuxNode);
end;

function TNFSeR_SP.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  aValor: string;
begin
  Result := True;

  with NFSe do
  begin
    Assinatura := ProcessarConteudo(ANode.Childrens.Find('Assinatura'), tcStr);
    DataEmissao := ProcessarConteudo(ANode.Childrens.Find('DataEmissao'), tcDat);

    aValor := ProcessarConteudo(ANode.Childrens.Find('StatusRPS'), tcStr);

    if aValor = 'N' then
      Status := srNormal
    else
      Status := srCancelado;

    TipoTributacaoRPS := ProcessarConteudo(ANode.Childrens.Find('TributacaoRPS'), tcStr);

    LerChaveRPS(ANode);

    with Servico do
    begin
      ItemListaServico := ProcessarConteudo(ANode.Childrens.Find('CodigoServico'), tcStr);

      Discriminacao := ProcessarConteudo(ANode.Childrens.Find('Discriminacao'), tcStr);

      ValorCargaTributaria := ProcessarConteudo(ANode.Childrens.Find('ValorCargaTributaria'), tcDe2);

      PercentualCargaTributaria := ProcessarConteudo(ANode.Childrens.Find('PercentualCargaTributaria'), tcDe4);

      FonteCargaTributaria := ProcessarConteudo(ANode.Childrens.Find('FonteCargaTributaria'), tcStr);

      MunicipioIncidencia := ProcessarConteudo(ANode.Childrens.Find('MunicipioPrestacao'), tcInt);

      with Valores do
      begin
        ValorServicos := ProcessarConteudo(ANode.Childrens.Find('ValorServicos'), tcDe2);

        ValorDeducoes := ProcessarConteudo(ANode.Childrens.Find('ValorDeducoes'), tcDe2);

        ValorPis := ProcessarConteudo(ANode.Childrens.Find('ValorPIS'), tcDe2);

        ValorCofins := ProcessarConteudo(ANode.Childrens.Find('ValorCOFINS'), tcDe2);

        ValorInss := ProcessarConteudo(ANode.Childrens.Find('ValorINSS'), tcDe2);

        ValorIr := ProcessarConteudo(ANode.Childrens.Find('ValorIR'), tcDe2);

        ValorCsll := ProcessarConteudo(ANode.Childrens.Find('ValorCSLL'), tcDe2);

        Aliquota := ProcessarConteudo(ANode.Childrens.Find('AliquotaServicos'), tcDe4);

        aValor := ProcessarConteudo(ANode.Childrens.Find('ISSRetido'), tcStr);

        if aValor = 'true' then
          IssRetido := stRetencao
        else
          IssRetido := stNormal;
      end;
    end;

    LerCPFCNPJTomador(ANode);

    with Tomador do
    begin
      RazaoSocial := ProcessarConteudo(ANode.Childrens.Find('RazaoSocialTomador'), tcStr);

      with IdentificacaoTomador do
      begin
        InscricaoMunicipal := ProcessarConteudo(ANode.Childrens.Find('InscricaoMunicipalTomador'), tcStr);

        InscricaoEstadual := ProcessarConteudo(ANode.Childrens.Find('InscricaoEstadualTomador'), tcStr);
      end;

      with Contato do
      begin
        Email := ProcessarConteudo(ANode.Childrens.Find('EmailTomador'), tcStr);
      end;
    end;

    LerEnderecoTomador(ANode);
    LerCPFCNPJIntermediario(ANode);

    with IntermediarioServico do
    begin
      InscricaoMunicipal := ProcessarConteudo(ANode.Childrens.Find('InscricaoMunicipalIntermediario'), tcStr);

      EMail := ProcessarConteudo(ANode.Childrens.Find('EmailIntermediario'), tcStr);

      aValor := ProcessarConteudo(ANode.Childrens.Find('ISSRetidoIntermediario'), tcStr);

      if aValor = 'true' then
        IssRetido := stRetencao
      else
        IssRetido := stNormal;
    end;

    with ConstrucaoCivil do
    begin
      nCei := ProcessarConteudo(ANode.Childrens.Find('CodigoCEI'), tcStr);

      nMatri := ProcessarConteudo(ANode.Childrens.Find('MatriculaObra'), tcStr);

      nNumeroEncapsulamento := ProcessarConteudo(ANode.Childrens.Find('NumeroEncapsulamento'), tcStr);
    end;
  end;
end;

procedure TNFSeR_SP.SetxItemListaServico(Codigo: string);
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
