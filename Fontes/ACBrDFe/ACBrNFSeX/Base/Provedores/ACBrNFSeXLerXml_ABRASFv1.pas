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

unit ACBrNFSeXLerXml_ABRASFv1;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ENDIF}
  SysUtils, Classes, StrUtils,
  ACBrUtil,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXLerXml, ACBrNFSeXConversao;

type
  { TNFSeR_ABRASFv1 }

  TNFSeR_ABRASFv1 = class(TNFSeRClass)
  private

    procedure SetxItemListaServico(Codigo: string);
  protected
    function LerDatas(const DataStr: string): TDateTime;

    procedure LerInfNfse(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoRps(const ANode: TACBrXmlNode);
    procedure LerServico(const ANode: TACBrXmlNode);
    procedure LerValores(const ANode: TACBrXmlNode);

    procedure LerPrestadorServico(const ANode: TACBrXmlNode);
    procedure LerEnderecoPrestadorServico(const ANode: TACBrXmlNode; aTag: string);
    procedure LerIdentificacaoPrestador(const ANode: TACBrXmlNode);
    procedure LerContatoPrestador(const ANode: TACBrXmlNode);

    procedure LerTomadorServico(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoTomador(const ANode: TACBrXmlNode);
    procedure LerEnderecoTomador(const ANode: TACBrXmlNode);
    procedure LerContatoTomador(const ANode: TACBrXmlNode);

    procedure LerIntermediarioServico(const ANode: TACBrXmlNode);

    procedure LerOrgaoGerador(const ANode: TACBrXmlNode);
    procedure LerConstrucaoCivil(const ANode: TACBrXmlNode);

    procedure LerInfNfseCancelamento(const ANode: TACBrXmlNode);
    procedure LerConfirmacao(const ANode: TACBrXmlNode);
    procedure LerPedido(const ANode: TACBrXmlNode);
    procedure LerInfConfirmacaoCancelamento(const ANode: TACBrXmlNode);
    procedure LerInfPedidoCancelamento(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoNfse(const ANode: TACBrXmlNode);

    procedure LerInfNfseSubstituicao(const ANode: TACBrXmlNode);
    procedure LerSubstituicaoNfse(const ANode: TACBrXmlNode);

    procedure LerRpsSubstituido(const ANode: TACBrXmlNode);
    procedure LerPrestador(const ANode: TACBrXmlNode);

    function TipodeXMLLeitura(aArquivo: string): TtpXML;
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;

  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva de Ler o XML da NFS-e e RPS dos provedores
//     que seguem a versão 1.xx do layout da ABRASF
//==============================================================================

{ TNFSeR_ABRASFv1 }

procedure TNFSeR_ABRASFv1.LerConfirmacao(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('Confirmacao');

  if AuxNode <> nil then
  begin
    LerPedido(AuxNode);
    LerInfConfirmacaoCancelamento(AuxNode);
  end;
end;

procedure TNFSeR_ABRASFv1.LerConstrucaoCivil(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('ConstrucaoCivil');

  if AuxNode <> nil then
  begin
    with NFSe.ConstrucaoCivil do
    begin
      CodigoObra := ProcessarConteudo(AuxNode.Childrens.Find('CodigoObra'), tcStr);
      Art        := ProcessarConteudo(AuxNode.Childrens.Find('Art'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerContatoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('Contato');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Contato do
    begin
      Telefone := ProcessarConteudo(AuxNode.Childrens.Find('Telefone'), tcStr);
      Email    := ProcessarConteudo(AuxNode.Childrens.Find('Email'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerContatoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('Contato');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Contato do
    begin
      Telefone := ProcessarConteudo(AuxNode.Childrens.Find('Telefone'), tcStr);
      Email    := ProcessarConteudo(AuxNode.Childrens.Find('Email'), tcStr);
    end;
  end;
end;

function TNFSeR_ABRASFv1.LerDatas(const DataStr: string): TDateTime;
begin
  if DataStr = '' then
    Result := 0
  else
  begin
    if Length(DataStr) > 10 then
      Result := EncodeDate(StrToInt(copy(DataStr, 01, 4)), StrToInt(copy(DataStr, 06, 2)), StrToInt(copy(DataStr, 09, 2))) +
                EncodeTime(StrToInt(copy(DataStr, 12, 2)), StrToInt(copy(DataStr, 15, 2)), StrToInt(copy(DataStr, 18, 2)), 0)
    else
      Result := EncodeDate(StrToInt(copy(DataStr, 01, 4)), StrToInt(copy(DataStr, 06, 2)), StrToInt(copy(DataStr, 09, 2)));
  end;
end;

procedure TNFSeR_ABRASFv1.LerEnderecoPrestadorServico(const ANode: TACBrXmlNode;
  aTag: string);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find(aTag);

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Endereco do
    begin
      Endereco        := ProcessarConteudo(AuxNode.Childrens.Find('Endereco'), tcStr);
      Numero          := ProcessarConteudo(AuxNode.Childrens.Find('Numero'), tcStr);
      Complemento     := ProcessarConteudo(AuxNode.Childrens.Find('Complemento'), tcStr);
      Bairro          := ProcessarConteudo(AuxNode.Childrens.Find('Bairro'), tcStr);
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('CodigoMunicipio'), tcStr);
      UF              := ProcessarConteudo(AuxNode.Childrens.Find('Uf'), tcStr);
      CodigoPais      := ProcessarConteudo(AuxNode.Childrens.Find('CodigoPais'), tcInt);
      CEP             := ProcessarConteudo(AuxNode.Childrens.Find('Cep'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerEnderecoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('Endereco');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      Endereco        := ProcessarConteudo(AuxNode.Childrens.Find('Endereco'), tcStr);
      Numero          := ProcessarConteudo(AuxNode.Childrens.Find('Numero'), tcStr);
      Complemento     := ProcessarConteudo(AuxNode.Childrens.Find('Complemento'), tcStr);
      Bairro          := ProcessarConteudo(AuxNode.Childrens.Find('Bairro'), tcStr);
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('CodigoMunicipio'), tcStr);
      UF              := ProcessarConteudo(AuxNode.Childrens.Find('Uf'), tcStr);
      CEP             := ProcessarConteudo(AuxNode.Childrens.Find('Cep'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerIdentificacaoNfse(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('IdentificacaoNfse');

  if AuxNode <> nil then
  begin
    with NFSe.NfseCancelamento.Pedido.IdentificacaoNfse do
    begin
      Numero             := ProcessarConteudo(AuxNode.Childrens.Find('Numero'), tcStr);
      Cnpj               := ProcessarConteudo(AuxNode.Childrens.Find('Cnpj'), tcStr);
      InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.Find('InscricaoMunicipal'), tcStr);
      CodigoMunicipio    := ProcessarConteudo(AuxNode.Childrens.Find('CodigoMunicipio'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerIdentificacaoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode, AuxNodeCpfCnpj: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('IdentificacaoPrestador');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.IdentificacaoPrestador do
    begin
      Cnpj := ProcessarConteudo(AuxNode.Childrens.Find('Cnpj'), tcStr);

      if Cnpj = '' then
      begin
        AuxNodeCpfCnpj := AuxNode.Childrens.Find('CpfCnpj');

        with NFSe.Tomador.IdentificacaoTomador do
        begin
          if AuxNodeCpfCnpj <> nil then
          begin
            Cnpj := ProcessarConteudo(AuxNodeCpfCnpj.Childrens.Find('Cpf'), tcStr);

            if Cnpj = '' then
              Cnpj := ProcessarConteudo(AuxNodeCpfCnpj.Childrens.Find('Cnpj'), tcStr);
          end;
        end;
      end;

      InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.Find('InscricaoMunicipal'), tcStr);
    end;
  end
  else
  begin
    with NFSe.Prestador.IdentificacaoPrestador do
    begin
      Cnpj := ProcessarConteudo(ANode.Childrens.Find('Cnpj'), tcStr);

      if Cnpj = '' then
      begin
        AuxNodeCpfCnpj := ANode.Childrens.Find('CpfCnpj');

        with NFSe.Tomador.IdentificacaoTomador do
        begin
          if AuxNodeCpfCnpj <> nil then
          begin
            Cnpj := ProcessarConteudo(AuxNodeCpfCnpj.Childrens.Find('Cpf'), tcStr);

            if Cnpj = '' then
              Cnpj := ProcessarConteudo(AuxNodeCpfCnpj.Childrens.Find('Cnpj'), tcStr);
          end;
        end;
      end;

      InscricaoMunicipal := ProcessarConteudo(ANode.Childrens.Find('InscricaoMunicipal'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerIdentificacaoRps(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.Find('IdentificacaoRps');

  if AuxNode <> nil then
  begin
    with NFSe.IdentificacaoRps do
    begin
      Numero := ProcessarConteudo(AuxNode.Childrens.Find('Numero'), tcStr);
      Serie  := ProcessarConteudo(AuxNode.Childrens.Find('Serie'), tcStr);
      Tipo   := StrToTipoRPS(Ok, ProcessarConteudo(AuxNode.Childrens.Find('Tipo'), tcStr));
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerIdentificacaoTomador(const ANode: TACBrXmlNode);
var
  AuxNode, AuxNodeCpfCnpj: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('IdentificacaoTomador');

  if AuxNode <> nil then
  begin
    AuxNodeCpfCnpj := AuxNode.Childrens.Find('CpfCnpj');

    with NFSe.Tomador.IdentificacaoTomador do
    begin
      if AuxNodeCpfCnpj <> nil then
      begin
        CpfCnpj := ProcessarConteudo(AuxNodeCpfCnpj.Childrens.Find('Cpf'), tcStr);

        if CpfCnpj = '' then
          CpfCnpj := ProcessarConteudo(AuxNodeCpfCnpj.Childrens.Find('Cnpj'), tcStr);
      end;

      InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.Find('InscricaoMunicipal'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerInfConfirmacaoCancelamento(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('InfConfirmacaoCancelamento');

  if AuxNode <> nil then
  begin
    with NFSe.NfseCancelamento do
    begin
      Sucesso  := StrToBool(ProcessarConteudo(AuxNode.Childrens.Find('Sucesso'), tcBoolStr));
      DataHora := LerDatas(ProcessarConteudo(AuxNode.Childrens.Find('DataHora'), tcStr));
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerInfNfse(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.Find('InfNfse');

  if AuxNode <> nil then
  begin
    NFSe.Numero            := ProcessarConteudo(AuxNode.Childrens.Find('Numero'), tcStr);
    NFSe.CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.Find('CodigoVerificacao'), tcStr);
    NFSe.DataEmissao       := LerDatas(ProcessarConteudo(AuxNode.Childrens.Find('DataEmissao'), tcStr));
    NFSe.NfseSubstituida   := ProcessarConteudo(AuxNode.Childrens.Find('NfseSubstituida'), tcStr);

    LerIdentificacaoRps(AuxNode);

    NFSe.DataEmissaoRps           := LerDatas(ProcessarConteudo(AuxNode.Childrens.Find('DataEmissaoRps'), tcStr));
    NFSe.NaturezaOperacao         := StrToNaturezaOperacao(Ok, ProcessarConteudo(AuxNode.Childrens.Find('NaturezaOperacao'), tcStr));
    NFSe.RegimeEspecialTributacao := StrToRegimeEspecialTributacao(Ok, ProcessarConteudo(AuxNode.Childrens.Find('RegimeEspecialTributacao'), tcStr));
    NFSe.OptanteSimplesNacional   := StrToSimNao(Ok, ProcessarConteudo(AuxNode.Childrens.Find('OptanteSimplesNacional'), tcStr));
    NFSe.IncentivadorCultural     := StrToSimNao(Ok, ProcessarConteudo(AuxNode.Childrens.Find('IncentivadorCultural'), tcStr));
    NFSe.Competencia              := LerDatas(ProcessarConteudo(AuxNode.Childrens.Find('Competencia'), tcStr));
    NFSe.NfseSubstituida          := ProcessarConteudo(AuxNode.Childrens.Find('NfseSubstituida'), tcStr);
    NFSe.OutrasInformacoes        := ProcessarConteudo(AuxNode.Childrens.Find('OutrasInformacoes'), tcStr);

    LerServico(AuxNode);

    NFSe.ValorCredito := ProcessarConteudo(AuxNode.Childrens.Find('ValorCredito'), tcDe2);

    LerPrestadorServico(AuxNode);
    LerTomadorServico(AuxNode);
    LerIntermediarioServico(AuxNode);
    LerOrgaoGerador(AuxNode);
    LerConstrucaoCivil(AuxNode);
  end;
end;

procedure TNFSeR_ABRASFv1.LerInfNfseCancelamento(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.Find('NfseCancelamento');

  if AuxNode <> nil then
  begin
    LerConfirmacao(AuxNode);
  end;
end;

procedure TNFSeR_ABRASFv1.LerInfNfseSubstituicao(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.Find('NfseSubstituicao');

  if AuxNode <> nil then
  begin
    LerSubstituicaoNfse(AuxNode);
  end;
end;

procedure TNFSeR_ABRASFv1.LerInfPedidoCancelamento(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('InfPedidoCancelamento');

  if AuxNode <> nil then
  begin
    LerIdentificacaoNfse(AuxNode);

    with NFSe.NfseCancelamento.Pedido do
      CodigoCancelamento := ProcessarConteudo(AuxNode.Childrens.Find('CodigoCancelamento'), tcStr);
  end;
end;

procedure TNFSeR_ABRASFv1.LerIntermediarioServico(const ANode: TACBrXmlNode);
var
  AuxNode, AuxNodeCpfCnpj: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('IntermediarioServico');

  if AuxNode <> nil then
  begin
    NFSe.IntermediarioServico.RazaoSocial := ProcessarConteudo(AuxNode.Childrens.Find('RazaoSocial'), tcStr);

    AuxNodeCpfCnpj := AuxNode.Childrens.Find('CpfCnpj');

    with NFSe.IntermediarioServico do
    begin
      if AuxNodeCpfCnpj <> nil then
      begin
        CpfCnpj := ProcessarConteudo(AuxNodeCpfCnpj.Childrens.Find('Cpf'), tcStr);

        if CpfCnpj = '' then
          CpfCnpj := ProcessarConteudo(AuxNodeCpfCnpj.Childrens.Find('Cnpj'), tcStr);
      end;

      InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.Find('InscricaoMunicipal'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerOrgaoGerador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('OrgaoGerador');

  if AuxNode <> nil then
  begin
    with NFSe.OrgaoGerador do
    begin
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('CodigoMunicipio'), tcStr);
      Uf              := ProcessarConteudo(AuxNode.Childrens.Find('Uf'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerPedido(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('Pedido');

  if AuxNode <> nil then
  begin
    LerInfPedidoCancelamento(AuxNode);
  end;
end;

procedure TNFSeR_ABRASFv1.LerPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('Prestador');

  if AuxNode <> nil then
  begin
    LerIdentificacaoPrestador(AuxNode);
  end;
end;

procedure TNFSeR_ABRASFv1.LerPrestadorServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('PrestadorServico');

  if AuxNode <> nil then
  begin
    LerIdentificacaoPrestador(AuxNode);

    with NFSe.Prestador do
    begin
      RazaoSocial  := ProcessarConteudo(AuxNode.Childrens.Find('RazaoSocial'), tcStr);
      NomeFantasia := ProcessarConteudo(AuxNode.Childrens.Find('NomeFantasia'), tcStr);
    end;

    LerEnderecoPrestadorServico(AuxNode, 'Endereco');
    LerContatoPrestador(AuxNode);
  end;
end;

procedure TNFSeR_ABRASFv1.LerRpsSubstituido(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.Find('RpsSubstituido');

  if AuxNode <> nil then
  begin
    with NFSe.RpsSubstituido do
    begin
      Numero := ProcessarConteudo(AuxNode.Childrens.Find('Numero'), tcStr);
      Serie  := ProcessarConteudo(AuxNode.Childrens.Find('Serie'), tcStr);
      Tipo   := StrToTipoRPS(Ok, ProcessarConteudo(AuxNode.Childrens.Find('Tipo'), tcStr));
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  ItemServico: string;
begin
  AuxNode := ANode.Childrens.Find('Servico');

  if AuxNode <> nil then
  begin
    LerValores(AuxNode);

    ItemServico := ProcessarConteudo(AuxNode.Childrens.Find('ItemListaServico'), tcStr);

    SetxItemListaServico(ItemServico);

    with NFSe.Servico do
    begin
      CodigoCnae                := ProcessarConteudo(AuxNode.Childrens.Find('CodigoCnae'), tcStr);
      CodigoTributacaoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('CodigoTributacaoMunicipio'), tcStr);
      Discriminacao             := ProcessarConteudo(AuxNode.Childrens.Find('Discriminacao'), tcStr);
      CodigoMunicipio           := ProcessarConteudo(AuxNode.Childrens.Find('CodigoMunicipio'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv1.LerSubstituicaoNfse(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('SubstituicaoNfse');

  if AuxNode <> nil then
  begin
    NFSe.NfseSubstituidora := ProcessarConteudo(AuxNode.Childrens.Find('NfseSubstituidora'), tcStr);
  end;
end;

procedure TNFSeR_ABRASFv1.LerTomadorServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('TomadorServico');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.Find('Tomador');

  if AuxNode <> nil then
  begin
    LerIdentificacaoTomador(AuxNode);

    NFSe.Tomador.RazaoSocial := ProcessarConteudo(AuxNode.Childrens.Find('RazaoSocial'), tcStr);

    LerEnderecoTomador(AuxNode);
    LerContatoTomador(AuxNode);
  end;
end;

procedure TNFSeR_ABRASFv1.LerValores(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
  Valor: Currency;
begin
  AuxNode := ANode.Childrens.Find('Valores');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      ValorServicos   := ProcessarConteudo(AuxNode.Childrens.Find('ValorServicos'), tcDe2);
      ValorDeducoes   := ProcessarConteudo(AuxNode.Childrens.Find('ValorDeducoes'), tcDe2);
      ValorPis        := ProcessarConteudo(AuxNode.Childrens.Find('ValorPis'), tcDe2);
      ValorCofins     := ProcessarConteudo(AuxNode.Childrens.Find('ValorCofins'), tcDe2);
      ValorInss       := ProcessarConteudo(AuxNode.Childrens.Find('ValorInss'), tcDe2);
      ValorIr         := ProcessarConteudo(AuxNode.Childrens.Find('ValorIr'), tcDe2);
      ValorCsll       := ProcessarConteudo(AuxNode.Childrens.Find('ValorCsll'), tcDe2);
      IssRetido       := StrToSituacaoTributaria(Ok, ProcessarConteudo(AuxNode.Childrens.Find('IssRetido'), tcStr));
      ValorIss        := ProcessarConteudo(AuxNode.Childrens.Find('ValorIss'), tcDe2);
      OutrasRetencoes := ProcessarConteudo(AuxNode.Childrens.Find('OutrasRetencoes'), tcDe2);
      BaseCalculo     := ProcessarConteudo(AuxNode.Childrens.Find('BaseCalculo'), tcDe2);
      Aliquota        := ProcessarConteudo(AuxNode.Childrens.Find('Aliquota'), tcDe4);
      Valor           := ProcessarConteudo(AuxNode.Childrens.Find('ValorLiquidoNfse'), tcDe2);

      if Valor <> 0 then
        ValorLiquidoNfse := Valor;

      ValorIssRetido := ProcessarConteudo(AuxNode.Childrens.Find('ValorIssRetido'), tcDe2);

      DescontoCondicionado   := ProcessarConteudo(AuxNode.Childrens.Find('DescontoCondicionado'), tcDe2);
      DescontoIncondicionado := ProcessarConteudo(AuxNode.Childrens.Find('DescontoIncondicionado'), tcDe2);
    end;
  end;
end;

function TNFSeR_ABRASFv1.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  tpXML := TipodeXMLLeitura(Arquivo);

  XmlNode := Document.Root;

  if XmlNode = nil then
    raise Exception.Create('Arquivo xml vazio.');

  if tpXML = txmlNFSe then
    Result := LerXmlNfse(XmlNode)
  else
    Result := LerXmlRps(XmlNode);
end;

function TNFSeR_ABRASFv1.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.Find('Nfse');

  LerInfNfse(AuxNode);

  AuxNode := ANode.Childrens.Find('NfseCancelamento');

  LerInfNfseCancelamento(AuxNode);

  AuxNode := ANode.Childrens.Find('NfseSubstituicao');

  LerInfNfseSubstituicao(AuxNode);
end;

function TNFSeR_ABRASFv1.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.Find('InfRps');

  if AuxNode <> nil then
  begin
    LerIdentificacaoRps(AuxNode);

    with NFSe do
    begin
      DataEmissao := LerDatas(ProcessarConteudo(AuxNode.Childrens.Find('DataEmissao'), tcStr));
      NaturezaOperacao := StrToNaturezaOperacao(Ok, ProcessarConteudo(AuxNode.Childrens.Find('NaturezaOperacao'), tcStr));
      RegimeEspecialTributacao := StrToRegimeEspecialTributacao(Ok, ProcessarConteudo(AuxNode.Childrens.Find('RegimeEspecialTributacao'), tcStr));
      OptanteSimplesNacional := StrToSimNao(Ok, ProcessarConteudo(AuxNode.Childrens.Find('OptanteSimplesNacional'), tcStr));
      IncentivadorCultural := StrToSimNao(Ok, ProcessarConteudo(AuxNode.Childrens.Find('IncentivadorCultural'), tcStr));
      Status := StrToStatusRPS(Ok, ProcessarConteudo(AuxNode.Childrens.Find('Status'), tcStr));
    end;

    LerRpsSubstituido(AuxNode);
    LerServico(AuxNode);
    LerPrestador(AuxNode);
    LerTomadorServico(AuxNode);
    LerIntermediarioServico(AuxNode);
    LerConstrucaoCivil(AuxNode);
  end;
end;

procedure TNFSeR_ABRASFv1.SetxItemListaServico(Codigo: string);
var
  Item: Integer;
  ItemServico: string;
begin
  NFSe.Servico.ItemListaServico := Codigo;

  Item := StrToIntDef(OnlyNumber(Nfse.Servico.ItemListaServico), 0);
  if Item < 100 then
    Item := Item * 100 + 1;

  ItemServico := FormatFloat('0000', Item);

  with FAOwner do
  begin
    case ConfigGeral.FormatoItemListaServico of
      filsSemFormatacao: NFSe.Servico.ItemListaServico := ItemServico;
       filsComFormatacaoSemZeroEsquerda: NFSe.Servico.ItemListaServico := IntToStr(Item);
    else
     // filsComFormatacao
      NFSe.Servico.ItemListaServico := Copy(ItemServico, 1, 2) + '.' + Copy(ItemServico, 3, 2);
    end;

    if ConfigGeral.TabServicosExt then
      NFSe.Servico.xItemListaServico := ObterDescricaoServico(ItemServico)
    else
      NFSe.Servico.xItemListaServico := CodItemServToDesc(ItemServico);
  end;
end;

function TNFSeR_ABRASFv1.TipodeXMLLeitura(aArquivo: string): TtpXML;
begin
  if (Pos('CompNfse', Arquivo) > 0) or (Pos('ComplNfse', Arquivo) > 0) or
     (Pos('tcCompNfse', Arquivo) > 0) then
    Result := txmlNFSe
  else
    Result := txmlRPS;
end;

end.
