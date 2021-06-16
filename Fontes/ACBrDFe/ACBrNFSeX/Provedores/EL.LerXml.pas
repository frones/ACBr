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

unit EL.LerXml;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ENDIF}
  SysUtils, Classes, StrUtils, Math,
  ACBrUtil,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXLerXml,
  ACBrNFSeXLerXml_ABRASFv2;

type
  { Provedor com layout próprio }
  { TNFSeR_EL }

  TNFSeR_EL = class(TNFSeRClass)
  protected

    procedure LerIdNota(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoNfse(const ANode: TACBrXmlNode);
    procedure LerDadosPrestador(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoPrestador(const ANode: TACBrXmlNode);
    procedure LerEnderecoPrestador(const ANode: TACBrXmlNode);
    procedure LerContatoPrestador(const ANode: TACBrXmlNode);
    procedure LerDadosTomador(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoTomador(const ANode: TACBrXmlNode);
    procedure LerEnderecoTomador(const ANode: TACBrXmlNode);
    procedure LerContatoTomador(const ANode: TACBrXmlNode);
    procedure LerServicos(const ANode: TACBrXmlNode);
    procedure LerValores(const ANode: TACBrXmlNode);
    procedure LerObservacao(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoRps(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

  { TNFSeR_ELv2 }

  TNFSeR_ELv2 = class(TNFSeR_ABRASFv2)
  protected

  public

  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     EL
//==============================================================================

{ TNFSeR_EL }

procedure TNFSeR_EL.LerContatoPrestador(const ANode: TACBrXmlNode);
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

procedure TNFSeR_EL.LerContatoTomador(const ANode: TACBrXmlNode);
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

procedure TNFSeR_EL.LerDadosPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.Find('DadosPrestador');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      NaturezaOperacao         := StrToNaturezaOperacao(Ok, ProcessarConteudo(AuxNode.Childrens.Find('NaturezaOperacao'), tcStr));
      RegimeEspecialTributacao := StrToRegimeEspecialTributacao(Ok, ProcessarConteudo(AuxNode.Childrens.Find('RegimeEspecialTributacao'), tcStr));
      OptanteSimplesNacional   := StrToSimNao(Ok, ProcessarConteudo(AuxNode.Childrens.Find('OptanteSimplesNacional'), tcStr));
      IncentivadorCultural     := StrToSimNao(Ok, ProcessarConteudo(AuxNode.Childrens.Find('IncentivadorCultural'), tcStr));

      with Prestador do
      begin
        RazaoSocial  := ProcessarConteudo(AuxNode.Childrens.Find('RazaoSocial'), tcStr);
        NomeFantasia := ProcessarConteudo(AuxNode.Childrens.Find('NomeFantasia'), tcStr);
      end;
    end;

    LerIdentificacaoPrestador(AuxNode);
    LerEnderecoPrestador(AuxNode);
    LerContatoPrestador(AuxNode);
  end;
end;

procedure TNFSeR_EL.LerDadosTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('DadosTomador');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador do
    begin
      RazaoSocial  := ProcessarConteudo(AuxNode.Childrens.Find('RazaoSocial'), tcStr);
      NomeFantasia := ProcessarConteudo(AuxNode.Childrens.Find('NomeFantasia'), tcStr);
    end;

    LerIdentificacaoTomador(AuxNode);
    LerEnderecoTomador(AuxNode);
    LerContatoTomador(AuxNode);
  end;
end;

procedure TNFSeR_EL.LerEnderecoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('Endereco');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Endereco do
    begin
      Endereco        := ProcessarConteudo(AuxNode.Childrens.Find('Logradouro'), tcStr);
      Numero          := ProcessarConteudo(AuxNode.Childrens.Find('LogradouroNumero'), tcStr);
      Complemento     := ProcessarConteudo(AuxNode.Childrens.Find('LogradouroComplemento'), tcStr);
      Bairro          := ProcessarConteudo(AuxNode.Childrens.Find('Bairro'), tcStr);
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('CodigoMunicipio'), tcStr);
      UF              := ProcessarConteudo(AuxNode.Childrens.Find('Uf'), tcStr);
      CEP             := ProcessarConteudo(AuxNode.Childrens.Find('Cep'), tcStr);
      xMunicipio      := ProcessarConteudo(AuxNode.Childrens.Find('Municipio'), tcStr);
    end;
  end;
end;

procedure TNFSeR_EL.LerEnderecoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('Endereco');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      Endereco        := ProcessarConteudo(AuxNode.Childrens.Find('Logradouro'), tcStr);
      Numero          := ProcessarConteudo(AuxNode.Childrens.Find('LogradouroNumero'), tcStr);
      Complemento     := ProcessarConteudo(AuxNode.Childrens.Find('LogradouroComplemento'), tcStr);
      Bairro          := ProcessarConteudo(AuxNode.Childrens.Find('Bairro'), tcStr);
      CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('CodigoMunicipio'), tcStr);
      UF              := ProcessarConteudo(AuxNode.Childrens.Find('Uf'), tcStr);
      CEP             := ProcessarConteudo(AuxNode.Childrens.Find('Cep'), tcStr);
      xMunicipio      := ProcessarConteudo(AuxNode.Childrens.Find('Municipio'), tcStr);
    end;
  end;
end;

procedure TNFSeR_EL.LerIdentificacaoNfse(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.Find('IdentificacaoNfse');

  if AuxNode <> nil then
  begin
    with NFSe.IdentificacaoRps do
    begin
      Numero := ProcessarConteudo(AuxNode.Childrens.Find('Numero'), tcStr);
      Serie  := ProcessarConteudo(AuxNode.Childrens.Find('Serie'), tcStr);
      Tipo   := StrToTipoRPS(Ok, ProcessarConteudo(AuxNode.Childrens.Find('Tipo'), tcStr));

      NFSe.InfID.ID := Numero;
    end;
  end;
end;

procedure TNFSeR_EL.LerIdentificacaoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('IdentificacaoPrestador');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.IdentificacaoPrestador do
    begin
      Cnpj               := ProcessarConteudo(AuxNode.Childrens.Find('CpfCnpj'), tcStr);
      InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.Find('InscricaoMunicipal'), tcStr);
      InscricaoEstadual  := ProcessarConteudo(AuxNode.Childrens.Find('InscricaoEstadual'), tcStr);
    end;
  end;
end;

procedure TNFSeR_EL.LerIdentificacaoRps(const ANode: TACBrXmlNode);
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

procedure TNFSeR_EL.LerIdentificacaoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('IdentificacaoTomador');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.IdentificacaoTomador do
    begin
      CpfCnpj            := ProcessarConteudo(AuxNode.Childrens.Find('CpfCnpj'), tcStr);
      InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.Find('InscricaoMunicipal'), tcStr);
      InscricaoEstadual  := ProcessarConteudo(AuxNode.Childrens.Find('InscricaoEstadual'), tcStr);
    end;
  end;
end;

procedure TNFSeR_EL.LerIdNota(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('idNota');

  if AuxNode <> nil then
  begin
    with NFSe do
      CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.Find('idNota'), tcStr);
  end;
end;

procedure TNFSeR_EL.LerObservacao(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('Observacao');

  if AuxNode <> nil then
    NFSe.OutrasInformacoes := ProcessarConteudo(AuxNode.Childrens.Find('Observacao'), tcStr);
end;

procedure TNFSeR_EL.LerServicos(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: integer;
  aValorTotal: Double;
begin
  AuxNode := ANode.Childrens.Find('Servicos');

  if AuxNode <> nil then
  begin
    ANodes := AuxNode.Childrens.FindAll('Servico');

    for i := 0 to Length(ANodes) - 1 do
    begin
      NFSe.Servico.ItemListaServico := OnlyNumber(ProcessarConteudo(ANodes[i].Childrens.Find('CodigoServico116'), tcStr));
      NFSe.Servico.CodigoCnae := ProcessarConteudo(ANodes[i].Childrens.Find('CodigoCnae'), tcStr);

      NFSe.Servico.ItemServico.New;
      with NFSe.Servico.ItemServico[i] do
      begin
        CodLCServ     := ProcessarConteudo(ANodes[i].Childrens.Find('CodigoServico116'), tcStr);
        CodServ       := ProcessarConteudo(ANodes[i].Childrens.Find('CodigoServicoMunicipal'), tcStr);
        Quantidade    := ProcessarConteudo(ANodes[i].Childrens.Find('Quantidade'), tcDe4);
        Unidade       := ProcessarConteudo(ANodes[i].Childrens.Find('Unidade'), tcStr);
        ValorUnitario := ProcessarConteudo(ANodes[i].Childrens.Find('ValorServico'), tcDe2);
        Descricao     := ProcessarConteudo(ANodes[i].Childrens.Find('Descricao'), tcStr);
        Aliquota      := ProcessarConteudo(ANodes[i].Childrens.Find('Aliquota'), tcDe2);
        ValorIss      := ProcessarConteudo(ANodes[i].Childrens.Find('ValorIssqn'), tcDe4);

        aValorTotal := Quantidade * ValorUnitario;

        ValorTotal := RoundTo(aValorTotal, - 2);
      end;
    end;
  end;
end;

procedure TNFSeR_EL.LerValores(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.Find('Valores');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      ValorServicos    := ProcessarConteudo(AuxNode.Childrens.Find('ValorServicos'), tcDe2);
      ValorIss         := ProcessarConteudo(AuxNode.Childrens.Find('ValorIss'), tcDe2);
      ValorLiquidoNfse := ProcessarConteudo(AuxNode.Childrens.Find('ValorLiquidoNfse'), tcDe2);
      ValorDeducoes    := ProcessarConteudo(AuxNode.Childrens.Find('ValorDeducoes'), tcDe2);
      ValorPis         := ProcessarConteudo(AuxNode.Childrens.Find('ValorPis'), tcDe2);
      ValorCofins      := ProcessarConteudo(AuxNode.Childrens.Find('ValorCofins'), tcDe2);
      ValorInss        := ProcessarConteudo(AuxNode.Childrens.Find('ValorInss'), tcDe2);
      ValorIr          := ProcessarConteudo(AuxNode.Childrens.Find('ValorIr'), tcDe2);
      ValorCsll        := ProcessarConteudo(AuxNode.Childrens.Find('ValorCsll'), tcDe2);
      OutrasRetencoes  := ProcessarConteudo(AuxNode.Childrens.Find('OutrasRetencoes'), tcDe2);
      ValorIssRetido   := ProcessarConteudo(AuxNode.Childrens.Find('ValorIssRetido'), tcDe2);
      OutrosDescontos  := ProcessarConteudo(AuxNode.Childrens.Find('OutrosDescontos'), tcDe2);
      BaseCalculo      := ValorServicos - ValorDeducoes;
    end;
  end;
end;

function TNFSeR_EL.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if (Pos('notasFiscais', Arquivo) > 0) then
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

function TNFSeR_EL.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  aValor: string;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.Find('notasFiscais');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.Find('nfeRpsNotaFiscal');

  if AuxNode <> nil then
  begin
    NFSe.Numero            := ProcessarConteudo(AuxNode.Childrens.Find('numero'), tcStr);
    NFSe.CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.Find('idRps'), tcStr);
    NFSe.DataEmissao       := ProcessarConteudo(AuxNode.Childrens.Find('dataProcessamento'), tcDatHor);

    NFSe.IdentificacaoRps.Numero := ProcessarConteudo(AuxNode.Childrens.Find('rpsNumero'), tcStr);

    aValor := ProcessarConteudo(AuxNode.Childrens.Find('situacao'), tcStr);

    if aValor <> 'A' then
    begin
      NFSe.Cancelada := snSim;
      NFSe.Status    := srCancelado;
    end;

    LerIdNota(AuxNode);
  end;

  AuxNode := ANode.Childrens.Find('Nfse');

  if AuxNode <> nil then
  begin
    NFSe.CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.Find('Id'), tcStr);

    NFSe.Link := NFSe.CodigoVerificacao;

    LerIdentificacaoNfse(AuxNode);
    LerDadosPrestador(AuxNode);
    LerDadosTomador(AuxNode);
    LerServicos(AuxNode);
    LerValores(AuxNode);
    LerObservacao(AuxNode);
  end;
end;

function TNFSeR_EL.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
var
  Ok: Boolean;
begin
  Result := True;

  with NFSe do
  begin
    CodigoVerificacao := ProcessarConteudo(ANode.Childrens.Find('Id'), tcStr);
    DataEmissao       := ProcessarConteudo(ANode.Childrens.Find('DataEmissao'), tcDatHor);
    OutrasInformacoes := ProcessarConteudo(ANode.Childrens.Find('Observacao'), tcStr);
    Status            := StrToStatusRPS(Ok, ProcessarConteudo(ANode.Childrens.Find('Status'), tcStr));

    Servico.Valores.IssRetido := StrToSituacaoTributaria(Ok, ProcessarConteudo(ANode.Childrens.Find('IssRetido'), tcStr));
  end;

  LerIdentificacaoRps(ANode);
  LerDadosPrestador(ANode);
  LerDadosTomador(ANode);
  LerServicos(ANode);
  LerValores(ANode);
end;

end.
