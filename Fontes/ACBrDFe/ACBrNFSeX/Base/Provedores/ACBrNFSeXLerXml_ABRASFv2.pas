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

unit ACBrNFSeXLerXml_ABRASFv2;

interface

uses
  SysUtils, Classes, StrUtils, IniFiles,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXClass,
  ACBrNFSeXLerXml;

type
  { TNFSeR_ABRASFv2 }

  TNFSeR_ABRASFv2 = class(TNFSeRClass)
  private

  protected
    procedure Configuracao; override;

    //======Arquivo XML==========================================
    function LerDataHoraCancelamento(const ANode: TACBrXmlNode): TDateTime; virtual;
    function LerDataHora(const ANode: TACBrXmlNode): TDateTime; virtual;
    function LerDataEmissao(const ANode: TACBrXmlNode): TDateTime; virtual;
    function LerDataEmissaoRps(const ANode: TACBrXmlNode): TDateTime; virtual;
    function LerCompetencia(const ANode: TACBrXmlNode): TDateTime; virtual;
    function LerCodigoPaisServico(const ANode: TACBrXmlNode): Integer; virtual;
    function LerCodigoPaisTomador(const ANode: TACBrXmlNode): Integer; virtual;

    procedure LerInfNfse(const ANode: TACBrXmlNode);

    procedure LerValoresNfse(const ANode: TACBrXmlNode);

    procedure LerPrestadorServico(const ANode: TACBrXmlNode);
    procedure LerEnderecoPrestadorServico(const ANode: TACBrXmlNode; const aTag: string); virtual;
    procedure LerContatoPrestadorServico(const ANode: TACBrXmlNode);
    procedure LerContatoPrestador(const ANode: TACBrXmlNode);

    procedure LerOrgaoGerador(const ANode: TACBrXmlNode);
    procedure LerDeclaracaoPrestacaoServico(const ANode: TACBrXmlNode);
    procedure LerInfDeclaracaoPrestacaoServico(const ANode: TACBrXmlNode);

    procedure LerRps(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoRps(const ANode: TACBrXmlNode);
    procedure LerRpsSubstituido(const ANode: TACBrXmlNode);

    procedure LerListaServicos(const ANode: TACBrXmlNode); virtual;
    procedure LerServicos(const ANode: TACBrXmlNode); virtual;
    procedure LerServico(const ANode: TACBrXmlNode); virtual;
    procedure LerValores(const ANode: TACBrXmlNode);

    procedure LerPrestador(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoPrestador(const ANode: TACBrXmlNode);

    procedure LerTomadorServico(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoTomador(const ANode: TACBrXmlNode);
    procedure LerEnderecoTomador(const ANode: TACBrXmlNode); virtual;
    procedure LerContatoTomador(const ANode: TACBrXmlNode);

    procedure LerIntermediarioServico(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoIntermediario(const ANode: TACBrXmlNode);

    procedure LerConstrucaoCivil(const ANode: TACBrXmlNode);

    procedure LerNfseCancelamento(const ANode: TACBrXmlNode);
    procedure LerConfirmacao(const ANode: TACBrXmlNode);
    procedure LerPedido(const ANode: TACBrXmlNode);
    procedure LerInfConfirmacaoCancelamento(const ANode: TACBrXmlNode);
    procedure LerInfPedidoCancelamento(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoNfse(const ANode: TACBrXmlNode);

    procedure LerNfseSubstituicao(const ANode: TACBrXmlNode);
    procedure LerSubstituicaoNfse(const ANode: TACBrXmlNode);
    //======Arquivo INI===========================================
    procedure LerINISecaoIdentificacaoNFSe(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoIdentificacaoRps(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoRpsSubstituido(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoNFSeSubstituicao(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoNFSeCancelamento(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoPrestador(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoTomador(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoIntermediario(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoTransportadora(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoConstrucaoCivil(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoServico(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoServicos(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoDeducoes(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoQuartos(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoEmail(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoGenericos(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoDespesas(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoItens(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoItemValores(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoDadosDeducao(const AINIRec: TMemIniFile;
      Item: TItemServicoCollectionItem; const AIndice: Integer); virtual;
    procedure LerINISecaoDadosProssionalParceiro(const AINIRec: TMemIniFile; const AIndice: Integer); virtual;
    procedure LerINISecaoComercioExterior(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoLocacaoSubLocacao(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoEvento(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoRodoviaria(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoInformacoesComplementares(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoValores(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoValoresNFSe(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoCondicaoPagamento(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoOrgaoGerador(const AINIRec: TMemIniFile); virtual;
    procedure LerINISecaoParcelas(const AINIRec: TMemIniFile); virtual;
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean; virtual;
    function LerIni: Boolean; override;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBrConsts,
  ACBrNFSeXConversao;

//==============================================================================
// Essa unit tem por finalidade exclusiva de ler o XML da NFS-e e RPS dos provedores:
//     que seguem a versão 2.xx do layout da ABRASF
//==============================================================================

{ TNFSeR_ABRASFv2 }

procedure TNFSeR_ABRASFv2.Configuracao;
begin
  // Executa a Configuração Padrão
  inherited Configuracao;

end;

function TNFSeR_ABRASFv2.LerCodigoPaisServico(
  const ANode: TACBrXmlNode): Integer;
begin
  Result := ObterConteudo(ANode.Childrens.FindAnyNs('CodigoPais'), tcInt);
end;

function TNFSeR_ABRASFv2.LerCodigoPaisTomador(
  const ANode: TACBrXmlNode): Integer;
begin
  Result := ObterConteudo(ANode.Childrens.FindAnyNs('CodigoPais'), tcInt);
end;

function TNFSeR_ABRASFv2.LerCompetencia(const ANode: TACBrXmlNode): TDateTime;
begin
  Result := ObterConteudo(ANode.Childrens.FindAnyNs('Competencia'), tcDat);
end;

function TNFSeR_ABRASFv2.LerDataEmissao(const ANode: TACBrXmlNode): TDateTime;
begin
  Result := ObterConteudo(ANode.Childrens.FindAnyNs('DataEmissao'), tcDatHor);
end;

function TNFSeR_ABRASFv2.LerDataEmissaoRps(
  const ANode: TACBrXmlNode): TDateTime;
begin
  Result := ObterConteudo(ANode.Childrens.FindAnyNs('DataEmissao'), tcDatHor);
end;

function TNFSeR_ABRASFv2.LerDataHora(const ANode: TACBrXmlNode): TDateTime;
begin
  Result := ObterConteudo(ANode.Childrens.FindAnyNs('DataHora'), tcDatHor);
end;

function TNFSeR_ABRASFv2.LerDataHoraCancelamento(
  const ANode: TACBrXmlNode): TDateTime;
begin
  Result := ObterConteudo(ANode.Childrens.FindAnyNs('DataHora'), tcDatHor);
end;

procedure TNFSeR_ABRASFv2.LerConfirmacao(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Confirmacao');

  if AuxNode <> nil then
  begin
    NFSe.NfseCancelamento.ID := ObterConteudoTag(AuxNode.Attributes.Items['Id']);

    LerPedido(AuxNode);
    LerInfConfirmacaoCancelamento(AuxNode);

    with NFSe.NfseCancelamento do
    begin
      DataHora := LerDataHoraCancelamento(AuxNode);

      if (DataHora > 0) or (NFSe.StatusRps = srCancelado) then
        NFSe.SituacaoNfse := snCancelado;
    end;
  end;
end;

procedure TNFSeR_ABRASFv2.LerConstrucaoCivil(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('ConstrucaoCivil');

  if AuxNode <> nil then
  begin
    with NFSe.ConstrucaoCivil do
    begin
      CodigoObra := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoObra'), tcStr);
      Art        := ObterConteudo(AuxNode.Childrens.FindAnyNs('Art'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv2.LerContatoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

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

procedure TNFSeR_ABRASFv2.LerContatoPrestadorServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('ContatoPrestadorServico');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Contato do
    begin
      Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('Telefone'), tcStr);
      Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('Email'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv2.LerContatoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

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

procedure TNFSeR_ABRASFv2.LerDeclaracaoPrestacaoServico(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.FindAnyNs('Rps');

  if AuxNode <> nil then
    LerInfDeclaracaoPrestacaoServico(AuxNode);
end;

procedure TNFSeR_ABRASFv2.LerEnderecoPrestadorServico(const ANode: TACBrXmlNode;
  const aTag: string);
var
  AuxNode: TACBrXmlNode;
  xUF: string;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs(aTag);

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Endereco do
    begin
      Endereco        := ObterConteudo(AuxNode.Childrens.FindAnyNs('Endereco'), tcStr);

      if Endereco = '' then
        Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('Logradouro'), tcStr);

      Numero          := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Complemento     := ObterConteudo(AuxNode.Childrens.FindAnyNs('Complemento'), tcStr);
      Bairro          := ObterConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);
      UF              := ObterConteudo(AuxNode.Childrens.FindAnyNs('Uf'), tcStr);
      CodigoPais      := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoPais'), tcInt);
      CEP             := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cep'), tcStr);
      xMunicipio := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);

      if UF = '' then
        UF := xUF;
    end;
  end;
end;

procedure TNFSeR_ABRASFv2.LerEnderecoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  xUF, xEndereco: string;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Endereco');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('Endereco'), tcStr);

      if Endereco = '' then
        Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('Logradouro'), tcStr);

      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('Complemento'), tcStr);
      Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('Bairro'), tcStr);
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);
      UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('Uf'), tcStr);
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cep'), tcStr);
      xMunicipio := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);
      CodigoPais := LerCodigoPaisTomador(AuxNode);

      if UF = '' then
        UF := xUF;
    end;
  end;

  AuxNode := ANode.Childrens.FindAnyNs('EnderecoExterior');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      xEndereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('EnderecoCompletoExterior'), tcStr);

      if xEndereco <> '' then
        Endereco := xEndereco;

      CodigoPais := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoPais'), tcInt);

      if (CodigoPais <> 1058) and (CodigoPais > 0) then
        UF := 'EX';
    end;
  end;
end;

procedure TNFSeR_ABRASFv2.LerIdentificacaoNfse(const ANode: TACBrXmlNode);
var
  AuxNode, AuxNodeCpfCnpj: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoNfse');

  if AuxNode <> nil then
  begin
    with NFSe.NfseCancelamento.Pedido.IdentificacaoNfse do
    begin
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Cnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cnpj'), tcStr);

      if Cnpj = '' then
      begin
        AuxNodeCpfCnpj := AuxNode.Childrens.FindAnyNs('CpfCnpj');

        if AuxNodeCpfCnpj <> nil then
        begin
          Cnpj := ObterConteudo(AuxNodeCpfCnpj.Childrens.FindAnyNs('Cpf'), tcStr);

          if Cnpj = '' then
            Cnpj := ObterConteudo(AuxNodeCpfCnpj.Childrens.FindAnyNs('Cnpj'), tcStr);
        end;
      end;

      InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv2.LerIdentificacaoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode, AuxNodeCpfCnpj: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoPrestador');

  if AuxNode = nil then
    AuxNode := ANode;

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.IdentificacaoPrestador do
    begin
      CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('Cnpj'), tcStr);

      if CpfCnpj = '' then
      begin
        AuxNodeCpfCnpj := AuxNode.Childrens.FindAnyNs('CpfCnpj');

        if AuxNodeCpfCnpj <> nil then
        begin
          CpfCnpj := ObterConteudo(AuxNodeCpfCnpj.Childrens.FindAnyNs('Cpf'), tcStr);

          if CpfCnpj = '' then
            CpfCnpj := ObterConteudo(AuxNodeCpfCnpj.Childrens.FindAnyNs('Cnpj'), tcStr);
        end;
      end;

      InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
    end;

    if NFSe.Prestador.RazaoSocial = '' then
    begin
      NFSe.Prestador.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);
      NFSe.Prestador.RazaoSocial := StringReplace(NFSe.Prestador.RazaoSocial, '&amp;', '&', [rfReplaceAll]);
    end;
  end;
end;

procedure TNFSeR_ABRASFv2.LerIdentificacaoRps(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoRps');

  if AuxNode <> nil then
  begin
    with NFSe.IdentificacaoRps do
    begin
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Serie  := ObterConteudo(AuxNode.Childrens.FindAnyNs('Serie'), tcStr);
      Tipo   := FpAOwner.StrToTipoRPS(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('Tipo'), tcStr));
    end;
  end;
end;

procedure TNFSeR_ABRASFv2.LerIdentificacaoTomador(const ANode: TACBrXmlNode);
var
  AuxNode, AuxNodeCpfCnpj: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoTomador');

  if AuxNode <> nil then
  begin
    AuxNodeCpfCnpj := AuxNode.Childrens.FindAnyNs('CpfCnpj');

    with NFSe.Tomador.IdentificacaoTomador do
    begin
      if AuxNodeCpfCnpj <> nil then
      begin
        CpfCnpj := ObterConteudo(AuxNodeCpfCnpj.Childrens.FindAnyNs('Cpf'), tcStr);

        if CpfCnpj = '' then
          CpfCnpj := ObterConteudo(AuxNodeCpfCnpj.Childrens.FindAnyNs('Cnpj'), tcStr);
      end;

      InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
      InscricaoEstadual := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoEstadual'), tcStr);
    end;

    if NFSe.Tomador.RazaoSocial = '' then
    begin
      NFSe.Tomador.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);
      NFSe.Tomador.RazaoSocial := StringReplace(NFSe.Tomador.RazaoSocial, '&amp;', '&', [rfReplaceAll]);
    end;

    LerEnderecoTomador(AuxNode);
    LerContatoTomador(AuxNode);
  end;
end;

procedure TNFSeR_ABRASFv2.LerInfConfirmacaoCancelamento(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('InfConfirmacaoCancelamento');

  NFSe.SituacaoNfse := snNormal;

  if AuxNode <> nil then
  begin
    with NFSe.NfseCancelamento do
    begin
      Sucesso  := ObterConteudo(AuxNode.Childrens.FindAnyNs('Sucesso'), tcBool);
      DataHora := LerDataHora(AuxNode);
    end;

    if NFSe.NfseCancelamento.DataHora > 0 then
      NFSe.SituacaoNfse := snCancelado;
  end;
end;

procedure TNFSeR_ABRASFv2.LerInfDeclaracaoPrestacaoServico(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
  sNatureza: string;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('InfDeclaracaoPrestacaoServico');

  if AuxNode = nil then
    AuxNode := ANode;

  if AuxNode <> nil then
  begin
    LerRps(AuxNode);

    if NFSe.Competencia = 0 then
      NFSe.Competencia := LerCompetencia(AuxNode);

    LerListaServicos(AuxNode);
    LerServico(AuxNode);
    LerPrestador(AuxNode);
    LerTomadorServico(AuxNode);
    LerIntermediarioServico(AuxNode);
    LerConstrucaoCivil(AuxNode);

    NFSe.RegimeEspecialTributacao := FpAOwner.StrToRegimeEspecialTributacao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('RegimeEspecialTributacao'), tcStr));

    sNatureza := ObterConteudo(AuxNode.Childrens.FindAnyNs('NaturezaOperacao'), tcStr);

    if sNatureza = '' then
      NFSe.NaturezaOperacao := noNenhum
    else
      NFSe.NaturezaOperacao := StrToNaturezaOperacao(Ok, sNatureza);

    NFSe.OptanteSimplesNacional := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('OptanteSimplesNacional'), tcStr));
    NFSe.IncentivadorCultural := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('IncentivoFiscal'), tcStr));
    NFSe.DataPagamento := ObterConteudo(AuxNode.Childrens.FindAnyNs('DataPagamento'), tcDat);

    if NFSe.InformacoesComplementares = '' then
    begin
      NFSe.InformacoesComplementares := ObterConteudo(AuxNode.Childrens.FindAnyNs('InformacoesComplementares'), tcStr);
      NFSe.InformacoesComplementares := StringReplace(NFSe.InformacoesComplementares,
                                 '&lt;br&gt;', FpQuebradeLinha, [rfReplaceAll]);
      NFSe.InformacoesComplementares := StringReplace(NFSe.InformacoesComplementares,
                                   FpQuebradeLinha, sLineBreak, [rfReplaceAll]);
    end;

    if NFSe.OutrasInformacoes = '' then
    begin
      NFSe.OutrasInformacoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('OutrasInformacoes'), tcStr);
      NFSe.OutrasInformacoes := StringReplace(NFSe.OutrasInformacoes, '&lt;br&gt;',
                                               FpQuebradeLinha, [rfReplaceAll]);
      NFSe.OutrasInformacoes := StringReplace(NFSe.OutrasInformacoes, FpQuebradeLinha,
                                                    sLineBreak, [rfReplaceAll]);
    end;
  end;
end;

procedure TNFSeR_ABRASFv2.LerInfNfse(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  IdAttr: string;
  Ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('InfNfse');

  if AuxNode <> nil then
  begin
    IdAttr := FpAOwner.ConfigGeral.Identificador;
    NFSe.infNFSe.ID := ObterConteudoTag(AuxNode.Attributes.Items[IdAttr]);

    NFSe.Numero            := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
    NFSe.CodigoVerificacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
    NFSe.DataEmissao       := LerDataEmissao(AuxNode);
    NFSe.NfseSubstituida   := ObterConteudo(AuxNode.Childrens.FindAnyNs('NfseSubstituida'), tcStr);

    NFSe.OutrasInformacoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('OutrasInformacoes'), tcStr);
    NFSe.OutrasInformacoes := StringReplace(NFSe.OutrasInformacoes, FpQuebradeLinha,
                                                    sLineBreak, [rfReplaceAll]);
    NFSe.InformacoesComplementares := ObterConteudo(AuxNode.Childrens.FindAnyNs('InformacoesComplementares'), tcStr);
    NFSe.InformacoesComplementares := StringReplace(NFSe.InformacoesComplementares, FpQuebradeLinha,
                                                    sLineBreak, [rfReplaceAll]);
    NFSe.Link := ObterConteudo(AuxNode.Childrens.FindAnyNs('UrlNfse'), tcStr);
    NFSe.Competencia := LerCompetencia(AuxNode);

    NFSe.Servico.Valores.IssRetido := FpAOwner.StrToSituacaoTributaria(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('IssRetido'), tcStr));

    if NFSe.Link = '' then
      NFSe.Link := ObterConteudo(AuxNode.Childrens.FindAnyNs('LinkNota'), tcStr);

    NFSe.Link := StringReplace(NFSe.Link, '&amp;', '&', [rfReplaceAll]);

    LerValoresNfse(AuxNode);

    NFSe.DescricaoCodigoTributacaoMunicipio :=
      ObterConteudo(AuxNode.Childrens.FindAnyNs(ACBrStr('DescricaoCodigoTributacaoMunicípio')), tcStr);
    NFSe.ValorCredito := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorCredito'), tcDe2);

    LerIdentificacaoRps(AuxNode);
    LerServico(AuxNode);

    LerTomadorServico(AuxNode);
    LerPrestadorServico(AuxNode);
    LerEnderecoPrestadorServico(AuxNode, 'EnderecoPrestadorServico');
    LerOrgaoGerador(AuxNode);
    LerDeclaracaoPrestacaoServico(AuxNode);

    NFSe.ChaveAcesso := ObterConteudo(AuxNode.Childrens.FindAnyNs('ChaveAcesso'), tcStr);

    if NFSe.ChaveAcesso = '' then
      NFSe.ChaveAcesso := NFSe.infNFSe.ID;

    if NFSe.IdentificacaoRps.Numero = '' then
      NFSe.IdentificacaoRps.Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumeroRps'), tcStr);
  end;
end;

procedure TNFSeR_ABRASFv2.LerNfseCancelamento(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('NfseCancelamento');

  LerConfirmacao(AuxNode);
end;

procedure TNFSeR_ABRASFv2.LerNfseSubstituicao(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('NfseSubstituicao');

  LerSubstituicaoNfse(AuxNode);
end;

procedure TNFSeR_ABRASFv2.LerInfPedidoCancelamento(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('InfPedidoCancelamento');

  if AuxNode <> nil then
  begin
    LerIdentificacaoNfse(AuxNode);

    with NFSe.NfseCancelamento.Pedido do
      CodigoCancelamento := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoCancelamento'), tcStr);
  end;
end;

procedure TNFSeR_ABRASFv2.LerIntermediarioServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Intermediario');

  if AuxNode <> nil then
  begin
    LerIdentificacaoIntermediario(AuxNode);

    NFSe.Intermediario.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);
    NFSe.Intermediario.CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);
  end;
end;

procedure TNFSeR_ABRASFv2.LerIdentificacaoIntermediario(
  const ANode: TACBrXmlNode);
var
  AuxNode, AuxNodeCpfCnpj: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoIntermediario');

  if AuxNode <> nil then
  begin
    AuxNodeCpfCnpj := AuxNode.Childrens.FindAnyNs('CpfCnpj');

    with NFSe.Intermediario.Identificacao do
    begin
      if AuxNodeCpfCnpj <> nil then
      begin
        CpfCnpj := ObterConteudo(AuxNodeCpfCnpj.Childrens.FindAnyNs('Cpf'), tcStr);

        if CpfCnpj = '' then
          CpfCnpj := ObterConteudo(AuxNodeCpfCnpj.Childrens.FindAnyNs('Cnpj'), tcStr);
      end;

      InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv2.LerOrgaoGerador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('OrgaoGerador');

  if AuxNode <> nil then
  begin
    with NFSe.OrgaoGerador do
    begin
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);
      Uf              := ObterConteudo(AuxNode.Childrens.FindAnyNs('Uf'), tcStr);
    end;
  end;
end;

procedure TNFSeR_ABRASFv2.LerPedido(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Pedido');

  if AuxNode <> nil then
  begin
    LerInfPedidoCancelamento(AuxNode);
  end;
end;

procedure TNFSeR_ABRASFv2.LerPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Prestador');

  if AuxNode <> nil then
  begin
    LerIdentificacaoPrestador(AuxNode);
  end;
end;

procedure TNFSeR_ABRASFv2.LerPrestadorServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('PrestadorServico');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.FindAnyNs('Prestador');


  if AuxNode <> nil then
  begin
    LerIdentificacaoPrestador(AuxNode);

    with NFSe.Prestador do
    begin
      RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);
      RazaoSocial := StringReplace(RazaoSocial, '&amp;', '&', [rfReplaceAll]);
      NomeFantasia := ObterConteudo(AuxNode.Childrens.FindAnyNs('NomeFantasia'), tcStr);
    end;

    LerEnderecoPrestadorServico(AuxNode, 'Endereco');
    LerContatoPrestador(AuxNode);
  end;
end;

procedure TNFSeR_ABRASFv2.LerRps(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Rps');

  if AuxNode = nil then
    AuxNode := ANode;

  if AuxNode <> nil then
  begin
    LerIdentificacaoRps(AuxNode);

    NFSe.DataEmissaoRps := LerDataEmissaoRps(AuxNode);

    NFSe.StatusRps := FpAOwner.StrToStatusRPS(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('Status'), tcStr));

    if NFSe.StatusRps = srCancelado then
      NFSe.SituacaoNfse := snCancelado;

    LerRpsSubstituido(AuxNode);
  end;
end;

procedure TNFSeR_ABRASFv2.LerRpsSubstituido(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('RpsSubstituido');

  if AuxNode <> nil then
  begin
    with NFSe.RpsSubstituido do
    begin
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
      Serie  := ObterConteudo(AuxNode.Childrens.FindAnyNs('Serie'), tcStr);
      Tipo   := FpAOwner.StrToTipoRPS(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('Tipo'), tcStr));
    end;
  end;
end;

procedure TNFSeR_ABRASFv2.LerListaServicos(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('ListaServicos');

  if AuxNode <> nil then
    LerServicos(AuxNode);
end;

procedure TNFSeR_ABRASFv2.LerServicos(const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  AuxNode: TACBrXmlNode;
  i: Integer;
  CodigoItemServico: string;
  Ok: Boolean;
  ValorLiq: Double;
begin
  ANodes := ANode.Childrens.FindAllAnyNs('Servico');

  for i := 0 to Length(ANodes) - 1 do
  begin
    NFSe.Servico.ItemServico.New;

    with NFSe.Servico.ItemServico[i] do
    begin
      AuxNode := ANodes[i].Childrens.FindAnyNs('Valores');

      Quantidade := 1;

      if AuxNode <> nil then
      begin
        ValorUnitario := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorServicos'), tcDe2);
        ValorDeducoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorDeducoes'), tcDe2);
        ValorIss := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorIss'), tcDe2);
        Aliquota := ObterConteudo(AuxNode.Childrens.FindAnyNs('Aliquota'), tcDe4);
        Aliquota := NormatizarAliquota(Aliquota);
        BaseCalculo := ObterConteudo(AuxNode.Childrens.FindAnyNs('BaseCalculo'), tcDe2);

        ValorTotal := Quantidade * ValorUnitario;
      end;

      CodigoItemServico := ObterConteudo(ANodes[i].Childrens.FindAnyNs('ItemListaServico'), tcStr);
      ItemListaServico := NormatizarItemListaServico(CodigoItemServico);
      CodigoCnae := ObterConteudo(ANodes[i].Childrens.FindAnyNs('CodigoCnae'), tcStr);
      Descricao := ObterConteudo(ANodes[i].Childrens.FindAnyNs('Discriminacao'), tcStr);
      Descricao := StringReplace(Descricao, FpQuebradeLinha,
                                                    sLineBreak, [rfReplaceAll]);
    end;

    with NFSe.Servico do
    begin
      xItemListaServico := ItemListaServicoDescricao(ItemListaServico);
      CodigoTributacaoMunicipio := ObterConteudo(ANodes[i].Childrens.FindAnyNs('CodigoTributacaoMunicipio'), tcStr);
      CodigoNBS := ObterConteudo(ANodes[i].Childrens.FindAnyNs('CodigoNbs'), tcStr);
      CodigoMunicipio := ObterConteudo(ANodes[i].Childrens.FindAnyNs('CodigoMunicipio'), tcStr);
      CodigoPais := ObterConteudo(ANodes[i].Childrens.FindAnyNs('CodigoPais'), tcInt);
      ExigibilidadeISS := FpAOwner.StrToExigibilidadeISS(Ok, ObterConteudo(ANodes[i].Childrens.FindAnyNs('ExigibilidadeISS'), tcStr));
      MunicipioIncidencia := ObterConteudo(ANodes[i].Childrens.FindAnyNs('MunicipioIncidencia'), tcInt);
      NumeroProcesso := ObterConteudo(ANodes[i].Childrens.FindAnyNs('NumeroProcesso'), tcStr);
      InfAdicional := ObterConteudo(ANodes[i].Childrens.FindAnyNs('InfAdicional'), tcStr);

      Valores.IssRetido := FpAOwner.StrToSituacaoTributaria(Ok, ObterConteudo(ANodes[i].Childrens.FindAnyNs('IssRetido'), tcStr));

      // Na versão 2 do layout da ABRASF o valor do ISS retido ou não é retornado
      // na tag ValorIss, sendo assim se faz necessário checar o valor da tag IssRetido
      // para saber quais dos dois campos vai receber a informação.
      if Valores.IssRetido = stRetencao then
      begin
        Valores.ValorIssRetido := Valores.ValorIss;
        Valores.ValorIss := 0;
      end
      else
        Valores.ValorIssRetido := 0;

      Valores.RetencoesFederais := Valores.ValorPis + Valores.ValorCofins +
                                   Valores.ValorInss + Valores.ValorIr +
                                   Valores.ValorCsll + Valores.ValorCpp;

      ValorLiq := Valores.ValorServicos - Valores.RetencoesFederais -
                  Valores.OutrasRetencoes - Valores.ValorIssRetido -
                  Valores.DescontoIncondicionado - Valores.DescontoCondicionado;

      if (Valores.ValorLiquidoNfse = 0) or (Valores.ValorLiquidoNfse > ValorLiq) then
        Valores.ValorLiquidoNfse := ValorLiq;

      Valores.ValorTotalNotaFiscal := Valores.ValorServicos -
                                      Valores.DescontoCondicionado -
                                      Valores.DescontoIncondicionado;
    end;
  end;
end;

procedure TNFSeR_ABRASFv2.LerServico(const ANode: TACBrXmlNode);
var
  AuxNode, AuxNode2: TACBrXmlNode;
  Ok: Boolean;
  CodigoItemServico, Responsavel, xUF: string;
  ValorLiq: Double;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Servico');

  if AuxNode <> nil then
  begin
    AuxNode2 := AuxNode.Childrens.FindAnyNs('tcDadosServico');

    if AuxNode2 <> nil then
      AuxNode := AuxNode2;

    LerValores(AuxNode);

    CodigoItemServico := ObterConteudo(AuxNode.Childrens.FindAnyNs('ItemListaServico'), tcStr);

    // Provedor MegaSoft
    if CodigoItemServico = '' then
      CodigoItemServico := OnlyNumber(ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoTributacaoMunicipio'), tcStr));

    with NFSe.Servico do
    begin
      Responsavel := ObterConteudo(AuxNode.Childrens.FindAnyNs('ResponsavelRetencao'), tcStr);

      if Responsavel = '' then
        ResponsavelRetencao := rtNenhum
      else
        ResponsavelRetencao := FpAOwner.StrToResponsavelRetencao(Ok, Responsavel);

      ItemListaServico          := NormatizarItemListaServico(CodigoItemServico);
      xItemListaServico         := ItemListaServicoDescricao(ItemListaServico);
      CodigoCnae                := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoCnae'), tcStr);
      CodigoTributacaoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoTributacaoMunicipio'), tcStr);
      CodigoNBS                 := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoNbs'), tcStr);
      Discriminacao             := ObterConteudo(AuxNode.Childrens.FindAnyNs('Discriminacao'), tcStr);
      Discriminacao := StringReplace(Discriminacao, FpQuebradeLinha,
                                                    sLineBreak, [rfReplaceAll]);

      VerificarSeConteudoEhLista(Discriminacao);

      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);

      if CodigoMunicipio = '' then
        CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('MunicipioPrestacaoServico'), tcStr);

      MunicipioPrestacaoServico := '';

      if CodigoMunicipio <> '' then
      begin
        MunicipioPrestacaoServico := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);
        MunicipioPrestacaoServico := MunicipioPrestacaoServico + '/' + xUF;
      end;

      CodigoPais := LerCodigoPaisServico(AuxNode);
      ExigibilidadeISS := FpAOwner.StrToExigibilidadeISS(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('ExigibilidadeISS'), tcStr));
      IdentifNaoExigibilidade := ObterConteudo(AuxNode.Childrens.FindAnyNs('IdentifNaoExigibilidade'), tcStr);

      MunicipioIncidencia := ObterConteudo(AuxNode.Childrens.FindAnyNs('MunicipioIncidencia'), tcInt);
      xMunicipioIncidencia := '';

      if MunicipioIncidencia > 0 then
      begin
        xMunicipioIncidencia := ObterNomeMunicipioUF(MunicipioIncidencia, xUF);
        xMunicipioIncidencia := xMunicipioIncidencia + '/' + xUF;
      end;

      NumeroProcesso := ObterConteudo(AuxNode.Childrens.FindAnyNs('NumeroProcesso'), tcStr);

      if Valores.IssRetido = stNenhum then
        Valores.IssRetido := FpAOwner.StrToSituacaoTributaria(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('IssRetido'), tcStr));

      // Na versão 2 do layout da ABRASF o valor do ISS retido ou não é retornado
      // na tag ValorIss, sendo assim se faz necessário checar o valor da tag IssRetido
      // para saber quais dos dois campos vai receber a informação.
      if Valores.IssRetido = stRetencao then
      begin
        Valores.ValorIssRetido := Valores.ValorIss;
        Valores.ValorIss := 0;
      end
      else
        Valores.ValorIssRetido := 0;

      Valores.RetencoesFederais := Valores.ValorPis + Valores.ValorCofins +
                                   Valores.ValorInss + Valores.ValorIr +
                                   Valores.ValorCsll + Valores.ValorCpp;

      ValorLiq := Valores.ValorServicos - Valores.RetencoesFederais -
                  Valores.OutrasRetencoes - Valores.ValorIssRetido -
                  Valores.DescontoIncondicionado - Valores.DescontoCondicionado;

      if (Valores.ValorLiquidoNfse = 0) or (Valores.ValorLiquidoNfse > ValorLiq) then
        Valores.ValorLiquidoNfse := ValorLiq;

      Valores.ValorTotalNotaFiscal := Valores.ValorServicos -
                                      Valores.DescontoCondicionado -
                                      Valores.DescontoIncondicionado +
                                      Valores.ValorTaxaTurismo;
    end;

    NFSe.TipoRecolhimento := FpAOwner.SituacaoTributariaDescricao(NFSe.Servico.Valores.IssRetido);
  end;
end;

procedure TNFSeR_ABRASFv2.LerSubstituicaoNfse(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('SubstituicaoNfse');

  if AuxNode <> nil then
  begin
    NFSe.NfseSubstituidora := ObterConteudo(AuxNode.Childrens.FindAnyNs('NfseSubstituidora'), tcStr);
  end;
end;

procedure TNFSeR_ABRASFv2.LerTomadorServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('TomadorServico');

  if AuxNode = nil then
    AuxNode := ANode.Childrens.FindAnyNs('Tomador');

  if AuxNode <> nil then
  begin
    NFSe.Tomador.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);
    NFSe.Tomador.RazaoSocial := StringReplace(NFSe.Tomador.RazaoSocial, '&amp;', '&', [rfReplaceAll]);
    NFSe.Tomador.IdentificacaoTomador.Nif := ObterConteudo(AuxNode.Childrens.FindAnyNs('NifTomador'), tcStr);

    LerIdentificacaoTomador(AuxNode);

    LerEnderecoTomador(AuxNode);
    LerContatoTomador(AuxNode);
  end;
end;

procedure TNFSeR_ABRASFv2.LerValores(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Valores');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      ValorServicos   := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorServicos'), tcDe2);

      if ValorServicos = 0 then
        ValorServicos := ValorLiquidoNfse;

      ValorDeducoes   := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorDeducoes'), tcDe2);

      if AliquotaPis = 0 then
        AliquotaPis := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaPis'), tcDe2);

      RetidoPis := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('RetidoPis'), tcStr));

      if ValorPis = 0 then
        ValorPis := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorPis'), tcDe2);

      if AliquotaCofins = 0 then
        AliquotaCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaCofins'), tcDe2);

      RetidoCofins := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('RetidoCofins'), tcStr));

      if ValorCofins = 0 then
        ValorCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorCofins'), tcDe2);

      if AliquotaInss = 0 then
        AliquotaInss := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaInss'), tcDe2);

      RetidoInss := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('RetidoInss'), tcStr));

      if ValorInss = 0 then
        ValorInss := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorInss'), tcDe2);

      if AliquotaIr = 0 then
        AliquotaIr := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaIr'), tcDe2);

      RetidoIr := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('RetidoIr'), tcStr));

      if ValorIr = 0 then
        ValorIr := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorIr'), tcDe2);

      if AliquotaCsll = 0 then
        AliquotaCsll := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaCsll'), tcDe2);

      RetidoCsll := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('RetidoCsll'), tcStr));

      if ValorCsll = 0 then
        ValorCsll := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorCsll'), tcDe2);

      if ValorIss = 0 then
        ValorIss := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorIss'), tcDe2);

      if AliquotaCpp = 0 then
      AliquotaCpp := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaCpp'), tcDe2);

      RetidoCpp := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('RetidoCpp'), tcStr));

      if ValorCpp = 0 then
        ValorCpp := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorCpp'), tcDe2);

      OutrasRetencoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('OutrasRetencoes'), tcDe2);
      ValorTotalTributos := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorTotTributos'), tcDe2);

      if NFSe.ValoresNfse.BaseCalculo = 0 then
      begin
        BaseCalculo := ObterConteudo(AuxNode.Childrens.FindAnyNs('BaseCalculo'), tcDe2);
        NFSe.ValoresNfse.BaseCalculo := BaseCalculo;
      end;

      if NFSe.ValoresNfse.ValorIss = 0 then
        NFSe.ValoresNfse.ValorIss := ValorIss;

      if Aliquota = 0 then
        Aliquota := ObterConteudo(AuxNode.Childrens.FindAnyNs('Aliquota'), tcDe4);

      Aliquota := NormatizarAliquota(Aliquota);

      if NFSe.ValoresNfse.ValorLiquidoNfse = 0 then
      begin
        ValorLiquidoNfse := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorLiquidoNfse'), tcDe2);
        NFSe.ValoresNfse.ValorLiquidoNfse := ValorLiquidoNfse;
      end;

      DescontoCondicionado   := ObterConteudo(AuxNode.Childrens.FindAnyNs('DescontoCondicionado'), tcDe2);
      DescontoIncondicionado := ObterConteudo(AuxNode.Childrens.FindAnyNs('DescontoIncondicionado'), tcDe2);

      if IssRetido = stNenhum then
        IssRetido := FpAOwner.StrToSituacaoTributaria(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('IssRetido'), tcStr));

      if ValorTaxaTurismo = 0 then
        ValorTaxaTurismo := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorTTS'), tcDe2);
    end;
  end;
end;

procedure TNFSeR_ABRASFv2.LerValoresNfse(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('ValoresNfse');

  if AuxNode <> nil then
  begin
    with NFSe.ValoresNfse do
    begin
      BaseCalculo      := ObterConteudo(AuxNode.Childrens.FindAnyNs('BaseCalculo'), tcDe2);
      Aliquota         := ObterConteudo(AuxNode.Childrens.FindAnyNs('Aliquota'), tcDe4);
      Aliquota         := NormatizarAliquota(Aliquota);
      ValorIss         := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorIss'), tcDe2);
      ValorLiquidoNfse := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorLiquidoNfse'), tcDe2);
    end;

    with NFSe.Servico.Valores do
    begin
      AliquotaIr := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaIr'), tcDe2);
      AliquotaInss := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaInss'), tcDe2);
      AliquotaPis := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaPis'), tcDe2);
      AliquotaCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaCofins'), tcDe2);
      AliquotaCsll := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaCsll'), tcDe2);
      AliquotaCpp := ObterConteudo(AuxNode.Childrens.FindAnyNs('AliquotaCpp'), tcDe2);

      ValorIr := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorIr'), tcDe2);
      ValorInss := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorInss'), tcDe2);
      ValorPis := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorPis'), tcDe2);
      ValorCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorCofins'), tcDe2);
      ValorCsll := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorCsll'), tcDe2);
      ValorCpp := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorCpp'), tcDe2);

      BaseCalculo := NFSe.ValoresNfse.BaseCalculo;
      Aliquota := NFSe.ValoresNfse.Aliquota;
      ValorIss := NFSe.ValoresNfse.ValorIss;
      ValorLiquidoNfse := NFSe.ValoresNfse.ValorLiquidoNfse;
      ValorServicos := ValorLiquidoNfse;
    end;
  end;
end;

function TNFSeR_ABRASFv2.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  FpQuebradeLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  LerParamsTabIni(True);

  Arquivo := NormatizarXml(Arquivo);

  tpXML := TipodeXMLLeitura(Arquivo);

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  FDocument.Clear();
  FDocument.LoadFromXml(Arquivo);

  XmlNode := FDocument.Root;

  if XmlNode = nil then
    raise Exception.Create('Arquivo xml vazio.');

  NFSe.Clear;
  NFSe.tpXML := tpXml;

  if tpXML = txmlNFSe then
    Result := LerXmlNfse(XmlNode)
  else
    Result := LerXmlRps(XmlNode);

  FreeAndNil(FDocument);
end;

function TNFSeR_ABRASFv2.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
begin
  Result := True;

  if not Assigned(ANode) then Exit;

  // O provedor Tecnos tem essa tag entre as tag CompNfse e Nfse.
  AuxNode := ANode.Childrens.FindAnyNs('tcCompNfse');

  if AuxNode = nil then
    AuxNode := ANode;

  AuxNode := AuxNode.Childrens.FindAnyNs('Nfse');

  if AuxNode = nil then
    AuxNode := ANode;

  LerInfNfse(AuxNode);

  LerNfseCancelamento(ANode);
  LerNfseSubstituicao(ANode);

  LerCampoLink;
end;

function TNFSeR_ABRASFv2.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
begin
  Result := True;

  if not Assigned(ANode) then Exit;

  NFSe.Servico.Valores.IssRetido := stNenhum;
  LerInfDeclaracaoPrestacaoServico(ANode);
end;

function TNFSeR_ABRASFv2.LerIni: Boolean;
var
  LIniRec: TMemIniFile;
begin
  LIniRec := TMemIniFile.Create('');
  try
    LerIniArquivoOuString(Arquivo, LIniRec);
    LerINISecaoIdentificacaoNFSe(LINIRec);
    LerINISecaoIdentificacaoRps(LINIRec);
    LerINISecaoRpsSubstituido(LINIRec);
    LerINISecaoNFSeSubstituicao(LINIRec);
    LerINISecaoNFSeCancelamento(LINIRec);
    LerINISecaoPrestador(LINIRec);
    LerINISecaoTomador(LINIRec);
    LerINISecaoIntermediario(LINIRec);
    LerINISecaoTransportadora(LINIRec);
    LerINISecaoConstrucaoCivil(LINIRec);
    LerINISecaoServico(LINIRec);
    LerINISecaoServicos(LINIRec);
    LerINISecaoDeducoes(LINIRec);
    LerINISecaoQuartos(LINIRec);
    LerINISecaoEmail(LINIRec);
    LerINISecaoGenericos(LINIRec);
    LerINISecaoDespesas(LINIRec);
    LerINISecaoItens(LINIRec);
    LerINISecaoItemValores(LINIRec);
    LerINISecaoComercioExterior(LINIRec);
    LerINISecaoLocacaoSubLocacao(LINIRec);
    LerINISecaoEvento(LINIRec);
    LerINISecaoRodoviaria(LINIRec);
    LerINISecaoInformacoesComplementares(LINIRec);
    LerINISecaoValores(LINIRec);
    LerINISecaoValoresNFSe(LINIRec);
    LerINISecaoCondicaoPagamento(LINIRec);
    LerINISecaoOrgaoGerador(LINIRec);
    LerINISecaoParcelas(LINIRec);
    Result := True;
  finally
    LIniRec.Free;
  end;
end;

procedure TNFSeR_ABRASFv2.LerINISecaoIdentificacaoNFSe(const AINIRec: TMemIniFile);
var
  LSecao, LAux: String;
  OK: Boolean;
begin
  LSecao:= 'IdentificacaoNFSe';
  if AIniRec.SectionExists(LSecao) then
  begin
    LAux := AINIRec.ReadString(LSecao, 'TipoXML', '');
    if LAux = 'NFSe' then
    begin
      NFSe.tpXMl := txmlNFSe;

      NFSe.Numero := AINIRec.ReadString(LSecao, 'Numero', '');
      NFSe.SituacaoNfse := StrToStatusNFSe(Ok, AINIRec.ReadString(LSecao, 'StatusNFSe', ''));
      NFSe.CodigoVerificacao := AINIRec.ReadString(LSecao, 'CodigoVerificacao', '');
      NFSe.InfNFSe.Id := AINIRec.ReadString(LSecao, 'ID', NFSe.InfNFSe.ID);
      NFSe.NfseSubstituida := AINIRec.ReadString(LSecao, 'NfseSubstituida', '');
      NFSe.NfseSubstituidora := AINIRec.ReadString(LSecao, 'NfseSubstituidora', '');
      NFSe.ValorCredito := AINIRec.ReadFloat(LSecao, 'ValorCredito', 0);
      NFSe.Link := AINIRec.ReadString(LSecao, 'Link', NFSe.Link);
    end
    else
      NFSe.tpXML := txmlRPS;
  end;
end;

procedure TNFSeR_ABRASFv2.LerINISecaoIdentificacaoRps(const AINIRec: TMemIniFile);
var
  LSecao, sData: String;
  Ok: Boolean;
begin
  LSecao:= 'IdentificacaoRps';

  NFSe.IdentificacaoRps.Numero := AINIRec.ReadString(LSecao, 'Numero', '');
  NFSe.IdentificacaoRps.Serie := AINIRec.ReadString(LSecao, 'Serie', '');
  NFSe.IdentificacaoRps.Tipo := FPAOwner.StrToTipoRPS(Ok, AINIRec.ReadString(LSecao, 'Tipo', ''));

  NFSe.DataEmissao := AINIRec.ReadDateTime(LSecao, 'DataEmissao', 0);
  NFSe.DataEmissaoRps := AINIRec.ReadDateTime(LSecao, 'DataEmissaoRps', 0);
  NFSe.StatusRps := FpAOwner.StrToStatusRPS(Ok, AINIRec.ReadString(LSecao, 'Status', ''));
  NFSe.Competencia := AINIRec.ReadDate(LSecao, 'Competencia', 0);
  NFSe.NaturezaOperacao := StrToNaturezaOperacao(Ok, AINIRec.ReadString(LSecao, 'NaturezaOperacao', ''));
  NFSe.PercentualCargaTributaria := StringToFloatDef(AINIRec.ReadString(LSecao, 'PercentualCargaTributaria', ''), 0);
  NFSe.ValorCargaTributaria := StringToFloatDef(AINIRec.ReadString(LSecao, 'ValorCargaTributaria', ''), 0);
  NFSe.PercentualCargaTributariaMunicipal := StringToFloatDef(AINIRec.ReadString(LSecao, 'PercentualCargaTributariaMunicipal', ''), 0);
  NFSe.ValorCargaTributariaMunicipal := StringToFloatDef(AINIRec.ReadString(LSecao, 'ValorCargaTributariaMunicipal', ''), 0);
  NFSe.PercentualCargaTributariaEstadual := StringToFloatDef(AINIRec.ReadString(LSecao, 'PercentualCargaTributariaEstadual', ''), 0);
  NFSe.ValorCargaTributariaEstadual := StringToFloatDef(AINIRec.ReadString(LSecao, 'ValorCargaTributariaEstadual', ''), 0);
  NFSe.OutrasInformacoes := StringReplace(AINIRec.ReadString(LSecao, 'OutrasInformacoes', ''), FpAOwner.ConfigGeral.QuebradeLinha, sLineBreak, [rfReplaceAll]) ;
  NFSe.InformacoesComplementares := AINIRec.ReadString(LSecao, 'InformacoesComplementares', '');
  NFSe.TipoNota := AINIRec.ReadInteger(LSecao, 'TipoNota', 0);
  NFSe.SiglaUF := AINIRec.ReadString(LSecao, 'SiglaUF', '');
  NFSe.EspecieDocumento := AINIRec.ReadInteger(LSecao, 'EspecieDocumento', 0);
  NFSe.SerieTalonario := AINIRec.ReadInteger(LSecao, 'SerieTalonario', 0);
  NFSe.FormaPagamento := AINIRec.ReadInteger(LSecao, 'FormaPagamento', 0);
  NFSe.NumeroParcelas := AINIRec.ReadInteger(LSecao, 'NumeroParcelas', 0);

  sData := AINIRec.ReadString(LSecao, 'DataPagamento', '');
  if sData <> '' then
    NFSe.DataPagamento := StringToDateTimeDef(sData, 0);
  {
  NFSe.TipoRecolhimento := AINIRec.ReadString(LSecao, 'TipoRecolhimento', '');
  }
end;

procedure TNFSeR_ABRASFv2.LerINISecaoRpsSubstituido(const AINIRec: TMemIniFile);
var
  LSecao: String;
  Ok: Boolean;
begin
  LSecao := 'RpsSubstituido';
  if AINIRec.SectionExists(LSecao) then
  begin
    NFSe.RpsSubstituido.Numero := AINIRec.ReadString(LSecao, 'Numero', '');
    NFSe.RpsSubstituido.Serie := AINIRec.ReadString(LSecao, 'Serie', '');
    NFSe.RpsSubstituido.Tipo := FpAOwner.StrToTipoRPS(Ok, AINIRec.ReadString(LSecao, 'Tipo', ''));
  end;
end;

procedure TNFSeR_ABRASFv2.LerINISecaoNFSeSubstituicao(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv2.LerINISecaoNFSeCancelamento(const AINIRec: TMemIniFile);
var
  LSecao: String;
begin
  LSecao := 'NFSeCancelamento';

  if AINIRec.SectionExists(LSecao) then
  begin
    NFSe.NFSeCancelamento.Pedido.IdentificacaoNfse.Numero := AINIRec.ReadString(LSecao, 'NumeroNFSe', '');
    NFSe.NfseCancelamento.Pedido.IdentificacaoNfse.Cnpj := AINIRec.ReadString(LSecao, 'CNPJ', '');
    NFSe.NFSeCancelamento.Pedido.IdentificacaoNfse.InscricaoMunicipal := AINIRec.ReadString(LSecao, 'InscricaoMunicipal', '');
    NFSe.NFSeCancelamento.Pedido.IdentificacaoNfse.CodigoMunicipio := AINIRec.ReadString(LSecao, 'CodigoMunicipio', '');
    NFSe.NfseCancelamento.Pedido.CodigoCancelamento := AINIRec.ReadString(LSecao, 'CodCancel', '');
    NFSe.NfSeCancelamento.DataHora := AINIRec.ReadDateTime(LSecao, 'DataHora', 0);
  end;
end;

procedure TNFSeR_ABRASFv2.LerINISecaoPrestador(const AINIRec: TMemIniFile);
var
  LSecao: String;
  Ok: Boolean;
begin
  LSecao := 'Prestador';

  if AINIRec.SectionExists(LSecao) then
  begin
    NFSe.Prestador.IdentificacaoPrestador.CpfCnpj := AINIRec.ReadString(LSecao, 'CNPJ', '');
    NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal := AINIRec.ReadString(LSecao, 'InscricaoMunicipal', '');
    NFSe.Prestador.RazaoSocial := AINIRec.ReadString(LSecao, 'RazaoSocial', '');

    NFSe.RegimeEspecialTributacao := FpAOwner.StrToRegimeEspecialTributacao(Ok, AINIRec.ReadString(LSecao, 'Regime', ''));
    NFSe.OptanteSimplesNacional := FpAOwner.StrToSimNao(Ok, AINIRec.ReadString(LSecao, 'OptanteSN', ''));
    NFSe.IncentivadorCultural := FpAOwner.StrToSimNao(Ok, AINIRec.ReadString(LSecao, 'IncentivadorCultural', ''));
    {
    NFSe.Prestador.NomeFantasia := AINIRec.ReadString(LSecao, 'NomeFantasia', '');

    NFSe.Prestador.Endereco.Endereco := AINIRec.ReadString(LSecao, 'Logradouro', '');
    NFSe.Prestador.Endereco.Numero := AINIRec.ReadString(LSecao, 'Numero', '');
    NFSe.Prestador.Endereco.Complemento := AINIRec.ReadString(LSecao, 'Complemento', '');
    NFSe.Prestador.Endereco.Bairro := AINIRec.ReadString(LSecao, 'Bairro', '');
    NFSe.Prestador.Endereco.CodigoMunicipio := AINIRec.ReadString(LSecao, 'CodigoMunicipio', '');
    NFSe.Prestador.Endereco.xMunicipio := AINIRec.ReadString(LSecao, 'xMunicipio', '');
    NFSe.Prestador.Endereco.UF := AINIRec.ReadString(LSecao, 'UF', '');
    NFSe.Prestador.Endereco.CodigoPais := AINIRec.ReadInteger(LSecao, 'CodigoPais', 0);
    NFSe.Prestador.Endereco.CEP := AINIRec.ReadString(LSecao, 'CEP', '');

    NFSe.Prestador.Contato.Telefone := AINIRec.ReadString(LSecao, 'Telefone', '');
    NFSe.Prestador.Contato.Email := AINIRec.ReadString(LSecao, 'Email', '');
    }
  end;
end;

procedure TNFSeR_ABRASFv2.LerINISecaoTomador(const AINIRec: TMemIniFile);
var
  LSecao: String;
  Ok: Boolean;
begin
  LSecao := 'Tomador';

  if AINIRec.SectionExists(LSecao) then
  begin
    NFSe.Tomador.IdentificacaoTomador.CpfCnpj := AINIRec.ReadString(LSecao, 'CNPJCPF', '');
    NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := AINIRec.ReadString(LSecao, 'InscricaoMunicipal', '');
    NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual := AINIRec.ReadString(LSecao, 'InscricaoEstadual', '');
    NFSe.Tomador.IdentificacaoTomador.Nif := AINIRec.ReadString(LSecao, 'NIF', '');
    NFSe.Tomador.RazaoSocial := AINIRec.ReadString(LSecao, 'RazaoSocial', '');

    NFSe.Tomador.Endereco.TipoLogradouro := AINIRec.ReadString(LSecao, 'TipoLogradouro', '');
    NFSe.Tomador.Endereco.Endereco := AINIRec.ReadString(LSecao, 'Logradouro', '');
    NFSe.Tomador.Endereco.Numero := AINIRec.ReadString(LSecao, 'Numero', '');
    NFSe.Tomador.Endereco.Complemento := AINIRec.ReadString(LSecao, 'Complemento', '');
    NFSe.Tomador.Endereco.Bairro := AINIRec.ReadString(LSecao, 'Bairro', '');
    NFSe.Tomador.Endereco.CodigoMunicipio := AINIRec.ReadString(LSecao, 'CodigoMunicipio', '');
    NFSe.Tomador.Endereco.xMunicipio := AINIRec.ReadString(LSecao, 'xMunicipio', '');
    NFSe.Tomador.Endereco.UF := AINIRec.ReadString(LSecao, 'UF', '');
    NFSe.Tomador.Endereco.CEP := AINIRec.ReadString(LSecao, 'CEP', '');
    NFSe.Tomador.Endereco.CodigoPais := AINIRec.ReadInteger(LSecao, 'CodigoPais', 0);

    NFSe.Tomador.Contato.DDD := AINIRec.ReadString(LSecao, 'DDD', '');
    NFSe.Tomador.Contato.TipoTelefone := AINIRec.ReadString(LSecao, 'TipoTelefone', '');
    NFSe.Tomador.Contato.Telefone := AINIRec.ReadString(LSecao, 'Telefone', '');
    NFSe.Tomador.Contato.Email := AINIRec.ReadString(LSecao, 'Email', '');

    NFSe.Tomador.AtualizaTomador := FpAOwner.StrToSimNao(Ok, AINIRec.ReadString(LSecao, 'AtualizaTomador', '1'));
    NFSe.Tomador.TomadorExterior := FpAOwner.StrToSimNao(Ok, AINIRec.ReadString(LSecao, 'TomadorExterior', '2'));
  end;
end;

procedure TNFSeR_ABRASFv2.LerINISecaoIntermediario(const AINIRec: TMemIniFile);
var
  LSecao: String;
begin
  LSecao := 'Intermediario';

  if AINIRec.SectionExists(LSecao) then
  begin
    NFSe.Intermediario.Identificacao.CpfCnpj := AINIRec.ReadString(LSecao, 'CNPJCPF', '');
    NFSe.Intermediario.Identificacao.InscricaoMunicipal := AINIRec.ReadString(LSecao, 'InscricaoMunicipal', '');
    NFSe.Intermediario.RazaoSocial := AINIRec.ReadString(LSecao, 'RazaoSocial', '');
    NFSe.Intermediario.CodigoMunicipio := AINIRec.ReadString(LSecao, 'CodigoMunicipio', '');
  end;
end;

procedure TNFSeR_ABRASFv2.LerINISecaoTransportadora(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv2.LerINISecaoConstrucaoCivil(const AINIRec: TMemIniFile);
var
  LSecao: String;
begin
  LSecao := 'ConstrucaoCivil';

  if AINIRec.SectionExists(LSecao) then
  begin
    NFSe.ConstrucaoCivil.CodigoObra := AINIRec.ReadString(LSecao, 'CodigoObra', '');
    NFSe.ConstrucaoCivil.Art := AINIRec.ReadString(LSecao, 'Art', '');
  end;
end;

procedure TNFSeR_ABRASFv2.LerINISecaoServico(const AINIRec: TMemIniFile);
var
  LSecao: String;
  Ok: Boolean;
begin
  LSecao := 'Servico';

  if AINIRec.SectionExists(LSecao) then
  begin
    NFSe.Servico.ResponsavelRetencao := FpAOwner.StrToResponsavelRetencao(Ok, AINIRec.ReadString(LSecao, 'ResponsavelRetencao', ''));
    NFSe.Servico.ItemListaServico := AINIRec.ReadString(LSecao, 'ItemListaServico', '');
    NFSe.Servico.xItemListaServico := AINIRec.ReadString(LSecao, 'xItemListaServico', '');
    NFSe.Servico.CodigoCnae := AINIRec.ReadString(LSecao, 'CodigoCnae', '');
    NFSe.Servico.CodigoTributacaoMunicipio := AINIRec.ReadString(LSecao, 'CodigoTributacaoMunicipio', '');
    NFSe.Servico.Discriminacao := ChangeLineBreak(AINIRec.ReadString(LSecao, 'Discriminacao', ''), FpAOwner.ConfigGeral.QuebradeLinha);
    NFSe.Servico.CodigoMunicipio := AINIRec.ReadString(LSecao, 'CodigoMunicipio', '');
    NFSe.Servico.CodigoNBS := AINIRec.ReadString(LSecao, 'CodigoNBS', '');
    NFSe.Servico.CodigoPais := AINIRec.ReadInteger(LSecao, 'CodigoPais', 0);
    NFSe.Servico.ExigibilidadeISS := FpAOwner.StrToExigibilidadeISS(Ok, AINIRec.ReadString(LSecao, 'ExigibilidadeISS', '1'));
    NFSe.Servico.MunicipioIncidencia := AINIRec.ReadInteger(LSecao, 'MunicipioIncidencia', 0);
    NFSe.Servico.xMunicipioIncidencia := AINIRec.ReadString(LSecao, 'xMunicipioIncidencia', '');
    NFSe.Servico.NumeroProcesso := AINIRec.ReadString(LSecao, 'NumeroProcesso', '');
    NFSe.Servico.InfAdicional := AINIRec.ReadString(LSecao, 'InfAdicional', '');

    NFSe.Servico.MunicipioPrestacaoServico := AINIRec.ReadString(LSecao, 'MunicipioPrestacaoServico', '');
    NFSe.Servico.ValorTotalRecebido := AINIRec.ReadFloat(LSecao, 'ValorTotalRecebido', 0.0);
  end;
end;

procedure TNFSeR_ABRASFv2.LerINISecaoServicos(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv2.LerINISecaoDeducoes(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv2.LerINISecaoQuartos(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv2.LerINISecaoEmail(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv2.LerINISecaoGenericos(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv2.LerINISecaoDespesas(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv2.LerINISecaoItens(const AINIRec: TMemIniFile);
var
  I: Integer;
  LSecao: String;
  Item: TItemServicoCollectionItem;
begin
  I := 0;
  while True do
  begin
    LSecao:= 'Itens' + IntToStrZero(I + 1, 3);
    if not AINIRec.SectionExists(LSecao) then
      break;

    Item := NFSe.Servico.ItemServico.New;

    Item.Descricao := ChangeLineBreak(AINIRec.ReadString(LSecao, 'Descricao', ''), FpAOwner.ConfigGeral.QuebradeLinha);
    Item.ItemListaServico := AINIRec.ReadString(LSecao, 'ItemListaServico', '');
    Item.xItemListaServico := AINIRec.ReadString(LSecao, 'xItemListaServico', '');
    Item.Quantidade := AINIRec.ReadFloat(LSecao, 'Quantidade', 0);
    Item.ValorUnitario := AINIRec.ReadFloat(LSecao, 'ValorUnitario', 0);
    Item.ValorISS := AINIRec.ReadFloat(LSecao, 'ValorIss', 0);
    Item.Aliquota := AINIRec.ReadFloat(LSecao, 'Aliquota', 0);
    Item.BaseCalculo := AINIRec.ReadFloat(LSecao, 'BaseCalculo', 0);
    Item.ValorTotal := AINIRec.ReadFloat(LSecao, 'ValorTotal', 0);

    LerINISecaoDadosDeducao(AIniRec, Item, I);
    LerINISecaoDadosProssionalParceiro(AIniRec, I);

    Inc(I);
  end;
end;

procedure TNFSeR_ABRASFv2.LerINISecaoDadosDeducao(const AINIRec: TMemIniFile;
  Item: TItemServicoCollectionItem; const AIndice: Integer);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv2.LerINISecaoDadosProssionalParceiro(
  const AINIRec: TMemIniFile; const AIndice: Integer);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv2.LerINISecaoItemValores(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv2.LerINISecaoComercioExterior(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv2.LerINISecaoLocacaoSubLocacao(
  const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv2.LerINISecaoEvento(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv2.LerINISecaoRodoviaria(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv2.LerINISecaoInformacoesComplementares(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv2.LerINISecaoValores(const AINIRec: TMemIniFile);
var
  LSecao: String;
  Ok: Boolean;
begin
  LSecao := 'Valores';

  if AINIRec.SectionExists(LSecao) then
  begin
    NFSe.Servico.Valores.BaseCalculo := AINIRec.ReadFloat(LSecao, 'BaseCalculo', 0.0);
    NFSe.Servico.Valores.IrrfIndenizacao := StringtoFloatDef(AINIRec.ReadString(LSecao, 'IrrfIndenizacao', ''), 0);
    NFSe.Servico.Valores.ValorServicos := AINIRec.ReadFloat(LSecao, 'ValorServicos', 0.0);
    NFSe.Servico.Valores.ValorDeducoes := AINIRec.ReadFloat(LSecao, 'ValorDeducoes', 0.0);
    NFSe.Servico.Valores.AliquotaPis := StringToFloatDef(AINIRec.ReadString(LSecao, 'AliquotaPis', ''), 0);
    NFSe.Servico.Valores.RetidoPis := FpAOwner.StrToSimNao(Ok, AINIRec.ReadString(LSecao, 'RetidoPis', ''));
    NFSe.Servico.Valores.ValorPis := AINIRec.ReadFloat(LSecao, 'ValorPis', 0.0);
    NFSe.Servico.Valores.AliquotaCofins := StringToFloatDef(AINIRec.ReadString(LSecao, 'AliquotaCofins', ''), 0);
    NFSe.Servico.Valores.RetidoCofins := FpAOwner.StrToSimNao(Ok, AINIRec.ReadString(LSecao, 'RetidoCofins', ''));
    NFSe.Servico.Valores.ValorCofins := AINIRec.ReadFloat(LSecao, 'ValorCofins', 0.0);
    NFSe.Servico.Valores.AliquotaInss := StringToFloatDef(AINIRec.ReadString(LSecao, 'AliquotaInss', ''), 0);
    NFSe.Servico.Valores.RetidoInss := FpAOwner.StrToSimNao(Ok, AINIRec.ReadString(LSecao, 'RetidoInss', ''));
    NFSe.Servico.Valores.ValorInss := AINIRec.ReadFloat(LSecao, 'ValorInss', 0.0);
    NFSe.Servico.Valores.AliquotaIr := StringToFloatDef(AINIRec.ReadString(LSecao, 'AliquotaIr', ''), 0);
    NFSe.Servico.Valores.RetidoIr := FpAOwner.StrToSimNao(Ok, AINIRec.ReadString(LSecao, 'RetidoIr', ''));
    NFSe.Servico.Valores.ValorIr := AINIRec.ReadFloat(LSecao, 'ValorIr', 0.0);
    NFSe.Servico.Valores.AliquotaCsll := StringToFloatDef(AINIRec.ReadString(LSecao, 'AliquotaCsll', ''), 0);
    NFSe.Servico.Valores.RetidoCsll := FpAOwner.StrToSimNao(Ok, AINIRec.ReadString(LSecao, 'RetidoCsll', ''));
    NFSe.Servico.Valores.ValorCsll := AINIRec.ReadFloat(LSecao, 'ValorCsll', 0.0);
    NFSe.Servico.Valores.AliquotaCpp := StringToFloatDef(AINIRec.ReadString(LSecao, 'AliquotaCpp', ''), 0);
    NFSe.Servico.Valores.RetidoCpp := FpAOwner.StrToSimNao(Ok, AINIRec.ReadString(LSecao, 'RetidoCpp', ''));
    NFSe.Servico.Valores.ValorCpp := StringToFloatDef(AINIRec.ReadString(LSecao, 'ValorCpp', ''), 0);
    NFSe.Servico.Valores.OutrasRetencoes := AINIRec.ReadFloat(LSecao, 'OutrasRetencoes', 0.0);
    NFSe.Servico.Valores.ValorTotalTributos := AINIRec.ReadFloat(LSecao, 'ValorTotalTributos', 0.0);
    NFSe.Servico.Valores.ValorIss := AINIRec.ReadFloat(LSecao, 'ValorIss', 0.0);
    NFSe.Servico.Valores.ValorTaxaTurismo := AINIRec.ReadFloat(LSecao, 'ValorTaxaTurismo', 0.0);
    NFSe.Servico.Valores.QtdeDiaria := StringToFloatDef(AINIRec.ReadString(LSecao, 'QtdeDiaria', ''), 0);
    NFSe.Servico.Valores.Aliquota := AINIRec.ReadFloat(LSecao, 'Aliquota', 0.0);
    NFSe.Servico.Valores.DescontoIncondicionado := StringToFloatDef(AINIRec.ReadString(LSecao, 'DescontoIncondicionado', ''), 0);
    NFSe.Servico.Valores.DescontoCondicionado := StringToFloatDef(AINIRec.ReadString(LSecao, 'DescontoCondicionado', ''), 0);
    NFSe.Servico.Valores.IssRetido := FpAOwner.StrToSituacaoTributaria(Ok, AINIRec.ReadString(LSecao, 'ISSRetido', ''));
    {
    NFSe.Servico.Valores.ValorIssRetido := AINIRec.ReadFloat(LSecao, 'ValorIssRetido', 0.0);
    NFSe.Servico.Valores.ValorLiquidoNfse := AINIRec.ReadFloat(LSecao, 'ValorLiquidoNfse', 0.0);
    NFSe.Servico.Valores.ValorTotalNotaFiscal := AINIRec.ReadFloat(LSecao, 'ValorTotalNotaFiscal', 0.0);
    NFSe.Servico.Valores.RetencoesFederais := AINIRec.ReadFloat(LSecao, 'RetencoesFederais', 0.0);
    }
  end;
end;

procedure TNFSeR_ABRASFv2.LerINISecaoValoresNFSe(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv2.LerINISecaoCondicaoPagamento(
  const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

procedure TNFSeR_ABRASFv2.LerINISecaoOrgaoGerador(const AINIRec: TMemIniFile);
var
  LSecao: String;
begin
  LSecao := 'OrgaoGerador';

  if AINIRec.SectionExists(LSecao) then
  begin
    NFSe.OrgaoGerador.CodigoMunicipio := AINIRec.ReadString(LSecao, 'CodigoMunicipio', '');
    NFSe.OrgaoGerador.Uf := AINIRec.ReadString(LSecao, 'UF', '');
  end;
end;

procedure TNFSeR_ABRASFv2.LerINISecaoParcelas(const AINIRec: TMemIniFile);
begin
  //Não faz nada neste leiaute...
end;

end.
