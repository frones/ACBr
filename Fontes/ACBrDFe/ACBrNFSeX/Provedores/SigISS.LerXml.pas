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

unit SigISS.LerXml;

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
  { TNFSeR_SigISS }

  TNFSeR_SigISS = class(TNFSeRClass)
  protected
    procedure LerDadosPrestador(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoRps(const ANode: TACBrXmlNode);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
  end;

  { TNFSeR_SigISS103 }

  TNFSeR_SigISS103 = class(TNFSeR_SigISS)
  protected

  public

  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     SigISS
//==============================================================================

{ TNFSeR_SigISS }

procedure TNFSeR_SigISS.LerDadosPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('DadosPrestador');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      with Prestador do
      begin
        crc := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('crc'), tcStr);
        crc_estado := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('crc_estado'), tcStr);

        with IdentificacaoPrestador do
        begin
          Cnpj := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('cnpj'), tcStr);
        end;
      end;

      with Servico.Valores do
      begin
        AliquotaSN := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('aliquota_simples'), tcDe2);
      end;
    end;
  end;
end;

procedure TNFSeR_SigISS.LerIdentificacaoRps(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Dia, Mes, Ano: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('DescricaoRps');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      with Prestador do
      begin
        crc := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('crc'), tcStr);
        crc_estado := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('crc_estado'), tcStr);

        with IdentificacaoPrestador do
        begin
          Cnpj := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('cnpj'), tcStr);
        end;
      end;

      with Servico.Valores do
      begin
        AliquotaSN := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('aliquota_simples'), tcDe2);
      end;

      id_sis_legado := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('id_sis_legado'), tcStr);
      SituacaoTrib := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('situacao'), tcStr);

      with Servico do
      begin
        Discriminacao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('descricaoNF'), tcStr);
        CodigoTributacaoMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('servico'), tcStr);
        MunicipioIncidencia := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('codigo_cidade_local_servico'), tcInt);

        with Valores do
        begin
          ValorServicos := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('valor'), tcDe4);
          BaseCalculo := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('base'), tcDe4);
          ValorInss := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('valor_inss'), tcDe4);
          ValorIr := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('valor_ir'), tcDe4);
          ValorPis := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('valor_pis'), tcDe4);
          ValorCofins := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('valor_cofins'), tcDe4);
          ValorCsll := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('valor_csll'), tcDe4);
        end;
      end;

      with Tomador do
      begin
        with IdentificacaoTomador do
        begin
          Tipo := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tomador_tipo'), tcStr);
          CpfCnpj := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tomador_cnpj'), tcStr);
          InscricaoEstadual := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tomador_ie'), tcStr);
          InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tomador_im'), tcStr);
        end;

        with Contato do
        begin
          Email := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tomador_email'), tcStr);
          Telefone := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tomador_fone'), tcStr);
        end;

        RazaoSocial := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tomador_razao'), tcStr);
        NomeFantasia := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tomador_fantasia'), tcStr);

        with Endereco do
        begin
          Endereco := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tomador_endereco'), tcStr);
          Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tomador_numero'), tcStr);
          Complemento := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tomador_complemento'), tcStr);
          Bairro := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tomador_bairro'), tcStr);
          CEP := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tomador_CEP'), tcStr);
          CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('tomador_cod_cidade'), tcStr);
        end;

        with IdentificacaoRps do
        begin
          Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('rps_num'), tcStr);
          Serie := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('rps_serie'), tcStr);
        end;
      end;

      Dia := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('rps_dia'), tcStr);
      Mes := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('rps_mes'), tcStr);
      Ano := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('rps_ano'), tcStr);

      DataEmissaoRps := StrToDateDef(Dia + '/' + Mes + '/' + Ano, Date);
    end;
  end;
end;

function TNFSeR_SigISS.LerXml: Boolean;
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

  if (Pos('Nota', xRetorno) > 0) then
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

function TNFSeR_SigISS.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode: TACBrXmlNode;
  aValor: string;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('DadosNota');

  if AuxNode = nil then Exit;

  with NFSe do
  begin
    SituacaoNfse := snNormal;
    dhRecebimento := Now;
    id_sis_legado := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('id_sis_legado'), tcInt);
    CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('autenticidade'), tcStr);
    Numero := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('nota'), tcStr);
    DataEmissao := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('dt_conversao'), tcDat);
    DataEmissaoRps := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('emissao_rps'), tcDat);
    Link := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('LinkImpressao'), tcStr);

    aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('StatusNFe'), tcStr);

    if aValor = 'Cancelada' then
      SituacaoNfse := snCancelado;

    aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('OpcaoSimples'), tcStr);

    if aValor = 'NAO' then
      OptanteSimplesNacional := snNao
    else
      OptanteSimplesNacional := snSim;

    with ValoresNfse do
    begin
      ValorLiquidoNfse := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('valor'), tcDe2);
      BaseCalculo      := ValorLiquidoNfse;
      Aliquota         := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('aliquota_atividade'), tcDe2);
      ValorIss         := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('iss'), tcDe2);
    end;

    with Servico do
    begin
      ItemListaServico := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('servico'), tcStr);
      Discriminacao    := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('descricao'), tcStr);

      with Valores do
      begin
        ValorServicos := NFSe.ValoresNfse.ValorLiquidoNfse;
        BaseCalculo   := ValorServicos;
        Aliquota      := NFSe.ValoresNfse.Aliquota;
        ValorIss      := NFSe.ValoresNfse.ValorIss;

        aValor := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ISSRetido'), tcStr);

        if aValor = 'NAO' then
           IssRetido := stNormal
        else
        begin
          IssRetido := stRetencao;
          ValorIssRetido := StrToFloatDef(ProcessarConteudo(AuxNode.Childrens.FindAnyNs('ISSRetido'), tcStr), 0);
        end;

        ValorLiquidoNfse := ValorServicos -
                            (ValorPis + ValorCofins + ValorInss + ValorIr +
                             ValorCsll + ValorDeducoes + DescontoCondicionado +
                             DescontoIncondicionado + ValorIssRetido);
      end;
    end;

    with Prestador do
    begin
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('prestador_razao'), tcStr);

      with Endereco do
      begin
        Endereco    := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('prestador_endereco'), tcStr);
        Numero      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('prestador_numero'), tcStr);
        Complemento := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('prestador_complemento'), tcStr);
        Bairro      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('prestador_bairro'), tcStr);
        xMunicipio  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('prestador_cidade'), tcStr);
        UF          := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('prestador_estado'), tcStr);
        CEP         := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('prestador_cep'), tcStr);

        Contato.Email := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('prestador_email'), tcStr);
      end;
    end;

    with Tomador do
    begin
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('razao_tomador'), tcStr);

      with IdentificacaoTomador do
        CpfCnpj := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('cnpj_tomador'), tcStr);

      with Endereco do
      begin
        Endereco    := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('endereco_tomador'), tcStr);
        Numero      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('numero_tomador'), tcStr);
        Complemento := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('complemento_tomador'), tcStr);
        Bairro      := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('bairro_tomador'), tcStr);
        xMunicipio  := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('cidade_tomador'), tcStr);
        UF          := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('estado_tomador'), tcStr);
        CEP         := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('cep_tomador'), tcStr);

        Contato.Email := ProcessarConteudo(AuxNode.Childrens.FindAnyNs('email_tomador'), tcStr);
      end;
    end;
  end;
end;

function TNFSeR_SigISS.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
begin
  Result := True;

  with NFSe do
  begin
    LerDadosPrestador(ANode);
    LerIdentificacaoRps(ANode);
  end;
end;

end.
