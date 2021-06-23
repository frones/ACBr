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

  { TNFSeR_SigISS_103 }

  TNFSeR_SigISS_103 = class(TNFSeR_SigISS)
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
  AuxNode := ANode.Childrens.Find('DadosPrestador');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      with Prestador do
      begin
        crc := ProcessarConteudo(AuxNode.Childrens.Find('crc'), tcStr);
        crc_estado := ProcessarConteudo(AuxNode.Childrens.Find('crc_estado'), tcStr);

        with IdentificacaoPrestador do
        begin
          Cnpj := ProcessarConteudo(AuxNode.Childrens.Find('cnpj'), tcStr);
        end;
      end;

      with Servico.Valores do
      begin
        AliquotaSN := ProcessarConteudo(AuxNode.Childrens.Find('aliquota_simples'), tcDe2);
      end;
    end;
  end;
end;

procedure TNFSeR_SigISS.LerIdentificacaoRps(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Dia, Mes, Ano: string;
begin
  AuxNode := ANode.Childrens.Find('DescricaoRps');

  if AuxNode <> nil then
  begin
    with NFSe do
    begin
      with Prestador do
      begin
        crc := ProcessarConteudo(AuxNode.Childrens.Find('crc'), tcStr);
        crc_estado := ProcessarConteudo(AuxNode.Childrens.Find('crc_estado'), tcStr);

        with IdentificacaoPrestador do
        begin
          Cnpj := ProcessarConteudo(AuxNode.Childrens.Find('cnpj'), tcStr);
        end;
      end;

      with Servico.Valores do
      begin
        AliquotaSN := ProcessarConteudo(AuxNode.Childrens.Find('aliquota_simples'), tcDe2);
      end;

      id_sis_legado := ProcessarConteudo(AuxNode.Childrens.Find('id_sis_legado'), tcStr);
      SituacaoTrib := ProcessarConteudo(AuxNode.Childrens.Find('situacao'), tcStr);

      with Servico do
      begin
        Discriminacao := ProcessarConteudo(AuxNode.Childrens.Find('descricaoNF'), tcStr);
        CodigoTributacaoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('servico'), tcStr);
        MunicipioIncidencia := ProcessarConteudo(AuxNode.Childrens.Find('codigo_cidade_local_servico'), tcInt);

        with Valores do
        begin
          ValorServicos := ProcessarConteudo(AuxNode.Childrens.Find('valor'), tcDe4);
          BaseCalculo := ProcessarConteudo(AuxNode.Childrens.Find('base'), tcDe4);
          ValorInss := ProcessarConteudo(AuxNode.Childrens.Find('valor_inss'), tcDe4);
          ValorIr := ProcessarConteudo(AuxNode.Childrens.Find('valor_ir'), tcDe4);
          ValorPis := ProcessarConteudo(AuxNode.Childrens.Find('valor_pis'), tcDe4);
          ValorCofins := ProcessarConteudo(AuxNode.Childrens.Find('valor_cofins'), tcDe4);
          ValorCsll := ProcessarConteudo(AuxNode.Childrens.Find('valor_csll'), tcDe4);
        end;
      end;

      with Tomador do
      begin
        with IdentificacaoTomador do
        begin
          Tipo := ProcessarConteudo(AuxNode.Childrens.Find('tomador_tipo'), tcStr);
          CpfCnpj := ProcessarConteudo(AuxNode.Childrens.Find('tomador_cnpj'), tcStr);
          InscricaoEstadual := ProcessarConteudo(AuxNode.Childrens.Find('tomador_ie'), tcStr);
          InscricaoMunicipal := ProcessarConteudo(AuxNode.Childrens.Find('tomador_im'), tcStr);
        end;

        with Contato do
        begin
          Email := ProcessarConteudo(AuxNode.Childrens.Find('tomador_email'), tcStr);
          Telefone := ProcessarConteudo(AuxNode.Childrens.Find('tomador_fone'), tcStr);
        end;

        RazaoSocial := ProcessarConteudo(AuxNode.Childrens.Find('tomador_razao'), tcStr);
        NomeFantasia := ProcessarConteudo(AuxNode.Childrens.Find('tomador_fantasia'), tcStr);

        with Endereco do
        begin
          Endereco := ProcessarConteudo(AuxNode.Childrens.Find('tomador_endereco'), tcStr);
          Numero := ProcessarConteudo(AuxNode.Childrens.Find('tomador_numero'), tcStr);
          Complemento := ProcessarConteudo(AuxNode.Childrens.Find('tomador_complemento'), tcStr);
          Bairro := ProcessarConteudo(AuxNode.Childrens.Find('tomador_bairro'), tcStr);
          CEP := ProcessarConteudo(AuxNode.Childrens.Find('tomador_CEP'), tcStr);
          CodigoMunicipio := ProcessarConteudo(AuxNode.Childrens.Find('tomador_cod_cidade'), tcStr);
        end;

        with IdentificacaoRps do
        begin
          Numero := ProcessarConteudo(AuxNode.Childrens.Find('rps_num'), tcStr);
          Serie := ProcessarConteudo(AuxNode.Childrens.Find('rps_serie'), tcStr);
        end;
      end;

      Dia := ProcessarConteudo(AuxNode.Childrens.Find('rps_dia'), tcStr);
      Mes := ProcessarConteudo(AuxNode.Childrens.Find('rps_mes'), tcStr);
      Ano := ProcessarConteudo(AuxNode.Childrens.Find('rps_ano'), tcStr);

      DataEmissaoRps := StrToDateDef(Dia + '/' + Mes + '/' + Ano, Date);
    end;
  end;
end;

function TNFSeR_SigISS.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if (Pos('Nota', Arquivo) > 0) then
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

  AuxNode := ANode.Childrens.Find('DadosNota');

  if AuxNode = nil then Exit;

  with NFSe do
  begin
    dhRecebimento     := Now;
    id_sis_legado     := ProcessarConteudo(AuxNode.Childrens.Find('id_sis_legado'), tcInt);
    CodigoVerificacao := ProcessarConteudo(AuxNode.Childrens.Find('autenticidade'), tcStr);
    Numero            := ProcessarConteudo(AuxNode.Childrens.Find('nota'), tcStr);
    DataEmissao       := ProcessarConteudo(AuxNode.Childrens.Find('dt_conversao'), tcDat);
    DataEmissaoRps    := ProcessarConteudo(AuxNode.Childrens.Find('emissao_rps'), tcDat);
    Link              := ProcessarConteudo(AuxNode.Childrens.Find('LinkImpressao'), tcStr);

    aValor := ProcessarConteudo(AuxNode.Childrens.Find('StatusNFe'), tcStr);

    if aValor = 'Cancelada' then
    begin
      Status    := srCancelado;
      Cancelada := snSim;
    end
    else
    begin
      Status    := srNormal;
      Cancelada := snNao;
    end;

    aValor := ProcessarConteudo(AuxNode.Childrens.Find('OpcaoSimples'), tcStr);

    if aValor = 'NAO' then
      OptanteSimplesNacional := snNao
    else
      OptanteSimplesNacional := snSim;

    with ValoresNfse do
    begin
      ValorLiquidoNfse := ProcessarConteudo(AuxNode.Childrens.Find('valor'), tcDe2);
      BaseCalculo      := ValorLiquidoNfse;
      Aliquota         := ProcessarConteudo(AuxNode.Childrens.Find('aliquota_atividade'), tcDe2);
      ValorIss         := ProcessarConteudo(AuxNode.Childrens.Find('iss'), tcDe2);
    end;

    with Servico do
    begin
      ItemListaServico := ProcessarConteudo(AuxNode.Childrens.Find('servico'), tcStr);
      Discriminacao    := ProcessarConteudo(AuxNode.Childrens.Find('descricao'), tcStr);

      with Valores do
      begin
        ValorServicos := NFSe.ValoresNfse.ValorLiquidoNfse;
        BaseCalculo   := ValorServicos;
        Aliquota      := NFSe.ValoresNfse.Aliquota;
        ValorIss      := NFSe.ValoresNfse.ValorIss;

        aValor := ProcessarConteudo(AuxNode.Childrens.Find('ISSRetido'), tcStr);

        if aValor = 'NAO' then
           IssRetido := stNormal
        else
        begin
          IssRetido := stRetencao;
          ValorIssRetido := StrToFloatDef(ProcessarConteudo(AuxNode.Childrens.Find('ISSRetido'), tcStr), 0);
        end;

        ValorLiquidoNfse := ValorServicos -
                            (ValorPis + ValorCofins + ValorInss + ValorIr +
                             ValorCsll + ValorDeducoes + DescontoCondicionado +
                             DescontoIncondicionado + ValorIssRetido);
      end;
    end;

    with Prestador do
    begin
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.Find('prestador_razao'), tcStr);

      with Endereco do
      begin
        Endereco    := ProcessarConteudo(AuxNode.Childrens.Find('prestador_endereco'), tcStr);
        Numero      := ProcessarConteudo(AuxNode.Childrens.Find('prestador_numero'), tcStr);
        Complemento := ProcessarConteudo(AuxNode.Childrens.Find('prestador_complemento'), tcStr);
        Bairro      := ProcessarConteudo(AuxNode.Childrens.Find('prestador_bairro'), tcStr);
        xMunicipio  := ProcessarConteudo(AuxNode.Childrens.Find('prestador_cidade'), tcStr);
        UF          := ProcessarConteudo(AuxNode.Childrens.Find('prestador_estado'), tcStr);
        CEP         := ProcessarConteudo(AuxNode.Childrens.Find('prestador_cep'), tcStr);

        Contato.Email := ProcessarConteudo(AuxNode.Childrens.Find('prestador_email'), tcStr);
      end;
    end;

    with Tomador do
    begin
      RazaoSocial := ProcessarConteudo(AuxNode.Childrens.Find('razao_tomador'), tcStr);

      with IdentificacaoTomador do
        CpfCnpj := ProcessarConteudo(AuxNode.Childrens.Find('cnpj_tomador'), tcStr);

      with Endereco do
      begin
        Endereco    := ProcessarConteudo(AuxNode.Childrens.Find('endereco_tomador'), tcStr);
        Numero      := ProcessarConteudo(AuxNode.Childrens.Find('numero_tomador'), tcStr);
        Complemento := ProcessarConteudo(AuxNode.Childrens.Find('complemento_tomador'), tcStr);
        Bairro      := ProcessarConteudo(AuxNode.Childrens.Find('bairro_tomador'), tcStr);
        xMunicipio  := ProcessarConteudo(AuxNode.Childrens.Find('cidade_tomador'), tcStr);
        UF          := ProcessarConteudo(AuxNode.Childrens.Find('estado_tomador'), tcStr);
        CEP         := ProcessarConteudo(AuxNode.Childrens.Find('cep_tomador'), tcStr);

        Contato.Email := ProcessarConteudo(AuxNode.Childrens.Find('email_tomador'), tcStr);
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
