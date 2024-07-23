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

unit Conam.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml, ACBrNFSeXConversao, ACBrNFSeXConsts;

type
  { TNFSeW_Conam }

  TNFSeW_Conam = class(TNFSeWClass)
  private
    FQtdReg30: Integer;
    FValReg30: Double;
  protected
    procedure Configuracao; override;

    function GerarReg30: TACBrXmlNode;
    function GerarReg30Item(const Sigla: string; Aliquota, Valor: Double): TACBrXmlNode;
  public
    function GerarXml: Boolean; override;

    property QtdReg30: Integer read FQtdReg30 write FQtdReg30;
    property ValReg30: Double  read FValReg30 write FValReg30;
  end;

implementation

uses
  ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     Conam
//==============================================================================

{ TNFSeW_Conam }

procedure TNFSeW_Conam.Configuracao;
begin
  inherited Configuracao;

//  PrefixoPadrao := 'nfe';
end;

function TNFSeW_Conam.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
  CpfCnpj, MunTomador, MunPrestador: String;
begin
  Configuracao;

  Opcoes.DecimalChar := ',';
  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSeNode := CreateElement('Reg20Item');

  FDocument.Root := NFSeNode;

  if NFSe.IdentificacaoRps.Tipo = trRPS then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'TipoNFS', 1, 3, 1, 'RPS', ''))
  else
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'TipoNFS', 1, 3, 1, 'RPC', ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'NumRps', 1, 9, 1,
                                    NFSe.IdentificacaoRps.Numero , DSC_NUMRPS));

  if NFSe.IdentificacaoRps.Serie = '' then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'SerRps', 1, 3, 1, '001', DSC_SERIERPS))
  else
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'SerRps', 1, 3, 1,
                                    NFSe.IdentificacaoRps.Serie, DSC_SERIERPS));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'DtEmi', 1, 10, 1,
                        FormatDateTime('dd/mm/yyyy', NFse.DataEmissaoRps), ''));

  if NFSe.Servico.Valores.IssRetido in [stNormal, stDevidoForaMunicipioNaoRetido] then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'RetFonte', 1, 3, 1, 'NAO', ''))
  else
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'RetFonte', 1, 3, 1, 'SIM', ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'CodSrv', 1, 5, 1,
                                            NFSe.Servico.ItemListaServico, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'DiscrSrv', 1, 4000, 1,
   StringReplace(NFSe.Servico.Discriminacao, ';',
        FpAOwner.ConfigGeral.QuebradeLinha, [rfReplaceAll, rfIgnoreCase]), ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'VlNFS', 1, 16, 1,
                                       NFSe.Servico.Valores.ValorServicos, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'VlDed', 1, 16, 1,
                                       NFSe.Servico.Valores.ValorDeducoes, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'DiscrDed', 1, 4000, 1,
   StringReplace(NFSe.Servico.Valores.JustificativaDeducao, ';',
        FpAOwner.ConfigGeral.QuebradeLinha, [rfReplaceAll, rfIgnoreCase]), ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'VlBasCalc', 1, 16, 1,
                                         NFSe.Servico.Valores.BaseCalculo, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'AlqIss', 1, 5, 1,
                                            NFSe.Servico.Valores.Aliquota, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'VlIss', 1, 16, 1,
                                            NFSe.Servico.Valores.ValorIss, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'VlIssRet', 1, 16, 1,
                                      NFSe.Servico.Valores.ValorIssRetido, ''));

  CpfCnpj := Trim(NFSe.Tomador.IdentificacaoTomador.CpfCnpj);

  if (CpfCnpj <> 'CONSUMIDOR') and (CpfCnpj <> 'EXTERIOR') then
    CpfCnpj := OnlyNumber(CpfCnpj);

  {
    Se hover municipios com apostofro deve ser substituido por espaço,
    por exemplo SANTA BARBARA D'OESTE deve informar SANTA BARBARA D OESTE.
  }
  MunPrestador:= UpperCase(StringReplace(NFSe.Prestador.Endereco.xMunicipio, '''', ' ', [rfReplaceAll]));
  MunTomador:= UpperCase(StringReplace(NFSe.Tomador.Endereco.xMunicipio, '''', ' ', [rfReplaceAll]));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'CpfCnpTom', 1, 14, 1,
                                                                  CpfCnpj, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'RazSocTom', 1, 60, 1,
                                                 NFSe.Tomador.RazaoSocial, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'TipoLogtom', 1, 10, 1,
                                     NFSe.Tomador.Endereco.TipoLogradouro, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'LogTom', 1, 60, 1,
                                           NFSe.Tomador.Endereco.Endereco, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'NumEndTom', 1, 10, 1,
                                             NFSe.Tomador.Endereco.Numero, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ComplEndTom', 1, 60, 1,
                                        NFSe.Tomador.Endereco.Complemento, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'BairroTom', 1, 60, 1,
                                             NFSe.Tomador.Endereco.Bairro, ''));

  if CpfCnpj = 'CONSUMIDOR'  then
  begin
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'MunTom', 1, 60, 1,
                                                             MunPrestador, ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'SiglaUFTom', 2, 2, 1,
                                               NFSe.Prestador.Endereco.UF, ''));
  end
  else
  begin
    if CpfCnpj = 'EXTERIOR' then
    begin
      NFSeNode.AppendChild(AddNode(tcStr, '#1', 'MunTom', 1, 60, 1,
                                                               'EXTERIOR', ''));

      NFSeNode.AppendChild(AddNode(tcStr, '#1', 'SiglaUFTom', 2, 2, 1,
                                                                     'EX', ''));
    end
    else
    begin
      NFSeNode.AppendChild(AddNode(tcStr, '#1', 'MunTom', 1, 60, 1,
                                                                 MunTomador, ''));

      NFSeNode.AppendChild(AddNode(tcStr, '#1', 'SiglaUFTom', 2, 2, 1,
                                                   NFSe.Tomador.Endereco.UF, ''));
    end;
  end;

  if (CpfCnpj <> 'CONSUMIDOR') and (CpfCnpj <> 'EXTERIOR') then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'CepTom', 1, 8, 1,
                                    OnlyNumber(NFSe.Tomador.Endereco.CEP), ''))
  else
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'CepTom', 1, 8, 1,
                                                                '00000000',''));

  if (CpfCnpj = 'CONSUMIDOR') then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'Telefone', 1, 10, 1, '', ''))
  else
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'Telefone', 1, 10, 1,
                   RightStr(OnlyNumber(NFSe.Tomador.Contato.Telefone), 8), ''));

  if (CpfCnpj <> 'CONSUMIDOR') and (CpfCnpj <> 'EXTERIOR') then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'InscricaoMunicipal', 1, 20, 1,
                     NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, ''))
  else
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'InscricaoMunicipal', 1, 20, 1,
                                                                       '', ''));

  {
    Segundo o manual: Informar somente se Local de Prestação de Serviços for
    diferente do Endereço do Tomador.

    O correto seria criar uma classe para informar o local da prestação de
    serviço local este diferente do local do tomador e do prestador.
  }
  if NFSe.LogradouLocalPrestacaoServico <> llpTomador then
  begin
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'TipoLogLocPre', 1, 10, 1,
                                   NFSe.Prestador.Endereco.TipoLogradouro, ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'LogLocPre', 1, 60, 1,
                                         NFSe.Prestador.Endereco.Endereco, ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'NumEndLocPre', 1, 10, 1,
                                           NFSe.Prestador.Endereco.Numero, ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ComplEndLocPre', 1, 60, 1,
                                      NFSe.Prestador.Endereco.Complemento, ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'BairroLocPre', 1, 60, 1,
                                           NFSe.Prestador.Endereco.Bairro, ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'MunLocPre', 1, 60, 1,
                                                             MunPrestador, ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'SiglaUFLocpre', 2, 2, 1,
                                               NFSe.Prestador.Endereco.UF, ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'CepLocPre', 1, 8, 1,
                                              NFSe.Prestador.Endereco.CEP, ''));
  end;

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'Email1', 1, 120, 1,
                                               NFSe.Tomador.Contato.Email, ''));

  if NFSe.email.Count > 0 then
  begin
    if NFSe.email.Count > 1 then
    begin
      NFSeNode.AppendChild(AddNode(tcStr, '#1', 'Email2', 1, 120, 1,
                                              NFSe.email.Items[0].emailCC, ''));

      NFSeNode.AppendChild(AddNode(tcStr, '#1', 'Email3', 1, 120, 1,
                                              NFSe.email.Items[1].emailCC, ''));
    end
    else
      NFSeNode.AppendChild(AddNode(tcStr, '#1', 'Email2', 1, 120, 1,
                                              NFSe.email.Items[0].emailCC, ''));
  end;

  QtdReg30 := 0;
  ValReg30 := 0;

  // So gera se houver tributos declarados
  if (NFSe.Servico.Valores.AliquotaPis > 0) or
     (NFSe.Servico.Valores.AliquotaCofins > 0) or
     (NFSe.Servico.Valores.AliquotaCsll > 0) or
     (NFSe.Servico.Valores.AliquotaInss > 0) or
     (NFSe.Servico.Valores.AliquotaIr > 0) then
  begin
    xmlNode := GerarReg30;
    NFSeNode.AppendChild(xmlNode);
  end;

  Result := True;
end;

function TNFSeW_Conam.GerarReg30: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  {
    Caso tenham tributos, estes podem ser declarados no registro 30 do XML
    assim eles serao destacados de forma separada na impressao da NFse
  }
  Result := CreateElement('Reg30');

  {
    Contém os tributos municipais, Estaduais e Federais que devem ser
    destacados na nota fiscal eletrônica impressa.
    Siglas de tributos permitidas:
    COFINS
    CSLL
    INSS
    IR
    PIS
  }

  if NFSe.Servico.Valores.AliquotaPis > 0 then
  begin
    xmlNode := GerarReg30Item('PIS',
      NFSe.Servico.Valores.AliquotaPis, NFSe.Servico.Valores.ValorPis);
    Result.AppendChild(xmlNode);

    Inc(FQtdReg30);
    ValReg30 := ValReg30 + NFSe.Servico.Valores.ValorPis;
  end;

  if NFSe.Servico.Valores.AliquotaCofins > 0 then
  begin
    xmlNode := GerarReg30Item('COFINS',
      NFSe.Servico.Valores.AliquotaCofins, NFSe.Servico.Valores.ValorCofins);
    Result.AppendChild(xmlNode);

    Inc(FQtdReg30);
    ValReg30 := ValReg30 + NFSe.Servico.Valores.ValorCofins;
  end;

  if NFSe.Servico.Valores.AliquotaCsll > 0 then
  begin
    xmlNode := GerarReg30Item('CSLL',
      NFSe.Servico.Valores.AliquotaCsll, NFSe.Servico.Valores.ValorCsll);
    Result.AppendChild(xmlNode);

    Inc(FQtdReg30);
    ValReg30 := ValReg30 + NFSe.Servico.Valores.ValorCsll;
  end;

  if NFSe.Servico.Valores.AliquotaInss > 0 then
  begin
    xmlNode := GerarReg30Item('INSS',
      NFSe.Servico.Valores.AliquotaInss, NFSe.Servico.Valores.ValorInss);
    Result.AppendChild(xmlNode);

    Inc(FQtdReg30);
    ValReg30 := ValReg30 + NFSe.Servico.Valores.ValorInss;
  end;

  if NFSe.Servico.Valores.AliquotaIr > 0 then
  begin
    xmlNode := GerarReg30Item('IR',
      NFSe.Servico.Valores.AliquotaIr, NFSe.Servico.Valores.ValorIr);
    Result.AppendChild(xmlNode);

    Inc(FQtdReg30);
    ValReg30 := ValReg30 + NFSe.Servico.Valores.ValorIr;
  end;
end;

function TNFSeW_Conam.GerarReg30Item(const Sigla: string; Aliquota,
  Valor: Double): TACBrXmlNode;
begin
  Result := CreateElement('Reg30Item');

  Result.AppendChild(AddNode(tcStr, '#1', 'TributoSigla', 1, 6, 1, Sigla, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'TributoAliquota', 1, 5, 1,
                                                                 Aliquota, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'TributoValor', 1, 16, 1, Valor, ''));
end;

end.
