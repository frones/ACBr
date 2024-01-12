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

unit WebFisco.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml, ACBrNFSeXConversao;

type
  { TNFSeW_WebFisco }

  TNFSeW_WebFisco = class(TNFSeWClass)
  protected

  public
    function GerarXml: Boolean; override;

  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     WebFisco
//==============================================================================

{ TNFSeW_WebFisco }

function TNFSeW_WebFisco.GerarXml: Boolean;
var
  NFSeNode: TACBrXmlNode;
  cSimples: Boolean;
  xAtrib, strAux: string;
  i: Integer;
  valAux: Double;
begin
  Configuracao;

  ListaDeAlertas.Clear;

  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  cSimples := (NFSe.OptanteSimplesNacional = snSim);
  xAtrib   := 'xsi:type="xsd:string"';

  FDocument.Clear();

  NFSeNode := CreateElement('EnvNfe');

  FDocument.Root := NFSeNode;

  NFSeNode.AppendChild(AddNode(tcStr, '#', 'usuario', 1, 6, 1,
                                                    Usuario, '', True, xAtrib));

  NFSeNode.AppendChild(AddNode(tcStr, '#', 'pass', 1, 6, 1,
                                                      Senha, '', True, xAtrib));

  // as Tags abaixo só devem ser geradas em ambinte de produção
  if Ambiente = taProducao then
  begin
    NFSeNode.AppendChild(AddNode(tcStr, '#', 'prf', 1, 18, 1,
                                             CNPJPrefeitura, '', True, xAtrib));

    NFSeNode.AppendChild(AddNode(tcStr, '#', 'usr', 1, 18, 1,
              NFSe.Prestador.IdentificacaoPrestador.CpfCnpj, '', True, xAtrib));
  end
  else
  begin
    // No campo prf, usar 00.000.000/0000-00 para a fase de Homologação.
    NFSeNode.AppendChild(AddNode(tcStr, '#', 'prf', 1, 18, 1,
                                       '00.000.000/0000-00', '', True, xAtrib));

    {
      TESTE PARA PRESTADOR DO SIMPLES NACIONAL
      No campo usr, usar 44.232.272/0001-92 para a fase de Homologação.

      TESTE PARA PRESTADOR NÃO OPTANTE DO SIMPLES NACIONAL
      No campo usr, usar 57.657.017/0001-33 para a fase de Homologação.
    }
    if cSimples then
      NFSeNode.AppendChild(AddNode(tcStr, '#', 'usr', 1, 18, 1,
                                       '44.232.272/0001-92', '', True, xAtrib))
    else
      NFSeNode.AppendChild(AddNode(tcStr, '#', 'usr', 1, 18, 1,
                                       '57.657.017/0001-33', '', True, xAtrib));
  end;

  NFSeNode.AppendChild(AddNode(tcStr, '#', 'ctr', 1, 8, 1,
                               NFSe.IdentificacaoRps.Numero, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcStr, '#', 'cnpj', 1, 18, 1,
                  NFSe.Tomador.IdentificacaoTomador.CpfCnpj, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcStr, '#', 'cnpjn', 1, 60, 1,
                                   NFSe.Tomador.RazaoSocial, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcStr, '#', 'ie', 1, 20, 1,
        NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcStr, '#', 'im', 1, 15, 1,
       NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcStr, '#', 'lgr', 1, 60, 1,
                             NFSe.Tomador.Endereco.Endereco, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcStr, '#', 'num', 1, 6, 1,
                               NFSe.Tomador.Endereco.Numero, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcStr, '#', 'cpl', 1, 20, 1,
                          NFSe.Tomador.Endereco.Complemento, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcStr, '#', 'bai', 1, 40, 1,
                               NFSe.Tomador.Endereco.Bairro, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcStr, '#', 'cid', 1, 40, 1,
                           NFSe.Tomador.Endereco.xMunicipio, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcStr, '#', 'est', 1, 2, 1,
                                   NFSe.Tomador.Endereco.UF, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcStr, '#', 'cep', 1, 8, 1,
                                  NFSe.Tomador.Endereco.CEP, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcStr, '#', 'fon', 1, 12, 1,
                              NFSe.Tomador.Contato.Telefone, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcStr, '#', 'mail', 1, 50, 1,
                                 NFSe.Tomador.Contato.Email, '', True, xAtrib));

  NFSeNode.AppendChild(AddNode(tcDatVcto, '#', 'dat', 1, 10, 1,
                                 NFSe.DataEmissao, '', True, xAtrib));

  for i := 1 to 6 do
  begin
    NFSeNode.AppendChild(AddNode(tcStr, '#', 'f' + IntToStr(i) + 'n', 1, 15, 1,
                                                         '', '', True, xAtrib));
    NFSeNode.AppendChild(AddNode(tcStr, '#', 'f' + IntToStr(i) + 'd', 1, 10, 1,
                                                         '', '', True, xAtrib));
    NFSeNode.AppendChild(AddNode(tcStr, '#', 'f' + IntToStr(i) + 'v', 1, 12, 1,
                                                         '', '', True, xAtrib));
  end;

  for i := 0 to 2 do
  begin
    if i <= NFSe.Servico.ItemServico.Count -1 then
      NFSeNode.AppendChild(AddNode(tcStr, '#', 'item' + IntToStr(i+1), 1, 5, 1,
        NFSe.Servico.ItemServico.Items[i].ItemListaServico, '', True, xAtrib))
    else
      NFSeNode.AppendChild(AddNode(tcStr, '#', 'item' + IntToStr(i+1), 1, 5, 1,
                                                         '', '', True, xAtrib));
  end;

  for i := 0 to 2 do
  begin
    if i <= NFSe.Servico.ItemServico.Count -1 then
      NFSeNode.AppendChild(AddNode(tcDe2, '#', 'aliq' + IntToStr(i+1), 1, 5, 1,
                NFSe.Servico.ItemServico.Items[i].Aliquota, '', True, xAtrib))
    else
      NFSeNode.AppendChild(AddNode(tcDe2, '#', 'aliq' + IntToStr(i+1), 1, 5, 1,
                                                          0, '', True, xAtrib));
  end;

  for i := 0 to 2 do
  begin
    if i <= NFSe.Servico.ItemServico.Count -1 then
      NFSeNode.AppendChild(AddNode(tcDe2, '#', 'val' + IntToStr(i+1), 1, 12, 1,
           NFSe.Servico.ItemServico.Items[i].ValorUnitario, '', True, xAtrib))
    else
      NFSeNode.AppendChild(AddNode(tcDe2, '#', 'val' + IntToStr(i+1), 1, 12, 1,
                                                          0, '', True, xAtrib));
  end;

  // Código da localidade de execução do serviço, se no local do estabelecimento
  // do prestador, deixar como 0000...
  if (NFSe.Prestador.Endereco.CodigoMunicipio <> IntToStr(NFSe.Servico.MunicipioIncidencia)) then
    NFSeNode.AppendChild(AddNode(tcStr, '#', 'loc', 1, 4, 1,
                                NFSe.Servico.CodigoMunicipio, '', True, xAtrib))
  else
    NFSeNode.AppendChild(AddNode(tcStr, '#', 'loc', 1, 4, 1,
                                                     '0000', '', True, xAtrib));

  if NFSe.Servico.Valores.IssRetido = stRetencao then
    NFSeNode.AppendChild(AddNode(tcStr, '#', 'ret', 1, 3, 1,
                                                       'SIM', '', True, xAtrib))
  else
    NFSeNode.AppendChild(AddNode(tcStr, '#', 'ret', 1, 3, 1,
                                                      'NAO', '', True, xAtrib));

  NFSeNode.AppendChild(AddNode(tcStr, '#', 'txt', 1, 720, 1,
                      NFSe.Servico.ItemServico[0].Descricao, '', True, xAtrib));

  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'val', 1, 12, 1,
                         NFSe.Servico.Valores.ValorServicos, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'valtrib', 1, 12, 1,
                         NFSe.Servico.Valores.ValorServicos, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'iss', 1, 12, 1,
                              NFSe.Servico.Valores.ValorIss, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'issret', 1, 12, 1,
                        NFSe.Servico.Valores.ValorIssRetido, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'desci', 1, 12, 1,
                NFSe.Servico.Valores.DescontoIncondicionado, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'desco', 1, 12, 1,
                  NFSe.Servico.Valores.DescontoCondicionado, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'binss', 1, 12, 1,
                                                          0, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'birrf', 1, 12, 1,
                                                          0, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'bcsll', 1, 12, 1,
                                                          0, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'bpis', 1, 12, 1,
                                                          0, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'bcofins', 1, 12, 1,
                                                          0, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'ainss', 1, 6, 1,
                                                          0, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'airrf', 1, 6, 1,
                                                          0, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'acsll', 1, 6, 1,
                                                          0, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'apis', 1, 6, 1,
                                                          0, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'acofins', 1, 6, 1,
                                                          0, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'inss', 1, 12, 1,
                             NFSe.Servico.Valores.ValorInss, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'irrf', 1, 12, 1,
                               NFSe.Servico.Valores.ValorIr, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'csll', 1, 12, 1,
                             NFSe.Servico.Valores.ValorCsll, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'pis', 1, 12, 1,
                              NFSe.Servico.Valores.ValorPis, '', True, xAtrib));
  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'cofins', 1, 12, 1,
                           NFSe.Servico.Valores.ValorCofins, '', True, xAtrib));

  for i := 4 to 8 do
    NFSeNode.AppendChild(AddNode(tcStr, '#', 'item' + IntToStr(i), 1, 5, 1,
                                                         '', '', True, xAtrib));

  for i := 4 to 8 do
    NFSeNode.AppendChild(AddNode(tcDe2, '#', 'aliq' + IntToStr(i), 1, 5, 1,
                                                          0, '', True, xAtrib));

  for i := 4 to 8 do
    NFSeNode.AppendChild(AddNode(tcDe2, '#', 'val' + IntToStr(i), 1, 5, 1,
                                                          0, '', True, xAtrib));

  for i := 1 to 8 do
  begin
    if (i = 1) and cSimples then
      NFSeNode.AppendChild(AddNode(tcStr, '#', 'iteser' + IntToStr(i), 1, 5, 1,
                               NFSe.Servico.ItemListaServico, '', True, xAtrib))
    else
      NFSeNode.AppendChild(AddNode(tcStr, '#', 'iteser' + IntToStr(i), 1, 5, 1,
                                                         '', '', True, xAtrib));
  end;

  for i := 1 to 8 do
  begin
    if (i = 1) and cSimples then
      NFSeNode.AppendChild(AddNode(tcDe2, '#', 'alqser' + IntToStr(i), 1, 5, 1,
                               NFSe.Servico.Valores.Aliquota, '', True, xAtrib))
    else
      NFSeNode.AppendChild(AddNode(tcDe2, '#', 'alqser' + IntToStr(i), 1, 5, 1,
                                                          0, '', True, xAtrib));
  end;

  for i := 1 to 8 do
  begin
    if (i = 1) and cSimples then
      NFSeNode.AppendChild(AddNode(tcDe2, '#', 'valser' + IntToStr(i), 1, 12, 1,
                          NFSe.Servico.Valores.ValorServicos, '', True, xAtrib))
    else
      NFSeNode.AppendChild(AddNode(tcDe2, '#', 'valser' + IntToStr(i), 1, 12, 1,
                                                          0, '', True, xAtrib));
  end;

  NFSeNode.AppendChild(AddNode(tcStr, '#', 'paisest', 1, 60, 1,
                                                         '', '', True, xAtrib));

  if cSimples then
    valAux := NFSe.Prestador.ValorReceitaBruta
  else
    valAux := 0.00;

  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'ssrecbr', 1, 12, 1,
                                                     valAux, '', True, xAtrib));

  if cSimples then
    strAux := NFSe.Prestador.Anexo
  else
    strAux :='';

  NFSeNode.AppendChild(AddNode(tcStr, '#', 'ssanexo', 1, 15, 1,
                                                     strAux, '', True, xAtrib));

  if cSimples then
    strAux := FormatDateTime('DD/MM/YYYY', NFSe.Prestador.DataInicioAtividade)
  else
    strAux :=' ';

  NFSeNode.AppendChild(AddNode(tcStr, '#', 'ssdtini', 1, 10, 1,
                                                     strAux, '', True, xAtrib));

  NFSeNode.AppendChild(AddNode(tcStr, '#', 'percded', 1, 6, 1,
                                                         '', '', True, xAtrib));

  for i := 1 to 5 do
  begin
    NFSeNode.AppendChild(AddNode(tcStr, '#', 'itemsac' + IntToStr(i), 1, 60, 1,
                                                         '', '', True, xAtrib));
    NFSeNode.AppendChild(AddNode(tcStr, '#', 'itemsaq' + IntToStr(i), 1, 60, 1,
                                                         '', '', True, xAtrib));
    NFSeNode.AppendChild(AddNode(tcStr, '#', 'itemsav' + IntToStr(i), 1, 60, 1,
                                                         '', '', True, xAtrib));
    // A tag abaixo foi incluida para atender o provedor FGMaiss
    NFSeNode.AppendChild(AddNode(tcStr, '#', 'itemsat' + IntToStr(i), 1, 60, 1,
                                                         '', '', True, xAtrib));
  end;

  // A tag abaixo foi incluida para atender o provedor FGMaiss
  NFSeNode.AppendChild(AddNode(tcStr, '#', 'sslimite', 1, 6, 1,
                                                      'NAO', '', True, xAtrib));

  // A tag abaixo foi incluida para atender o provedor FGMaiss
  if NFSe.Servico.Valores.OutrosDescontos > 0 then
    NFSeNode.AppendChild(AddNode(tcDe2, '#', 'descoutros', 1, 12, 1,
                        NFSe.Servico.Valores.OutrosDescontos, '', True, xAtrib))
  else
    NFSeNode.AppendChild(AddNode(tcStr, '#', 'descoutros', 1, 12, 1,
                                                         '', '', True, xAtrib));

  // As tag abaixo foram incluidas para atender o provedor FGMaiss
  for i := 7 to 12 do
  begin
    NFSeNode.AppendChild(AddNode(tcStr, '#', 'f' + IntToStr(i) + 'n', 1, 15, 1,
                                                         '', '', True, xAtrib));
    NFSeNode.AppendChild(AddNode(tcStr, '#', 'f' + IntToStr(i) + 'd', 1, 10, 1,
                                                         '', '', True, xAtrib));
    NFSeNode.AppendChild(AddNode(tcStr, '#', 'f' + IntToStr(i) + 'v', 1, 12, 1,
                                                         '', '', True, xAtrib));
  end;

  Result := True;
end;

end.
