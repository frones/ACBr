{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit pnfsNFSeW_Governa;

interface

uses
{$IFDEF FPC}
  LResources, 
  Controls, 
{$ELSE}

{$ENDIF}
  SysUtils, 
  Classes, 
  StrUtils,
  ACBrConsts,
  pnfsNFSeW, 
  pcnAuxiliar, 
  pcnConversao, 
  pcnGerador,
  pnfsNFSe, 
  pnfsConversao, 
  pnfsConsts;

type
  { TNFSeW_Governa }

  TNFSeW_Governa = class(TNFSeWClass)
  private
  protected

    procedure GerarTomador;

    procedure GerarListaServicos;
    procedure GerarValoresServico;

    procedure GerarXML_Governa;

  public
    constructor Create(ANFSeW: TNFSeW); override;

    function ObterNomeArquivo: String; override;
    function GerarXml: Boolean; override;
  end;

implementation

uses
  ACBrUtil.Strings;

{==============================================================================}
{ Essa unit tem por finalidade exclusiva de gerar o XML do RPS segundo o       }
{ layout do Governa.                                                       }
{ Sendo assim só será criado uma nova unit para um novo layout.                }
{==============================================================================}

{ TNFSeW_Governa }

procedure TNFSeW_Governa.GerarTomador;
begin
  Gerador.wCampo(tcStr, '', 'NumDocTmd', 11, 014, 1, NFSe.Tomador.IdentificacaoTomador.CpfCnpj,'');
  Gerador.wCampo(tcStr, '', 'InsEstTmd', 01, 020, 1, NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual,'');
  Gerador.wCampo(tcStr, '', 'TlfTmd'   , 01, 011, 1, NFSe.Tomador.Contato.Telefone, '');
  Gerador.wCampo(tcStr, '', 'InsMunTmd', 01, 020, 1, NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, '');
  Gerador.wCampo(tcStr, '', 'NomTmd'   , 05, 100, 1, NFSe.Tomador.RazaoSocial, '');
  Gerador.wCampo(tcStr, '', 'DesEndTmd', 05, 100, 1, NFSe.Tomador.Endereco.Endereco, '');
  Gerador.wCampo(tcStr, '', 'NomBaiTmd', 01, 060, 1, NFSe.Tomador.Endereco.Bairro, '');
  Gerador.wCampo(tcStr, '', 'NomCidTmd', 01, 060, 1, NFSe.Tomador.Endereco.xMunicipio, '');
  Gerador.wCampo(tcStr, '', 'CodEstTmd', 02, 002, 1, NFSe.Tomador.Endereco.UF, '');
  Gerador.wCampo(tcStr, '', 'CEPTmd'   , 08, 008, 1, NFSe.Tomador.Endereco.CEP, '');
  Gerador.wCampo(tcStr, '', 'EmlTmd'   , 01, 050, 1, NFSe.Tomador.Contato.Email,'');
end;

procedure TNFSeW_Governa.GerarListaServicos;
var
  i: integer;
begin
  Gerador.Prefixo := Prefixo4;
  Gerador.wGrupo('ItensRps');

  for i := 0 to Nfse.Servico.ItemServico.Count -1 do
  begin
    Gerador.wGrupo('ItemRps');
    Gerador.Prefixo := Prefixo3;
    Gerador.wCampo(tcInt, '', 'SeqItem', 01, 02, 1, i+1, '');
    Gerador.wCampo(tcDe2, '', 'QdeSvc', 01, 09, 1, Nfse.Servico.ItemServico.Items[i].Quantidade, '');
    Gerador.wCampo(tcDe2, '', 'VlrUnt', 01, 16, 1, Nfse.Servico.ItemServico.Items[i].ValorUnitario, '');
    Gerador.wCampo(tcStr, '', 'DesSvc', 00, 100, 1, Nfse.Servico.ItemServico.Items[i].Descricao, '');
    Gerador.Prefixo := Prefixo4;
    Gerador.wGrupo('/ItemRps');
  end;

  Gerador.Prefixo := Prefixo4;
  Gerador.wGrupo('/ItensRps');
end;

procedure TNFSeW_Governa.GerarValoresServico;
begin
  Gerador.wCampo(tcStr, '', 'CodAti', 4, 10, 1, NFSe.Servico.CodigoTributacaoMunicipio, '');
  Gerador.wCampo(tcDe2, '', 'PerAlq', 01, 15, 1, NFSe.Servico.Valores.Aliquota, '');

  if TRegRecToStr(NFSe.RegRec) <> '' then
  begin
    Gerador.wCampo(tcStr, '', 'RegRec', 01, 01, 2, TRegRecToStr(NFSe.RegRec) , '');
    Gerador.wCampo(tcStr, '', 'FrmRec', 01, 01, 1, TFrmRecToStr(NFSe.FrmRec) , '');
    Gerador.wCampo(tcStr, '', 'AnoCpt', 01, 01, 1, FormatDateTime('YYYY', StrToDateTimeDef(NFSe.Competencia,NFSe.DataEmissao)) , '');
    Gerador.wCampo(tcStr, '', 'MesCpt', 01, 01, 1, FormatDateTime('MM', StrToDateTimeDef(NFSe.Competencia,NFSe.DataEmissao)) , '');
  end
  else begin
    Gerador.wCampo(tcStr, '', 'FrmTrb', 01, 02, 1, '11', '');
    Gerador.wCampo(tcStr, '', 'TipRec', 01, 01, 1, NFSe.TipoRecolhimento , '');
  end;

  Gerador.wCampo(tcStr, '', 'DatEmsRps', 08, 08, 1, StringReplace(FormatDateTime('yyyymmdd',NFSe.DataEmissao),'/', '',[rfReplaceAll]), '');
  Gerador.wCampo(tcStr, '', 'DatEmsNFSe', 08, 08, 1, '' , '');
  Gerador.wCampo(tcDe2, '', 'VlrDed', 01, 16, 1, NFSe.Servico.Valores.ValorDeducoes, '');
  //**analisar valores maximos, dependendo do tipo de serviço** campo - VLrDed
  Gerador.wCampo(tcDe2, '', 'VlrDsc', 01, 01, 1, NFSe.Servico.Valores.DescontoCondicionado, '');
  //**valor fixo 0 para desconto
  gerador.wCampo(tcDe2, '', 'VlrPIS', 01, 16, 1, NFSe.Servico.Valores.ValorPis, '');
  Gerador.wCampo(tcDe2, '', 'VlrINSS', 01, 16, 1, NFSe.Servico.Valores.ValorInss, '');
  Gerador.wCampo(tcDe2, '', 'VlrCofins', 01, 16, 1, NFSe.Servico.Valores.ValorCofins, '');
  Gerador.wCampo(tcDe2, '', 'VlrIR', 01, 16, 1, NFSe.Servico.Valores.ValorIr, '');
  Gerador.wCampo(tcDe2, '', 'VlrCSLL', 01, 16, 1, NFSe.Servico.Valores.ValorCsll, '');
  Gerador.wCampo(tcDe2, '', 'VlrOtrRtn', 01, 16, 1, NFSe.Servico.Valores.valorOutrasRetencoes , '');
  Gerador.wCampo(tcStr, '', 'DesOtrRtn', 01, 16, 1, NFSe.Servico.Valores.DescricaoOutrasRetencoes, '');

  if TRegRecToStr(NFSe.RegRec) <> '' then
  begin
    Gerador.wCampo(tcStr, '', 'EstServ', 01, 01, 2, iif(NFSe.Servico.UFPrestacao = '', NFSe.Tomador.Endereco.UF,NFSe.Servico.UFPrestacao) , '');
    Gerador.wCampo(tcStr, '', 'MunSvc', 01, 01, 2, iif(NFSe.Servico.CodigoMunicipio = '', NFSe.Tomador.Endereco.CodigoMunicipio, NFSe.Servico.CodigoMunicipio), '');
    Gerador.wCampo(tcDe2, '', 'VlrRep', 01, 16, 1, NFSe.Servico.Valores.ValorRepasse, '');
  end;

  Gerador.wCampo(tcStr, '', 'Obs', 01, 16, 1, NFSe.OutrasInformacoes, '');
end;

procedure TNFSeW_Governa.GerarXML_Governa;
begin
  Gerador.Prefixo := Prefixo4;
  Gerador.wGrupo('Rps');
  Gerador.wGrupo('InfRps');

  Gerador.Prefixo := Prefixo3;
  Gerador.wCampo(tcStr, '', 'NumRps', 01, 10, 1, NFSe.IdentificacaoRps.Numero, DSC_NUMRPS);
  Gerador.wCampo(tcStr, '', 'CodVer', 01, 10, 1, NFSe.CodigoVerificacao, '');

  if NFSe.PrestadorServico.Endereco.CodigoMunicipio = '3104007' then //Araxá
    Gerador.wCampo(tcStr, '', 'VrsImp', 01, 01, 1, '5', '')
  else
    Gerador.wCampo(tcStr, '', 'VrsImp', 01, 01, 1, '3', '');

  GerarTomador;
  GerarValoresServico;
  GerarListaServicos;

  Gerador.wGrupo('/InfRps');
  Gerador.wGrupo('/Rps');
end;

constructor TNFSeW_Governa.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);
end;

function TNFSeW_Governa.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

function TNFSeW_Governa.GerarXml: Boolean;
begin
  Gerador.ListaDeAlertas.Clear;

  Gerador.Opcoes.DecimalChar := ',';
  Gerador.Opcoes.QuebraLinha := FQuebradeLinha;

  Gerador.Prefixo := Prefixo4;

  Gerador.wGrupo('LoteRps');

  Gerador.wCampo(tcStr, '', 'CodCadBic', 01, 15, 1, NFSe.Prestador.InscricaoMunicipal, '');

  if NFSe.PrestadorServico.Endereco.CodigoMunicipio = '3104007' then //Araxá
    Gerador.wCampo(tcStr, '', 'VrsArq', 01, 01, 1, '4', '')
  else
    Gerador.wCampo(tcStr, '', 'VrsArq', 01, 01, 1, '1', '');

  Gerador.wCampo(tcStr, '', 'ChvAcs', 30, 30, 1, NFSe.Prestador.ChaveAcesso, '');

  GerarXML_Governa;

  Gerador.wGrupo('/LoteRps');

  Gerador.gtAjustarRegistros(NFSe.InfID.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.
