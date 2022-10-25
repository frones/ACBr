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

unit pnfsNFSeW_Equiplano;

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
  pnfsConsts, 
  pcnConsts;

type
  { TNFSeW_Equiplano }

  TNFSeW_Equiplano = class(TNFSeWClass)
  private
  protected

    procedure GerarIdentificacaoRPS;
    procedure GerarTomador;

    procedure GerarListaServicos;
    procedure GerarValoresServico;

    procedure GerarXML_Equiplano;

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
{ layout do Equiplano.                                                         }
{ Sendo assim só será criado uma nova unit para um novo layout.                }
{==============================================================================}

{ TNFSeW_Equiplano }

procedure TNFSeW_Equiplano.GerarIdentificacaoRPS;
begin
  Gerador.wCampo(tcInt,   '', 'nrRps       ', 01, 15, 1, OnlyNumber(NFSe.IdentificacaoRps.Numero), DSC_NUMRPS);
  Gerador.wCampo(tcStr,   '', 'nrEmissorRps', 01, 01, 1, NFSe.IdentificacaoRps.Serie, DSC_SERIERPS);
  Gerador.wCampo(tcDatHor,'', 'dtEmissaoRps', 19, 19, 1, NFSe.DataEmissao, DSC_DEMI);
  Gerador.wCampo(tcStr,   '', 'stRps       ', 01, 01, 1, '1', '');
  Gerador.wCampo(tcStr,   '', 'tpTributacao', 01, 01, 1, NaturezaOperacaoToStr(NFSe.NaturezaOperacao), '');
  Gerador.wCampo(tcStr,   '', 'isIssRetido ', 01, 01, 1, SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido), '');
end;

procedure TNFSeW_Equiplano.GerarTomador;
var
  sTpDoc: String;
begin
  if (NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '') and
     (NFSe.Tomador.RazaoSocial <> '') then
  begin
    if (Trim(NFSe.Tomador.IdentificacaoTomador.DocTomadorEstrangeiro) <> '') then
      sTpDoc := '3'  //Est
    else
      if (Length(OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj)) = 14) then
        sTpDoc := '2'  //CNPJ
      else
        sTpDoc := '1'; //CPF

    Gerador.wGrupo('tomador');

    Gerador.wGrupo('documento');
    Gerador.wCampo(tcStr, '', 'nrDocumento           ', 01, 14,  1, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), '');
    Gerador.wCampo(tcStr, '', 'tpDocumento           ', 01, 01,  1, sTpDoc, '');
    Gerador.wCampo(tcStr, '', 'dsDocumentoEstrangeiro', 00, 20,  1, NFSe.Tomador.IdentificacaoTomador.DocTomadorEstrangeiro, '');
    Gerador.wGrupo('/documento');

    Gerador.wCampo(tcStr, '', 'nmTomador          ', 01, 080, 1, NFSe.Tomador.RazaoSocial, '');
    Gerador.wCampo(tcStr, '', 'dsEmail            ', 00, 080, 0, NFSe.Tomador.Contato.Email, '');
    Gerador.wCampo(tcStr, '', 'nrInscricaoEstadual', 00, 020, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, '');
    Gerador.wCampo(tcStr, '', 'dsEndereco         ', 00, 040, 1, NFSe.Tomador.Endereco.Endereco, '');
    Gerador.wCampo(tcStr, '', 'nrEndereco         ', 00, 010, 1, NFSe.Tomador.Endereco.Numero, '');
    Gerador.wCampo(tcStr, '', 'dsComplemento      ', 00, 060, 1, NFSe.Tomador.Endereco.Complemento, '');
    Gerador.wCampo(tcStr, '', 'nmBairro           ', 00, 025, 1, NFSe.Tomador.Endereco.Bairro, '');
    Gerador.wCampo(tcStr, '', 'nrCidadeIbge       ', 00, 007, 1, NFSe.Tomador.Endereco.CodigoMunicipio, '');
    Gerador.wCampo(tcStr, '', 'nmUf               ', 00, 002, 1, NFSe.Tomador.Endereco.UF, '');
    Gerador.wCampo(tcStr, '', 'nmPais             ', 01, 040, 1, NFSe.Tomador.Endereco.xPais, '');
    Gerador.wCampo(tcStr, '', 'nrCep              ', 00, 015, 1, OnlyNumber(NFSe.Tomador.Endereco.CEP), '');
    Gerador.wCampo(tcStr, '', 'nrTelefone         ', 00, 020, 1, NFSe.Tomador.Contato.Telefone, '');
    Gerador.wGrupo('/tomador');
  end;
end;

procedure TNFSeW_Equiplano.GerarListaServicos;
var
  iAux, iSerItem, iSerSubItem, i: Integer;
  itemServico: TItemServicoCollectionItem;

  //----------------------------------------------------------------------------
  procedure tratarSerItem(AItemServico: string);
  begin
    iAux := StrToInt(OnlyNumber(AItemServico)); //Ex.: 1402, 901
    if (iAux > 999) then //Ex.: 1402
    begin
      iSerItem    := StrToInt(Copy(IntToStr(iAux), 1, 2)); //14
      iSerSubItem := StrToInt(Copy(IntToStr(iAux), 3, 2)); //2
    end
    else begin //Ex.: 901
      iSerItem    := StrToInt(Copy(IntToStr(iAux), 1, 1)); //9
      iSerSubItem := StrToInt(Copy(IntToStr(iAux), 2, 2)); //1
    end;
  end;
  //----------------------------------------------------------------------------

begin
  {
  iAux := StrToInt(OnlyNumber(NFSe.Servico.ItemListaServico)); //Ex.: 1402, 901
  if (iAux > 999) then //Ex.: 1402
  begin
    iSerItem    := StrToInt(Copy(IntToStr(iAux), 1, 2)); //14
    iSerSubItem := StrToInt(Copy(IntToStr(iAux), 3, 2)); //2
  end
  else begin //Ex.: 901
    iSerItem    := StrToInt(Copy(IntToStr(iAux), 1, 1)); //9
    iSerSubItem := StrToInt(Copy(IntToStr(iAux), 2, 2)); //1
  end;
  }
  tratarSerItem(NFSe.Servico.ItemListaServico);
  Gerador.wGrupo('listaServicos');


  if NFSe.Servico.ItemServico.Count > 1 then
  begin
    for i:=0 to NFSe.Servico.ItemServico.Count-1 do
    begin
      itemServico := NFSe.Servico.ItemServico[i];
      if itemServico.CodServ <> '' then
        tratarSerItem(itemServico.CodServ)
      else
        tratarSerItem(NFSe.Servico.ItemListaServico);

      Gerador.wGrupo('servico');
      Gerador.wCampo(tcStr, '', 'nrServicoItem   ', 01, 02, 1, iSerItem, '');
      Gerador.wCampo(tcStr, '', 'nrServicoSubItem', 01, 02, 1, iSerSubItem, '');
      Gerador.wCampo(tcDe2, '', 'vlServico       ', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].ValorUnitario, '');
      Gerador.wCampo(tcDe2, '', 'vlAliquota      ', 01, 02, 1, NFSe.Servico.ItemServico.Items[i].Aliquota, '');

      if (NFSe.Servico.Valores.ValorDeducoes > 0) then
      begin
        Gerador.wGrupo('deducao');
        Gerador.wCampo(tcDe2, '', 'vlDeducao             ', 01, 15, 1, NFSe.Servico.Valores.ValorDeducoes, '');
        Gerador.wCampo(tcStr, '', 'dsJustificativaDeducao', 01,255, 1, NFSe.Servico.Valores.JustificativaDeducao, '');
        Gerador.wGrupo('/deducao');
      end;

      Gerador.wCampo(tcDe2, '', 'vlBaseCalculo         ', 01,  15, 1, NFSe.Servico.ItemServico.Items[i].BaseCalculo, '');
      Gerador.wCampo(tcDe2, '', 'vlIssServico          ', 01,  15, 1, NFSe.Servico.ItemServico.Items[i].ValorIss, '');
      Gerador.wCampo(tcStr, '', 'dsDiscriminacaoServico', 01,1024, 1, NFSe.Servico.ItemServico.Items[i].Discriminacao, '');
      Gerador.wGrupo('/servico');
    end;
  end
  else
  begin
    Gerador.wGrupo('servico');
    Gerador.wCampo(tcStr, '', 'nrServicoItem   ', 01, 02, 1, iSerItem, '');
    Gerador.wCampo(tcStr, '', 'nrServicoSubItem', 01, 02, 1, iSerSubItem, '');
    Gerador.wCampo(tcDe2, '', 'vlServico       ', 01, 15, 1, NFSe.Servico.Valores.ValorServicos, '');
    Gerador.wCampo(tcDe2, '', 'vlAliquota      ', 01, 02, 1, NFSe.Servico.Valores.Aliquota, '');

    if (NFSe.Servico.Valores.ValorDeducoes > 0) then
    begin
      Gerador.wGrupo('deducao');
      Gerador.wCampo(tcDe2, '', 'vlDeducao             ', 01, 15, 1, NFSe.Servico.Valores.ValorDeducoes, '');
      Gerador.wCampo(tcStr, '', 'dsJustificativaDeducao', 01,255, 1, NFSe.Servico.Valores.JustificativaDeducao, '');
      Gerador.wGrupo('/deducao');
    end;

    Gerador.wCampo(tcDe2, '', 'vlBaseCalculo         ', 01,  15, 1, NFSe.Servico.Valores.BaseCalculo, '');
    Gerador.wCampo(tcDe2, '', 'vlIssServico          ', 01,  15, 1, NFSe.Servico.Valores.ValorIss, '');
    Gerador.wCampo(tcStr, '', 'dsDiscriminacaoServico', 01,1024, 1, NFSe.Servico.Discriminacao, '');
    Gerador.wGrupo('/servico');
  end;

  Gerador.wGrupo('/listaServicos');
end;

procedure TNFSeW_Equiplano.GerarValoresServico;
begin
  Gerador.wCampo(tcDe2, '', 'vlTotalRps  ', 01, 15, 1, NFSe.Servico.Valores.ValorServicos, '');
  Gerador.wCampo(tcDe2, '', 'vlLiquidoRps', 01, 15, 1, NFSe.Servico.Valores.ValorLiquidoNfse, '');

  Gerador.wGrupo('retencoes');
  Gerador.wCampo(tcDe2, '', 'vlCofins        ', 01, 15, 1, NFSe.Servico.Valores.ValorCofins, '');
  Gerador.wCampo(tcDe2, '', 'vlCsll          ', 01, 15, 1, NFSe.Servico.Valores.ValorCsll, '');
  Gerador.wCampo(tcDe2, '', 'vlInss          ', 01, 15, 1, NFSe.Servico.Valores.ValorInss, '');
  Gerador.wCampo(tcDe2, '', 'vlIrrf          ', 01, 15, 1, NFSe.Servico.Valores.ValorIr, '');
  Gerador.wCampo(tcDe2, '', 'vlPis           ', 01, 15, 1, NFSe.Servico.Valores.ValorPis, '');
  Gerador.wCampo(tcDe2, '', 'vlIss           ', 01, 15, 1, NFSe.Servico.Valores.ValorIssRetido, '');
  Gerador.wCampo(tcDe2, '', 'vlAliquotaCofins', 01, 02, 1, NFSe.Servico.Valores.AliquotaCofins, '');
  Gerador.wCampo(tcDe2, '', 'vlAliquotaCsll  ', 01, 02, 1, NFSe.Servico.Valores.AliquotaCsll, '');
  Gerador.wCampo(tcDe2, '', 'vlAliquotaInss  ', 01, 02, 1, NFSe.Servico.Valores.AliquotaInss, '');
  Gerador.wCampo(tcDe2, '', 'vlAliquotaIrrf  ', 01, 02, 1, NFSe.Servico.Valores.AliquotaIr, '');
  Gerador.wCampo(tcDe2, '', 'vlAliquotaPis   ', 01, 02, 1, NFSe.Servico.Valores.AliquotaPis, '');
  Gerador.wGrupo('/retencoes');
end;

procedure TNFSeW_Equiplano.GerarXML_Equiplano;
begin
  Gerador.wGrupo('rps');

  GerarIdentificacaoRPS;
  GerarTomador;
  GerarListaServicos;
  GerarValoresServico;

  Gerador.wGrupo('/rps');
end;

constructor TNFSeW_Equiplano.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);
end;

function TNFSeW_Equiplano.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

function TNFSeW_Equiplano.GerarXml: Boolean;
begin
  Gerador.ListaDeAlertas.Clear;

  Gerador.ArquivoFormatoXML := '';
  Gerador.Prefixo           := FPrefixo4;

  Gerador.Opcoes.QuebraLinha := FQuebradeLinha;

  if (RightStr(FURL, 1) <> '/') and (FDefTipos <> '') then
    FDefTipos := '/' + FDefTipos;

  if Trim(FPrefixo4) <> '' then
    Atributo := ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FURL + FDefTipos + '"'
  else
    Atributo := ' xmlns="' + FURL + FDefTipos + '"';

  FNFSe.InfID.ID := OnlyNumber(FNFSe.IdentificacaoRps.Numero) +
                      FNFSe.IdentificacaoRps.Serie;

  GerarXML_Equiplano;

  Gerador.gtAjustarRegistros(NFSe.InfID.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.
