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

unit pnfsNFSeW_Infisc;

interface

uses
{$IFDEF FPC}
  LResources, 
  Controls, 
{$ELSE}

{$ENDIF}
  SysUtils, 
  Classes,
  ACBrConsts,
  pnfsNFSeW, 
  pcnAuxiliar, 
  pcnConversao, 
  pcnGerador,
  pnfsNFSe, 
  pnfsConversao, 
  pcnConsts;

type
  { TNFSeW_Infisc }

  TNFSeW_Infisc = class(TNFSeWClass)
  private
    FdTotBCISS: Double;
    FdTotISS: Double;
  protected

    // **************************** Versão 1.00
    procedure GerarIdentificacaoRPS_v10;
    procedure GerarPrestador_v10;
    procedure GerarTomador_v10;
    procedure GerarListaServicos_v10;
    procedure GerarValoresServico_v10;
    procedure GerarCondicaoPagamento_v10;

    procedure GerarXML_Infisc_v10;

    // **************************** Versão 1.10
    procedure GerarIdentificacaoRPS_v11;
    procedure GerarPrestador_v11;
    procedure GerarTomador_v11;
    procedure GerarListaServicos_v11;
    procedure GerarValoresServico_v11;
    procedure GerarCondicaoPagamento_v11;
    procedure GerarDespesasAdicionaisNaoTributaveis;

    procedure GerarXML_Infisc_v11;

    // **************************** Ambas Versões
    procedure GerarTransportadora;
    procedure GerarConstrucaoCivil;

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
{ layout do Infisc.                                                            }
{ Sendo assim só será criado uma nova unit para um novo layout.                }
{==============================================================================}

{ TNFSeW_Infisc }

constructor TNFSeW_Infisc.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);
end;

function TNFSeW_Infisc.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

function TNFSeW_Infisc.GerarXml: Boolean;
begin
  Gerador.ListaDeAlertas.Clear;

  Gerador.ArquivoFormatoXML := '';
  Gerador.Prefixo           := FPrefixo4;

  Gerador.Opcoes.QuebraLinha := FQuebradeLinha;

  FNFSe.InfID.ID  := FNFSe.Numero;

  if VersaoNFSe = ve100 then
    GerarXML_Infisc_v10
  else
    GerarXML_Infisc_v11;


  Gerador.gtAjustarRegistros(NFSe.InfID.ID);

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

////////////////////////////////////////////////////////////////////////////////
// **************************** Versão 1.00
////////////////////////////////////////////////////////////////////////////////

procedure TNFSeW_Infisc.GerarIdentificacaoRPS_v10;
var
  sChave, cNFSe, serie, nNFSe:String;
begin
  sChave := NFSe.ChaveNFSe;
  serie  := NFSE.SeriePrestacao;
  nNFSe  := NFSe.Numero;
  cNFSe  := copy(sChave, 31, 9);

  Gerador.wGrupo('Id');
  Gerador.wCampo(tcStr, '', 'cNFS-e', 01, 09, 1, cNFSe, '');
  Gerador.wCampo(tcStr, '', 'natOp' , 01, 50, 1, NaturezaOperacaoToStr(NFSe.NaturezaOperacao), '');
  Gerador.wCampo(tcStr, '', 'mod'   , 01, 02, 1, NFSe.ModeloNFSe, '');  // segundo manual obrigatório ser 98
  Gerador.wCampo(tcStr, '', 'serie' , 01, 03, 1, serie, '');            // tem que ser S - ver como está sendo passado a variável "serie"
  Gerador.wCampo(tcStr, '', 'nNFS-e', 01, 09, 1, nNFSe, '');
  Gerador.wCampo(tcStr, '', 'dEmi'  , 01, 10, 1, FormatDateTime('yyyy-mm-dd',NFSe.DataEmissao), '');
  Gerador.wCampo(tcStr, '', 'hEmi'  , 01, 10, 1, FormatDateTime('hh:mm',NFSe.DataEmissao), '');
  Gerador.wCampo(tcStr, '', 'tpNF'  , 01, 01, 1, '1', '');     // 0- Entrada 1- Saída
  Gerador.wCampo(tcStr, '', 'cMunFG', 01, 07, 1, NFSe.Servico.CodigoMunicipio, '');
  Gerador.wCampo(tcStr, '', 'refNF' , 01, 39, 1, sChave, '');  // chave de acesso 39 caracteres
  Gerador.wCampo(tcStr, '', 'tpEmis', 01, 01, 1, TipoEmissaoToStr(NFSe.TipoEmissao), ''); // N- Normal C- Contigencia
  Gerador.wCampo(tcStr, '', 'anulada', 01, 01, 1, 'N', '');
  Gerador.wGrupo('/Id');
end;

procedure TNFSeW_Infisc.GerarPrestador_v10;
begin
  Gerador.wGrupo('emit');
  Gerador.wCampo(tcStr, '', 'CNPJ'  , 01, 014, 1, NFSe.Prestador.Cnpj, '');
  Gerador.wCampo(tcStr, '', 'xNome' , 01, 100, 1, NFSe.PrestadorServico.RazaoSocial, '');
  Gerador.wCampo(tcStr, '', 'xFant' , 01, 060, 1, NFSe.PrestadorServico.NomeFantasia, '');
  Gerador.wCampo(tcStr, '', 'IM'    , 01, 015, 1, NFSe.Prestador.InscricaoMunicipal, '');

  Gerador.wGrupo('end');
  Gerador.wCampo(tcStr, '', 'xLgr'   , 01, 100, 1, NFSe.PrestadorServico.Endereco.Endereco, '');
  Gerador.wCampo(tcStr, '', 'nro'    , 01, 015, 1, NFSe.PrestadorServico.Endereco.Numero, '');
  Gerador.wCampo(tcStr, '', 'xCpl'   , 01, 100, 1, NFSe.PrestadorServico.Endereco.Complemento, '');
  Gerador.wCampo(tcStr, '', 'xBairro', 01, 100, 1, NFSe.PrestadorServico.Endereco.Bairro, '');
  Gerador.wCampo(tcStr, '', 'cMun'   , 01, 007, 1, NFSe.PrestadorServico.Endereco.CodigoMunicipio, '');
  Gerador.wCampo(tcStr, '', 'xMun'   , 01, 060, 1, copy(CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio,0)),
                                                      0, pos('/',CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio,0)))-1), '');
  Gerador.wCampo(tcStr, '', 'UF'     , 01, 002, 1, NFSe.PrestadorServico.Endereco.UF, '');
  Gerador.wCampo(tcStr, '', 'CEP'    , 01, 008, 1, NFSe.PrestadorServico.Endereco.CEP, '');
  Gerador.wCampo(tcInt, '', 'cPais'  , 01, 100, 1, NFSe.PrestadorServico.Endereco.CodigoPais, '');
  Gerador.wCampo(tcStr, '', 'xPais'  , 01, 100, 1, NFSe.PrestadorServico.Endereco.xPais, '');
  Gerador.wCampo(tcStr, '', 'fone'   , 01, 100, 1, NFSe.PrestadorServico.Contato.Telefone, '');
  Gerador.wGrupo('/end');

  case Nfse.RegimeEspecialTributacao of
    retSimplesNacional:
      Gerador.wCampo(tcStr, '', 'regimeTrib', 01, 01, 1, '1', ''); // 1 - Simples
    retMicroempresarioEmpresaPP:
      Gerador.wCampo(tcStr, '', 'regimeTrib', 01, 01, 1, '2', ''); // 2 - SIMEI
    retLucroReal,
    retLucroPresumido:
      Gerador.wCampo(tcStr, '', 'regimeTrib', 01, 01, 1, '3', ''); // 3 - Normal
  else
    Gerador.wCampo(tcStr, '', 'regimeTrib', 01, 01, 1, '1', ''); // 1 - Simples
  end;

  Gerador.wGrupo('/emit');
end;

procedure TNFSeW_Infisc.GerarTomador_v10;
begin
  Gerador.wGrupo('TomS');

  if Length(NFSe.Tomador.IdentificacaoTomador.CpfCnpj) = 11 then
    Gerador.wCampo(tcStr, '', 'CPF', 01, 11, 1, NFSe.Tomador.IdentificacaoTomador.CpfCnpj, '')
  else
    Gerador.wCampo(tcStr, '', 'CNPJ', 01, 14, 1, NFSe.Tomador.IdentificacaoTomador.CpfCnpj, '');

  Gerador.wCampo(tcStr, '', 'xNome', 01, 100, 1, NFSe.Tomador.RazaoSocial, '');

  Gerador.wGrupo('ender');
  Gerador.wCampo(tcStr, '', 'xLgr'   , 01, 100, 1, NFSe.Tomador.Endereco.Endereco, '');
  Gerador.wCampo(tcStr, '', 'nro'    , 01, 015, 1, NFSe.Tomador.Endereco.Numero, '');
  Gerador.wCampo(tcStr, '', 'xCpl'   , 01, 100, 1, NFSe.Tomador.Endereco.Complemento, '');
  Gerador.wCampo(tcStr, '', 'xBairro', 01, 100, 1, NFSe.Tomador.Endereco.Bairro, '');
  Gerador.wCampo(tcStr, '', 'cMun'   , 01, 007, 1, NFSe.Tomador.Endereco.CodigoMunicipio, '');
  Gerador.wCampo(tcStr, '', 'xMun'   , 01, 060, 1, CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio,0)), '');
  Gerador.wCampo(tcStr, '', 'UF'     , 01, 002, 1, NFSe.Tomador.Endereco.UF, '');
  Gerador.wCampo(tcStr, '', 'CEP'    , 01, 008, 1, NFSe.Tomador.Endereco.CEP, '');
  Gerador.wCampo(tcStr, '', 'cPais'  , 01, 004, 1, '1058', '');
  Gerador.wCampo(tcStr, '', 'xPais'  , 01, 100, 1, 'Brasil', '');
  Gerador.wCampo(tcStr, '', 'fone'   , 01, 100, 1, NFSe.Tomador.Contato.Telefone, '');
  Gerador.wGrupo('/ender');

  Gerador.wCampo(tcStr, '', 'xEmail', 01, 100, 1, NFSe.Tomador.Contato.Email, '');
  Gerador.wCampo(tcStr, '', 'IM'    , 01, 015, 1, NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, '');

  if NFSe.Servico.MunicipioIncidencia <> 0 then
  begin
    if (NFSe.Servico.MunicipioIncidencia = 4303905) then
      Gerador.wCampo(tcStr, '', 'Praca', 01, 60, 1, 'Campo Bom-RS', '')
    else
      Gerador.wCampo(tcStr, '', 'Praca', 01, 60, 1, CodCidadeToCidade(NFSe.Servico.MunicipioIncidencia), '');
  end;

  Gerador.wGrupo('/TomS');
end;

procedure TNFSeW_Infisc.GerarListaServicos_v10;
var
  i, GeraTag: Integer;
  cServ, xServ: String;
begin
  FdTotBCISS := 0;
  FdTotISS   := 0;

  for i := 0 to NFSe.Servico.ItemServico.Count-1 do
  begin
    cServ := NFSe.Servico.ItemServico.Items[i].codServ; // cod. municipal
    xServ := NFSe.Servico.ItemServico.Items[i].Descricao;

    if NFSe.Servico.ItemListaServico <> '' then
      xServ := xServ + ' (Class.: ' + NFSe.Servico.ItemListaServico+')';

    Gerador.wGrupo('det');
    Gerador.wCampo(tcStr, '', 'nItem', 01, 02, 1, IntToStr(i+1), '');

    Gerador.wGrupo('serv');
    Gerador.wCampo(tcStr, '', 'cServ' , 01, 002, 1, cServ, '');
    Gerador.wCampo(tcStr, '', 'xServ' , 01, 120, 1, xServ, '');
    Gerador.wCampo(tcStr, '', 'uTrib' , 01, 006, 1, 'UN', '');
    Gerador.wCampo(tcDe2, '', 'qTrib' , 01, 015, 1, NFSe.Servico.ItemServico.Items[i].Quantidade, '');
    Gerador.wCampo(tcDe3, '', 'vUnit' , 01, 015, 1, NFSe.Servico.ItemServico.Items[i].ValorUnitario, '');
    Gerador.wCampo(tcDe2, '', 'vServ' , 01, 015, 0, NFSe.Servico.ItemServico.Items[i].ValorServicos, '');
    Gerador.wCampo(tcDe2, '', 'vDesc' , 01, 015, 0, NFSe.Servico.ItemServico.Items[i].DescontoIncondicionado, '');

    if ( Nfse.RegimeEspecialTributacao = retSimplesNacional ) or ( Nfse.OptanteSimplesNacional = snSim ) then
      GeraTag := 1
    else
      GeraTag := 0;

    Gerador.wCampo(tcDe2, '', 'vBCISS', 01, 015, GeraTag, NFSe.Servico.ItemServico.Items[i].BaseCalculo, '');
    Gerador.wCampo(tcDe2, '', 'pISS'  , 01, 015, GeraTag, NFSe.Servico.ItemServico.Items[i].Aliquota, '');
    Gerador.wCampo(tcDe2, '', 'vISS'  , 01, 015, GeraTag, NFSe.Servico.ItemServico.Items[i].ValorIss, '');

    FdTotBCISS := FdTotBCISS + NFSe.Servico.ItemServico.Items[i].BaseCalculo;
    FdTotISS   := FdTotISS   + NFSe.Servico.ItemServico.Items[i].ValorIss;

    Gerador.wCampo(tcDe2, '', 'pRed', 01, 15, 0, 0, '');
    Gerador.wCampo(tcDe2, '', 'vRed', 01, 15, 0, 0, '');

    // Retenção INSS
    if NFSe.Servico.ItemServico.Items[i].ValorInss > 0 then
      Gerador.wCampo(tcDe2, '', 'vRetINSS', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].ValorInss, '');

    // Retenção IRRF
    if NFSe.Servico.ItemServico.Items[i].ValorIr > 0 then
    begin
      Gerador.wCampo(tcStr, '', 'xRetIRF', 01, 02, 1, 'Retenção IRRF', '');
      Gerador.wCampo(tcDe2, '', 'vRetIRF', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].ValorIr, '');
    end;

    // Retenção PIS
    if NFSe.Servico.ItemServico.Items[i].ValorPis > 0 then
    begin
      Gerador.wCampo(tcStr, '', 'xRetLei10833-PIS-PASEP', 01, 02, 1, 'Retenção PIS', '');
      Gerador.wCampo(tcDe2, '', 'vRetLei10833-PIS-PASEP', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].ValorPis, '');
    end;

    // Retenção COFINS
    if NFSe.Servico.ItemServico.Items[i].ValorCofins > 0 then
    begin
      Gerador.wCampo(tcStr, '', 'xRetLei10833-COFINS', 01, 02, 1, 'Retenção COFINS', '');
      Gerador.wCampo(tcDe2, '', 'vRetLei10833-COFINS', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].ValorCofins, '');
    end;

    // Retenção CSLL
    if NFSe.Servico.ItemServico.Items[i].ValorCsll > 0 then
    begin
      Gerador.wCampo(tcStr, '', 'xRetLei10833-CSLL', 01, 02, 1, 'Retenção CSLL', '');
      Gerador.wCampo(tcDe2, '', 'vRetLei10833-CSLL', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].ValorCsll, '');
    end;

    Gerador.wGrupo('/serv');

    // Retenção ISSQN
    if NFSe.Servico.ItemServico.Items[i].ValorISSST > 0 then
    begin
      Gerador.wGrupo('ISSST');
      Gerador.wCampo(tcDe2, '', 'vISSST', 01, 15, 0, NFSe.Servico.ItemServico.Items[i].ValorISSST, '');
      Gerador.wGrupo('/ISSST');
    end;

    Gerador.wGrupo('/det');
  end;
end;

procedure TNFSeW_Infisc.GerarValoresServico_v10;
begin
  Gerador.wGrupo('total');
  Gerador.wCampo(tcDe2, '', 'vServ' , 01, 15, 0, NFSe.Servico.Valores.ValorServicos, '');
  Gerador.wCampo(tcDe2, '', 'vDesc' , 01, 15, 0, NFSe.Servico.Valores.DescontoIncondicionado, '');
  Gerador.wCampo(tcDe2, '', 'vOutro', 01, 15, 0, 0, '');
  Gerador.wCampo(tcDe2, '', 'vtNF'  , 01, 15, 1, NFSe.Servico.Valores.ValorLiquidoNfse, '');
  Gerador.wCampo(tcDe2, '', 'vtLiq' , 01, 15, 1, NFSe.Servico.Valores.ValorLiquidoNfse, '');

  Gerador.wCampo(tcDe2, '', 'totalAproxTribServ', 01, 15, 0, 0, '');

  // Total Retenção IRRF
  if (NFSe.Servico.Valores.ValorIr + NFSe.Servico.Valores.ValorPis +
      NFSe.Servico.Valores.ValorCofins + NFSe.Servico.Valores.ValorCsll +
      NFSe.Servico.Valores.ValorInss) > 0 then
  begin
    Gerador.wGrupo('Ret');

    if NFSe.Servico.Valores.ValorIr>0 then
    begin
      Gerador.wCampo(tcStr, '', 'xRetIRF', 01, 02, 1, 'Retenção IRRF', '');
      Gerador.wCampo(tcDe2, '', 'vRetIRF', 01, 15, 1, NFSe.Servico.Valores.ValorIr, '');
    end;

    if NFSe.Servico.Valores.ValorPis>0 then
    begin
      Gerador.wCampo(tcStr, '', 'xRetLei10833-PIS-PASEP', 01, 02, 1, 'Retenção PIS', '');
      Gerador.wCampo(tcDe2, '', 'vRetLei10833-PIS-PASEP', 01, 15, 1, NFSe.Servico.Valores.ValorPis, '');
    end;

    if NFSe.Servico.Valores.ValorCofins>0 then
    begin
      Gerador.wCampo(tcStr, '', 'xRetLei10833-COFINS', 01, 02, 1, 'Retenção COFINS', '');
      Gerador.wCampo(tcDe2, '', 'vRetLei10833-COFINS', 01, 15, 1, NFSe.Servico.Valores.ValorCofins, '');
    end;

    if NFSe.Servico.Valores.ValorCsll>0 then
    begin
      Gerador.wCampo(tcStr, '', 'xRetLei10833-CSLL', 01, 02, 1, 'Retenção CSLL', '');
      Gerador.wCampo(tcDe2, '', 'vRetLei10833-CSLL', 01, 15, 1, NFSe.Servico.Valores.ValorCsll, '');
    end;

    if NFSe.Servico.Valores.ValorInss>0 then
    begin
      Gerador.wCampo(tcStr, '', 'xRetINSS', 01, 02, 1, 'Retenção INSS', '');
      Gerador.wCampo(tcDe2, '', 'vRetINSS', 01, 15, 1, NFSe.Servico.Valores.ValorInss, '');
    end;

    Gerador.wGrupo('/Ret');
  end;

  // incluir tag 'fat' aqui!

  // Total Retenção ISSQN
  Gerador.wGrupo('ISS');
  Gerador.wCampo(tcDe2, '', 'vBCISS'  , 01, 15, 0, FdTotBCISS, '');
  Gerador.wCampo(tcDe2, '', 'vISS'    , 01, 15, 0, FdTotISS, '');
  Gerador.wCampo(tcDe2, '', 'vBCSTISS', 01, 15, 0, 0, '');
  Gerador.wCampo(tcDe2, '', 'vSTISS'  , 01, 15, 0, NFSe.Servico.Valores.ValorIssRetido, '');
  Gerador.wGrupo('/ISS');

  Gerador.wGrupo('/total');
end;

procedure TNFSeW_Infisc.GerarCondicaoPagamento_v10;
var
  i: Integer;
begin
  if NFSe.CondicaoPagamento.Parcelas.Count > 0 then
  begin
    Gerador.wGrupo('cobr');
    for i := 0 to NFSe.CondicaoPagamento.Parcelas.Count -1 do
    begin
      Gerador.wGrupo('dup');
      Gerador.wCampo(tcStr, '', 'nDup',  01,  9,  1, NFSe.CondicaoPagamento.Parcelas[i].Parcela, '');
      Gerador.wCampo(tcDat, '', 'dVenc', 01, 15,  1, NFSe.CondicaoPagamento.Parcelas[i].DataVencimento, DSC_DEMI);
      Gerador.wCampo(tcDe2, '', 'vDup',  01, 15,  1, NFSe.CondicaoPagamento.Parcelas[i].Valor, '');
      Gerador.wCampo(tcStr, '', 'bBol',  01,  1,  1, '2', '');
      Gerador.wGrupo('/dup');
    end;
    Gerador.wGrupo('/cobr');
  end;
end;

procedure TNFSeW_Infisc.GerarXML_Infisc_v10;
begin
  Gerador.Prefixo := FPrefixo4;

  Gerador.wGrupo('NFS-e');

  Gerador.wGrupo('infNFSe versao="1.00"');

  GerarIdentificacaoRPS_v10;
  GerarPrestador_v10;
  GerarTomador_v10;

  //Dados da Obra caso empreitada global seja construção (1)
  if EmpreitadaGlobalToStr(NFSe.EmpreitadaGlobal) = '1' then
    GerarConstrucaoCivil;

  GerarTransportadora;
  GerarListaServicos_v10;
  GerarValoresServico_v10;
  GerarCondicaoPagamento_v10;

  if Trim(NFSe.OutrasInformacoes) <> '' then
  begin
    Gerador.wGrupo('Observacoes');
    Gerador.wCampo(tcStr, '', 'xinf', 01, 100, 1, copy(NFSe.OutrasInformacoes, 1, 100), '');
    Gerador.wGrupo('/Observacoes');
  end;

  Gerador.wGrupo('/infNFSe');

  Gerador.wGrupo('/NFS-e');
end;

////////////////////////////////////////////////////////////////////////////////
// **************************** Versão 1.10
////////////////////////////////////////////////////////////////////////////////

procedure TNFSeW_Infisc.GerarIdentificacaoRPS_v11;
var
  sChave, cNFSe, serie, nNFSe:String;
begin
  sChave := NFSe.ChaveNFSe;
  serie  := NFSE.SeriePrestacao;
  nNFSe  := NFSe.Numero;
  cNFSe  := copy(sChave, 31, 9);

  Gerador.wGrupo('Id');
  Gerador.wCampo(tcStr, '', 'cNFS-e', 01, 09, 1, cNFSe, '');
  Gerador.wCampo(tcStr, '', 'mod'   , 01, 02, 1, NFSe.ModeloNFSe, '');  // segundo manual obrigatório ser 98
  Gerador.wCampo(tcStr, '', 'serie' , 01, 03, 1, serie, '');          // tem que ser S - ver como está sendo passado a variável "serie"
  Gerador.wCampo(tcStr, '', 'nNFS-e', 01, 09, 1, nNFSe, '');
  Gerador.wCampo(tcStr, '', 'dEmi'  , 01, 10, 1, FormatDateTime('yyyy-mm-dd',NFSe.DataEmissao), '');
  Gerador.wCampo(tcStr, '', 'hEmi'  , 01, 10, 1, FormatDateTime('hh:mm',NFSe.DataEmissao), '');
  Gerador.wCampo(tcStr, '', 'tpNF'  , 01, 01, 1, '1', '');     // 0- Entrada 1- Saída
  Gerador.wCampo(tcStr, '', 'refNF' , 01, 39, 1, sChave, ''); // chave de acesso 39 caracteres
  Gerador.wCampo(tcStr, '', 'tpEmis', 01, 01, 1, TipoEmissaoToStr(NFSe.TipoEmissao), ''); // N- Normal C- Contigencia

  Gerador.wCampo(tcStr, '', 'cancelada', 01, 01, 1, SimNaoInFiscToStr(NFSe.Cancelada), '');
  Gerador.wCampo(tcStr, '', 'canhoto'  , 01, 01, 1, TCanhotoToStr(NFSe.Canhoto), '');

  {para Garibaldi - RS 4308607 Não existe as tags abaixo}
  if (NFSe.PrestadorServico.Endereco.CodigoMunicipio <> '4308607') then
  begin
    { Ambiente 1- Producao 2- Homologacao }
    Gerador.wCampo(tcStr, '', 'ambienteEmi'     , 01, 01, 1, SimNaoToStr(NFSe.Producao), '');
    { forma de emissao 1- portal contribuinte 2- servidor web 3- submetidos via Upload no portal 4- emissao via RPS }
    Gerador.wCampo(tcStr, '', 'formaEmi'        , 01, 01, 1, '2', '');
    { 1- construcao civil 2- outros casos }
    Gerador.wCampo(tcStr, '', 'empreitadaGlobal', 01, 01, 1, EmpreitadaGlobalToStr(NFSe.EmpreitadaGlobal), '');
  end;

  Gerador.wGrupo('/Id');
end;

procedure TNFSeW_Infisc.GerarPrestador_v11;
begin
  Gerador.wGrupo('prest');
  Gerador.wCampo(tcStr, '', 'CNPJ' , 01, 014, 1, NFSe.Prestador.Cnpj, '');
  Gerador.wCampo(tcStr, '', 'xNome', 01, 100, 1, NFSe.PrestadorServico.RazaoSocial, '');
  Gerador.wCampo(tcStr, '', 'xFant', 01, 60, 1, NFSe.PrestadorServico.NomeFantasia, '');
  Gerador.wCampo(tcStr, '', 'IM'   , 01, 015, 1, NFSe.Prestador.InscricaoMunicipal, '');
  Gerador.wCampo(tcStr, '', 'xEmail' , 01, 50, 1, NFSe.PrestadorServico.Contato.Email, '');

  Gerador.wGrupo('end');
  Gerador.wCampo(tcStr, '', 'xLgr'   , 01, 100, 1, NFSe.PrestadorServico.Endereco.Endereco, '');
  Gerador.wCampo(tcStr, '', 'nro'    , 01, 015, 1, NFSe.PrestadorServico.Endereco.Numero, '');
  Gerador.wCampo(tcStr, '', 'xBairro', 01, 100, 1, NFSe.PrestadorServico.Endereco.Bairro, '');
  Gerador.wCampo(tcStr, '', 'cMun'   , 01, 007, 1, NFSe.PrestadorServico.Endereco.CodigoMunicipio, '');
  Gerador.wCampo(tcStr, '', 'xMun'   , 01, 060, 1, copy(CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio,0)),
                                                      0, pos('/',CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio,0)))-1), '');
  Gerador.wCampo(tcStr, '', 'UF'     , 01, 002, 1, NFSe.PrestadorServico.Endereco.UF, '');
  Gerador.wCampo(tcStr, '', 'CEP'    , 01, 008, 1, NFSe.PrestadorServico.Endereco.CEP, '');
  Gerador.wCampo(tcInt, '', 'cPais'  , 01, 100, 1, NFSe.PrestadorServico.Endereco.CodigoPais, '');
  Gerador.wCampo(tcStr, '', 'xPais'  , 01, 100, 1, NFSe.PrestadorServico.Endereco.xPais, '');
  Gerador.wGrupo('/end');

  Gerador.wCampo(tcStr, '', 'fone'  , 01, 100, 1, NFSe.PrestadorServico.Contato.Telefone, '');
  Gerador.wCampo(tcStr, '', 'IE'    , 01, 015, 1, NFSe.Prestador.InscricaoEstadual, '');

  case Nfse.RegimeEspecialTributacao of
    retSimplesNacional:
      Gerador.wCampo(tcStr, '', 'regimeTrib', 01, 01, 1, '1', ''); // 1 - Simples
    retMicroempresarioEmpresaPP:
      Gerador.wCampo(tcStr, '', 'regimeTrib', 01, 01, 1, '2', ''); // 2 - SIMEI
    retLucroReal,
    retLucroPresumido:
      Gerador.wCampo(tcStr, '', 'regimeTrib', 01, 01, 1, '3', ''); // 3 - Normal
  else
    Gerador.wCampo(tcStr, '', 'regimeTrib', 01, 01, 1, '1', ''); // 1 - Simples
  end;

  Gerador.wGrupo('/prest');
end;

procedure TNFSeW_Infisc.GerarTomador_v11;
begin
  Gerador.wGrupo('TomS');

  if Length(NFSe.Tomador.IdentificacaoTomador.CpfCnpj) = 11 then
    Gerador.wCampo(tcStr, '', 'CPF', 01, 11, 1, NFSe.Tomador.IdentificacaoTomador.CpfCnpj, '')
  else
    Gerador.wCampo(tcStr, '', 'CNPJ', 01, 14, 1, NFSe.Tomador.IdentificacaoTomador.CpfCnpj, '');

  Gerador.wCampo(tcStr, '', 'xNome', 01, 100, 1, NFSe.Tomador.RazaoSocial, '');

  Gerador.wGrupo('ender');
  Gerador.wCampo(tcStr, '', 'xLgr'   , 01, 100, 1, NFSe.Tomador.Endereco.Endereco, '');
  Gerador.wCampo(tcStr, '', 'nro'    , 01, 015, 1, NFSe.Tomador.Endereco.Numero, '');
  Gerador.wCampo(tcStr, '', 'xCpl'   , 01, 100, 1, NFSe.Tomador.Endereco.Complemento, '');
  Gerador.wCampo(tcStr, '', 'xBairro', 01, 100, 1, NFSe.Tomador.Endereco.Bairro, '');
  Gerador.wCampo(tcStr, '', 'cMun'   , 01, 007, 1, NFSe.Tomador.Endereco.CodigoMunicipio, '');
  Gerador.wCampo(tcStr, '', 'xMun'   , 01, 060, 1, copy(CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio,0)),
                                                           0, pos('/',CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio,0)))-1), '');
  Gerador.wCampo(tcStr, '', 'UF'     , 01, 002, 1, NFSe.Tomador.Endereco.UF, '');
  Gerador.wCampo(tcStr, '', 'CEP'    , 01, 008, 1, NFSe.Tomador.Endereco.CEP, '');
  Gerador.wCampo(tcInt, '', 'cPais'  , 01, 100, 1, NFSe.Tomador.Endereco.CodigoPais, '');
  Gerador.wCampo(tcStr, '', 'xPais'  , 01, 100, 1, NFSe.Tomador.Endereco.xPais, '');
  Gerador.wGrupo('/ender');

  Gerador.wCampo(tcStr, '', 'xEmail', 01, 100, 0, NFSe.Tomador.Contato.Email, '');
  Gerador.wCampo(tcStr, '', 'IE'    , 01, 015, 1, NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, '');
  Gerador.wCampo(tcStr, '', 'IM'    , 01, 015, 1, NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, '');
  Gerador.wCampo(tcStr, '', 'fone'  , 01, 100, 0, NFSe.Tomador.Contato.Telefone, '');

  Gerador.wGrupo('/TomS');
end;

procedure TNFSeW_Infisc.GerarListaServicos_v11;
var
  i, GeraTag: Integer;
  cServ, xServ: String;
begin
  FdTotBCISS := 0;
  FdTotISS   := 0;

  for i := 0 to NFSe.Servico.ItemServico.Count-1 do
  begin
    cServ := NFSe.Servico.ItemServico.Items[i].codServ; // cod. municipal
    xServ := NFSe.Servico.ItemServico.Items[i].Descricao;

    Gerador.wGrupo('det');
    Gerador.wCampo(tcStr, '', 'nItem', 01, 02, 1, IntToStr(i+1), '');

    Gerador.wGrupo('serv');
    Gerador.wCampo(tcStr, '', 'cServ'  , 01, 002, 1, cServ, '');  // Código municipal do serviço, apenas números
    Gerador.wCampo(tcStr, '', 'cLCServ', 01, 004, 1, NFSe.Servico.ItemServico.Items[i].codLCServ, ''); // Código do serviço conforme lei compl. 116
    Gerador.wCampo(tcStr, '', 'xServ'  , 01, 120, 1, xServ, ''); // Discriminação do serviço

    Gerador.wCampo(tcStr, '', 'localTributacao'  , 01, 04, 1, IntToStr(NFSe.Servico.MunicipioIncidencia), ''); // Local tributacao conforme codificacao do IBGE
    Gerador.wCampo(tcStr, '', 'localVerifResServ', 01, 04, 1, '1', ''); // Local da verificacao: 1 - Brasil 2 - Exterior

    Gerador.wCampo(tcStr, '', 'uTrib', 01, 06, 1, NFSe.Servico.ItemServico.Items[i].Unidade, '');       // unidade
    Gerador.wCampo(tcDe2, '', 'qTrib', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].Quantidade, '');    // quantidade
    Gerador.wCampo(tcDe2, '', 'vUnit', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].ValorUnitario, ''); // formatacao com 2 casas

    // (valor unitario * quantidade) - desconto
    Gerador.wCampo(tcDe2, '', 'vServ', 01, 15, 0, NFSe.Servico.ItemServico.Items[i].ValorServicos, '');
    Gerador.wCampo(tcDe2, '', 'vDesc', 01, 15, 0, NFSe.Servico.ItemServico.Items[i].DescontoIncondicionado, '');
    Gerador.wCampo(tcDe2, '', 'vDed' , 01, 15, 0, NFSe.Servico.ItemServico.Items[i].vDed, '');

    if NFSe.Servico.Valores.IssRetido = stNormal then
    begin  // 1 - stRetencao ; 2 - stNormal ; 3 - stSubstituicao

      // Alterado de = para <> por Italo em 28/05/2019
      if Nfse.RegimeEspecialTributacao <> retSimplesNacional then
        GeraTag := 1
      else
        GeraTag := 0;

      Gerador.wCampo(tcDe2, '', 'vBCISS', 01, 15, GeraTag, NFSe.Servico.ItemServico.Items[i].BaseCalculo, '');
      Gerador.wCampo(tcDe2, '', 'pISS'  , 01, 15, GeraTag, NFSe.Servico.ItemServico.Items[i].Aliquota, '');
      Gerador.wCampo(tcDe2, '', 'vISS'  , 01, 15, GeraTag, NFSe.Servico.ItemServico.Items[i].ValorIss, '');

      FdTotBCISS := FdTotBCISS + NFSe.Servico.ItemServico.Items[i].BaseCalculo;
      FdTotISS   := FdTotISS   + NFSe.Servico.ItemServico.Items[i].ValorIss;
    end;

    // Retenção INSS
    if NFSe.Servico.ItemServico.Items[i].ValorInss > 0 then
    begin
      Gerador.wCampo(tcDe2, '', 'vBCINSS ', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].vBCINSS, '');
      Gerador.wCampo(tcDe2, '', 'pRetINSS', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].pRetINSS, '');
      Gerador.wCampo(tcDe2, '', 'vRetINSS', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].ValorInss, '');
    end;

    Gerador.wCampo(tcDe2, '', 'vRed' , 01, 15, 0, NFSe.Servico.ItemServico.Items[i].vRed, '');

    // Retenção IRRF
    if NFSe.Servico.ItemServico.Items[i].ValorIr > 0 then
    begin
      Gerador.wCampo(tcDe2, '', 'vBCRetIR', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].vBCRetIR, '');
      Gerador.wCampo(tcDe2, '', 'pRetIR  ', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].pRetIR, '');
      Gerador.wCampo(tcDe2, '', 'vRetIR  ', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].ValorIr, '');
    end;

    // Retenção COFINS
    if NFSe.Servico.ItemServico.Items[i].ValorCofins > 0 then
    begin
      Gerador.wCampo(tcDe2, '', 'vBCCOFINS ', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].vBCCOFINS, '');
      Gerador.wCampo(tcDe2, '', 'pRetCOFINS', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].pRetCOFINS, '');
      Gerador.wCampo(tcDe2, '', 'vRetCOFINS', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].ValorCofins, '');
    end;

    // Retenção CSLL
    if NFSe.Servico.ItemServico.Items[i].ValorCsll > 0 then
    begin
      Gerador.wCampo(tcDe2, '', 'vBCCSLL ', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].vBCCSLL, '');
      Gerador.wCampo(tcDe2, '', 'pRetCSLL', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].pRetCSLL, '');
      Gerador.wCampo(tcDe2, '', 'vRetCSLL', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].ValorCsll, '');
    end;

    // Retenção PIS
    if NFSe.Servico.ItemServico.Items[i].ValorPis > 0 then
    begin
      Gerador.wCampo(tcDe2, '', 'vBCPISPASEP ', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].vBCPISPASEP, '');
      Gerador.wCampo(tcDe2, '', 'pRetPISPASEP', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].pRetPISPASEP, '');
      Gerador.wCampo(tcDe2, '', 'vRetPISPASEP', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].ValorPis, '');
    end;

    Gerador.wGrupo('/serv');

    // Retenção ISSQN
    if NFSe.Servico.Valores.IssRetido = stRetencao then
    begin  // 1 - stRetencao ; 2 - stNormal ; 3 - stSubstituicao
      Gerador.wGrupo('ISSST');
      Gerador.wCampo(tcDe2, '', 'vBCST ', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].ValorServicos, '');
      Gerador.wCampo(tcDe2, '', 'pISSST', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].AlicotaISSST, '');
      Gerador.wCampo(tcDe2, '', 'vISSST', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].ValorISSST, '');
      Gerador.wGrupo('/ISSST');
    end;

    Gerador.wGrupo('/det');
  end;
end;

procedure TNFSeW_Infisc.GerarValoresServico_v11;
begin
  Gerador.wGrupo('total');
  Gerador.wCampo(tcDe2, '', 'vServ', 01, 15, 0, NFSe.Servico.Valores.ValorServicos, '');
  Gerador.wCampo(tcDe2, '', 'vDesc', 01, 15, 0, NFSe.Servico.Valores.DescontoIncondicionado, '');
  Gerador.wCampo(tcDe2, '', 'vtNF' , 01, 15, 1,  NFSe.Servico.Valores.ValorServicos, '');

//    if NFSe.CondicaoPagamento.Parcelas.Count > 0 then
    Gerador.wCampo(tcDe2, '', 'vtLiq', 01, 15, 1, NFSe.Servico.Valores.ValorLiquidoNfse, '');
//    else
//      Gerador.wCampo(tcDe2, '', 'vtLiq', 01, 15, 1, NFSe.Servico.Valores.ValorServicos, '');

//  Gerador.wCampo(tcDe2, '', 'totalAproxTrib', 01, 15, 1, 0, '');

  // Total Retenção IRRF
  if (NFSe.Servico.Valores.ValorIr + NFSe.Servico.Valores.ValorPis +
      NFSe.Servico.Valores.ValorCofins + NFSe.Servico.Valores.ValorCsll +
      NFSe.Servico.Valores.ValorInss) > 0 then
  begin
    Gerador.wGrupo('Ret');

    Gerador.wCampo(tcDe2, '', 'vRetIR', 01, 15, 0, NFSe.Servico.Valores.ValorIr, '');
    Gerador.wCampo(tcDe2, '', 'vRetPISPASEP', 01, 15, 0, NFSe.Servico.Valores.ValorPis, '');
    Gerador.wCampo(tcDe2, '', 'vRetCOFINS', 01, 15, 0, NFSe.Servico.Valores.ValorCofins, '');
    Gerador.wCampo(tcDe2, '', 'vRetCSLL', 01, 15, 0, NFSe.Servico.Valores.ValorCsll, '');
    Gerador.wCampo(tcDe2, '', 'vRetINSS', 01, 15, 1, NFSe.Servico.Valores.ValorInss, '');
//    if NFSe.Servico.Valores.ValorInss > 0 then
//    begin
//      //Felipe Não é necessario este valor
////      Gerador.wCampo(tcStr, '', 'xRetINSS', 01, 02, 1, 'Retenção INSS', '');
//      Gerador.wCampo(tcDe2, '', 'vRetINSS', 01, 15, 1, NFSe.Servico.Valores.ValorInss, '');
//    end;

    Gerador.wGrupo('/Ret');
  end;

  if NFSe.CondicaoPagamento.Parcelas.Count > 0 then
    Gerador.wCampo(tcDe2, '', 'vtLiqFaturas', 01, 15, 1, NFSe.Servico.Valores.ValorLiquidoNfse, '');

  if NFSe.Servico.Valores.ValorDespesasNaoTributaveis > 0 then
    Gerador.wCampo(tcDe2, '', 'vtDespesas', 01, 15, 1, NFSe.Servico.Valores.ValorDespesasNaoTributaveis, '');

  // Total Retenção ISSQN
  if NFSe.Servico.Valores.IssRetido = stRetencao then
  begin  // 1 - stRetencao
    Gerador.wGrupo('ISS');
    Gerador.wCampo(tcDe2, '', 'vBCSTISS', 01, 15, 1, NFSe.Servico.Valores.ValorServicos, '');
    Gerador.wCampo(tcDe2, '', 'vSTISS'  , 01, 15, 1, NFSe.Servico.Valores.ValorIssRetido, '');
    Gerador.wGrupo('/ISS');
  end;

  if NFSe.Servico.Valores.IssRetido = stNormal then
  begin  // 2 - stNormal
    Gerador.wGrupo('ISS');
    Gerador.wCampo(tcDe2, '', 'vBCISS', 01, 15, 0, FdTotBCISS, '');
    Gerador.wCampo(tcDe2, '', 'vISS'  , 01, 15, 0, FdTotISS, '');
    Gerador.wGrupo('/ISS');
  end;

  Gerador.wGrupo('/total');
end;

procedure TNFSeW_Infisc.GerarCondicaoPagamento_v11;
var
  i: Integer;
begin
  if NFSe.CondicaoPagamento.Parcelas.Count > 0 then
  begin
    Gerador.wGrupo('faturas');
    for i := 0 to NFSe.CondicaoPagamento.Parcelas.Count -1 do
    begin
      Gerador.wGrupo('fat');
      Gerador.wCampo(tcStr, '', 'nItem', 01, 15,  1, IntToStr(i+1), '');
      Gerador.wCampo(tcStr, '', 'nFat',  01, 15,  1, NFSe.CondicaoPagamento.Parcelas[i].Parcela, '');
      Gerador.wCampo(tcDat, '', 'dVenc', 01, 15,  1, NFSe.CondicaoPagamento.Parcelas[i].DataVencimento, DSC_DEMI);
      Gerador.wCampo(tcDe2, '', 'vFat',  01, 15,  1, NFSe.CondicaoPagamento.Parcelas[i].Valor, '');
      Gerador.wGrupo('/fat');
    end;
    Gerador.wGrupo('/faturas');
  end;
end;

procedure TNFSeW_Infisc.GerarXML_Infisc_v11;
var
  lLimiteLinha: Integer;
  lDeOndeIniciaCopia: Integer;
  lTexto: String;
  lNumeroCaracteres: Integer;
  lResultadoDivisao: Word;
  lResultadoSobra: Word;
  lIndex: Integer;
begin
  Gerador.Prefixo := FPrefixo4;

  Gerador.wGrupo('NFS-e');

  Gerador.wGrupo('infNFSe versao="1.1"');

  GerarIdentificacaoRPS_v11;
  GerarPrestador_v11;
  GerarTomador_v11;

  //Dados da Obra caso empreitada global seja construção (1)
  if EmpreitadaGlobalToStr(NFSe.EmpreitadaGlobal) = '1' then
    GerarConstrucaoCivil;

  GerarTransportadora;
  GerarListaServicos_v11;
  GerarDespesasAdicionaisNaoTributaveis;
  GerarValoresServico_v11;
  GerarCondicaoPagamento_v11;

  // - Felipe Mesturini
  //Quando é tributado fora do municipio temos que ter o IBGE o mesmo do localdeTributação do Serviço
  Gerador.wCampo(tcStr, '', 'infAdicLT', 01, 100,  1, NFSe.Servico.MunicipioIncidencia, '');

  lIndex := 0;
  lLimiteLinha := 250;
  lDeOndeIniciaCopia := 1;
  lTexto := NFSe.OutrasInformacoes;
  lNumeroCaracteres := Length(lTexto);

  lResultadoDivisao := lNumeroCaracteres div lLimiteLinha;
  lResultadoSobra := lNumeroCaracteres mod lLimiteLinha;

  if (lResultadoDivisao > 0) then
  begin
    repeat
      lDeOndeIniciaCopia := lIndex * lLimiteLinha;
      Gerador.wCampo(tcStr, '', 'infAdic', 01, 100, 1, Copy(lTexto, lDeOndeIniciaCopia, lLimiteLinha), '');
      inc(lIndex);
    until lIndex > (lResultadoDivisao -1);

    lDeOndeIniciaCopia := (lIndex * lLimiteLinha);
  end;
  if (lResultadoSobra > 0) and (lDeOndeIniciaCopia > 0) then
    Gerador.wCampo(tcStr, '', 'infAdic', 01, 100, 1, Copy(lTexto, lIndex * lLimiteLinha, lLimiteLinha), '');

  Gerador.wGrupo('/infNFSe');

  Gerador.wGrupo('/NFS-e');
end;

////////////////////////////////////////////////////////////////////////////////
// **************************** Ambas Versões
////////////////////////////////////////////////////////////////////////////////

procedure TNFSeW_Infisc.GerarTransportadora;
begin
  if NFSe.Transportadora.xCpfCnpjTrans <> '' then
  begin
    Gerador.wGrupo('transportadora');

    Gerador.wCampo(tcStr, '', 'xNomeTrans'   , 01, 100, 1, NFSe.Transportadora.xNomeTrans, '');
    Gerador.wCampo(tcStr, '', 'xCpfCnpjTrans', 01, 014, 1, NFSe.Transportadora.xCpfCnpjTrans, '');
    Gerador.wCampo(tcStr, '', 'xInscEstTrans', 01, 015, 1, NFSe.Transportadora.xInscEstTrans, '');
    Gerador.wCampo(tcStr, '', 'xPlacaTrans'  , 01, 007, 1, NFSe.Transportadora.xPlacaTrans, '');
    Gerador.wCampo(tcStr, '', 'xEndTrans'    , 01, 100, 1, NFSe.Transportadora.xEndTrans, '');
    Gerador.wCampo(tcInt, '', 'cMunTrans'    , 01, 007, 1, NFSe.Transportadora.cMunTrans, '');
    Gerador.wCampo(tcStr, '', 'xMunTrans'    , 01, 060, 1, NFSe.Transportadora.xMunTrans, '');
    Gerador.wCampo(tcStr, '', 'xUfTrans'     , 01, 002, 1, NFSe.Transportadora.xUFTrans, '');
    Gerador.wCampo(tcInt, '', 'cPaisTrans'   , 01, 004, 1, NFSe.Transportadora.cPaisTrans, '');
    Gerador.wCampo(tcStr, '', 'xPaisTrans'   , 01, 100, 1, NFSe.Transportadora.xPaisTrans, '');

    Gerador.wCampo(tcInt, '', 'vTipoFreteTrans', 01, 01, 1, TipoFreteToStr(NFSe.Transportadora.vTipoFreteTrans), '');

    Gerador.wGrupo('/transportadora');
  end;
end;

procedure TNFSeW_Infisc.GerarConstrucaoCivil;
begin
  Gerador.wGrupo('dadosDaObra');
  Gerador.wCampo(tcStr, '', 'xLogObra',    01, 100, 1, NFSe.ConstrucaoCivil.LogradouroObra, '');
  Gerador.wCampo(tcStr, '', 'xComplObra',  01, 100, 1, NFSe.ConstrucaoCivil.ComplementoObra, '');
  Gerador.wCampo(tcStr, '', 'vNumeroObra', 01,  06, 1, NFSe.ConstrucaoCivil.NumeroObra, '');
  Gerador.wCampo(tcStr, '', 'xBairroObra', 01, 100, 1, NFSe.ConstrucaoCivil.BairroObra, '');
  Gerador.wCampo(tcStr, '', 'xCepObra',    01,  08, 1, NFSe.ConstrucaoCivil.CEPObra, '');
  Gerador.wCampo(tcStr, '', 'cCidadeObra', 01,  07, 1, NFSe.ConstrucaoCivil.CodigoMunicipioObra, '');
  Gerador.wCampo(tcStr, '', 'xCidadeObra', 01,  60, 1, copy(CodCidadeToCidade(StrToIntDef(NFSe.ConstrucaoCivil.CodigoMunicipioObra,0)),
                                                           0, pos('/',CodCidadeToCidade(StrToIntDef(NFSe.ConstrucaoCivil.CodigoMunicipioObra,0)))-1), '');
  Gerador.wCampo(tcStr, '', 'xUfObra',     01,  02, 1, NFSe.ConstrucaoCivil.UFObra, '');
  Gerador.wCampo(tcInt, '', 'cPaisObra',   01, 100, 1, NFSe.ConstrucaoCivil.CodigoPaisObra, '');
  Gerador.wCampo(tcStr, '', 'xPaisObra',   01, 100, 1, NFSe.ConstrucaoCivil.xPaisObra, '');
  // Campos opcionais
  Gerador.wCampo(tcStr, '', 'numeroArt',   01,  12, 0, NFSe.ConstrucaoCivil.Art,    ''); //num. ART 12 digitos
  Gerador.wCampo(tcStr, '', 'numeroCei',   01,  12, 0, NFSe.ConstrucaoCivil.nCei,   ''); //num. CEI 12 digitos
  Gerador.wCampo(tcStr, '', 'numeroProj',  01,  15, 0, NFSe.ConstrucaoCivil.nProj,  ''); //num. projeto
  Gerador.wCampo(tcStr, '', 'numeroMatri', 01,  15, 0, NFSe.ConstrucaoCivil.nMatri, ''); //num. matricula
  Gerador.wGrupo('/dadosDaObra');
end;

procedure TNFSeW_Infisc.GerarDespesasAdicionaisNaoTributaveis;
var
  lItem: Integer;
  lItemDepesa: TDespesaCollectionItem;
begin
  //Criação de despesas adicionais não tributaveis
  {Usado para inclusão de serviços que não são tributaveis e agregam valor no total da nota}
  if NFSe.Despesa.Count > 0 then
  begin
    Gerador.wGrupo('despesas');
    for lItem := 0 to Pred(NFSe.Despesa.Count) do
    begin
      lItemDepesa := NFSe.Despesa.Items[lItem];
      Gerador.wGrupo('despesa');
      Gerador.wCampo(tcStr, '', 'nItemDesp',    01, 100, 1, lItemDepesa.nItemDesp, '');
      Gerador.wCampo(tcStr, '', 'xDesp',  01, 100, 1, lItemDepesa.xDesp, '');
      Gerador.wCampo(tcDat, '', 'dDesp', 01, 15,  1, lItemDepesa.dDesp, DSC_DEMI);
      Gerador.wCampo(tcDe2, '', 'vDesp',  01, 15,  1, lItemDepesa.vDesp, '');
      Gerador.wGrupo('/despesa');
    end;
    Gerador.wGrupo('/despesas');
  end;
end;

end.
