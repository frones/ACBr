{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit pnfsNFSeW_Infisc;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ELSE}

{$ENDIF}
  SysUtils, Classes, StrUtils,
  synacode, ACBrConsts,
  pnfsNFSeW,
  pcnAuxiliar, pcnConversao, pcnGerador,
  pnfsNFSe, pnfsConversao;

type
  { TNFSeW_Infisc }

  TNFSeW_Infisc = class(TNFSeWClass)
  private
    dTotBCISS: Double;
    dTotISS: Double;
  protected

    procedure GerarIdentificacaoRPS; override;
    procedure GerarRPSSubstituido; override;

    procedure GerarPrestador; override;
    procedure GerarTomador; override;
    procedure GerarIntermediarioServico; override;

    procedure GerarServicoValores; override;
    procedure GerarListaServicos; override;
    procedure GerarValoresServico; override;

    procedure GerarConstrucaoCivil; override;
    procedure GerarCondicaoPagamento; override;

    procedure GerarXML_Infisc;

  public
    constructor Create(ANFSeW: TNFSeW); override;

    function ObterNomeArquivo: String; override;
    function GerarXml: Boolean; override;
  end;

implementation

uses
  ACBrUtil;

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

procedure TNFSeW_Infisc.GerarIdentificacaoRPS;
var
  sChave, cNFSe, serie, nNFSe:String;
begin
   sChave := NFSe.ChaveNFSe;
   serie  := NFSE.SeriePrestacao;
   nNFSe  := NFSe.Numero;
   cNFSe  := copy(sChave,31,9);

  Gerador.wGrupoNFSe('Id');
  Gerador.wCampoNFSe(tcStr, '', 'cNFS-e', 01, 09, 1, cNFSe, '');

  if versaoNFSe = ve100 then
    Gerador.wCampoNFSe(tcStr, '', 'natOp' , 01, 50, 1, NaturezaOperacaoToStr(NFSe.NaturezaOperacao), '');

  Gerador.wCampoNFSe(tcStr, '', 'mod'   , 01, 02, 1, NFSe.ModeloNFSe, '');  // segundo manual obrigatório ser 98
  Gerador.wCampoNFSe(tcStr, '', 'serie' , 01, 03, 1, serie, '');          // tem que ser S - ver como está sendo passado a variável "serie"
  Gerador.wCampoNFSe(tcStr, '', 'nNFS-e', 01, 09, 1, nNFSe, '');
  Gerador.wCampoNFSe(tcStr, '', 'dEmi'  , 01, 10, 1, FormatDateTime('yyyy-mm-dd',NFSe.DataEmissao), '');
  Gerador.wCampoNFSe(tcStr, '', 'hEmi'  , 01, 10, 1, FormatDateTime('hh:mm',NFSe.DataEmissao), '');
  Gerador.wCampoNFSe(tcStr, '', 'tpNF'  , 01, 01, 1, '1', '');     // 0- Entrada 1- Saída

  if versaoNFSe = ve100 then
    Gerador.wCampoNFSe(tcStr, '', 'cMunFG', 01, 07,  1, NFSe.Servico.CodigoMunicipio, '');

  Gerador.wCampoNFSe(tcStr, '', 'refNF' , 01, 39, 1, sChave, ''); // chave de acesso 39 caracteres
  Gerador.wCampoNFSe(tcStr, '', 'tpEmis', 01, 01, 1, TipoEmissaoToStr(NFSe.TipoEmissao), ''); // N- Normal C- Contigencia

  if versaoNFSe = ve100 then
    Gerador.wCampoNFSe(tcStr, '', 'anulada', 01, 01, 1, 'N', '')
  else begin
    if NFSe.Cancelada = snNao then
      Gerador.wCampoNFSe(tcStr, '', 'cancelada', 01, 01, 1, 'N', '')    // N- Não
    else
      Gerador.wCampoNFSe(tcStr, '', 'cancelada', 01, 01, 1, 'S', '');   // S- Sim

    // ambiente 1- producao 2- homologacao
    Gerador.wCampoNFSe(tcStr, '', 'ambienteEmi'     , 01, 01, 1, SimNaoToStr(NFSe.Producao), '');
    // forma de emissao 1- portal contribuinte 2- servidor web 3- submetidos via Upload no portal 4- emissao via RPS
    Gerador.wCampoNFSe(tcStr, '', 'formaEmi'        , 01, 01, 1, '2', '');
    // 1- construcao civil 2- outros casos
    Gerador.wCampoNFSe(tcStr, '', 'empreitadaGlobal', 01, 01, 1, EmpreitadaGlobalToStr(NFSe.EmpreitadaGlobal), '');
  end;

  Gerador.wGrupoNFSe('/Id');
end;

procedure TNFSeW_Infisc.GerarRPSSubstituido;
begin
  inherited;

end;

procedure TNFSeW_Infisc.GerarPrestador;
begin
  if versaoNFSe = ve100 then
    Gerador.wGrupoNFSe('emit')
  else
    Gerador.wGrupoNFSe('prest');

  Gerador.wCampoNFSe(tcStr, '', 'CNPJ' , 01, 014, 1, NFSe.Prestador.Cnpj, '');
  Gerador.wCampoNFSe(tcStr, '', 'xNome', 01, 100, 1, NFSe.PrestadorServico.RazaoSocial, '');
  Gerador.wCampoNFSe(tcStr, '', 'xFant', 01, 060, 1, NFSe.PrestadorServico.NomeFantasia, '');
  Gerador.wCampoNFSe(tcStr, '', 'IM'   , 01, 015, 1, NFSe.Prestador.InscricaoMunicipal, '');

  Gerador.wGrupoNFSe('end');
  Gerador.wCampoNFSe(tcStr, '', 'xLgr'   , 01, 100, 1, NFSe.PrestadorServico.Endereco.Endereco, '');
  Gerador.wCampoNFSe(tcStr, '', 'nro'    , 01, 015, 1, NFSe.PrestadorServico.Endereco.Numero, '');
  Gerador.wCampoNFSe(tcStr, '', 'xCpl'   , 01, 100, 1, NFSe.PrestadorServico.Endereco.Complemento, '');
  Gerador.wCampoNFSe(tcStr, '', 'xBairro', 01, 100, 1, NFSe.PrestadorServico.Endereco.Bairro, '');
  Gerador.wCampoNFSe(tcStr, '', 'cMun'   , 01, 007, 1, NFSe.PrestadorServico.Endereco.CodigoMunicipio, '');
  Gerador.wCampoNFSe(tcStr, '', 'xMun'   , 01, 060, 1, copy(CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio,0)),
                                                0, pos('/',CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio,0)))-1), '');
  Gerador.wCampoNFSe(tcStr, '', 'UF'     , 01, 002, 1, NFSe.PrestadorServico.Endereco.UF, '');
  Gerador.wCampoNFSe(tcStr, '', 'CEP'    , 01, 008, 1, NFSe.PrestadorServico.Endereco.CEP, '');
  Gerador.wCampoNFSe(tcInt, '', 'cPais'  , 01, 100, 1, NFSe.PrestadorServico.Endereco.CodigoPais, '');
  Gerador.wCampoNFSe(tcStr, '', 'xPais'  , 01, 100, 1, NFSe.PrestadorServico.Endereco.xPais, '');
  Gerador.wGrupoNFSe('/end');

  Gerador.wCampoNFSe(tcStr, '', 'fone'  , 01, 100, 1, NFSe.PrestadorServico.Contato.Telefone, '');
  Gerador.wCampoNFSe(tcStr, '', 'xEmail', 01, 100, 1, NFSe.PrestadorServico.Contato.Email, '');
  Gerador.wCampoNFSe(tcStr, '', 'IE'    , 01, 015, 1, NFSe.Prestador.InscricaoEstadual, '');

  case Nfse.RegimeEspecialTributacao of
    retNenhum,
    retMicroempresaMunicipal,
    retEstimativa,
    retSociedadeProfissionais,
    retCooperativa,
    retMicroempresarioIndividual    : ; // Não tem informação no manual

    retSimplesNacional              : Gerador.wCampoNFSe(tcStr, '', 'regimeTrib', 01, 01, 1, '1', ''); // 1 - Simples

    retMicroempresarioEmpresaPP     : Gerador.wCampoNFSe(tcStr, '', 'regimeTrib', 01, 01, 1, '2', ''); // 2 - SIMEI

    retLucroReal, retLucroPresumido : Gerador.wCampoNFSe(tcStr, '', 'regimeTrib', 01, 01, 1, '3', ''); // 3 - Normal
  end;

  if versaoNFSe = ve100 then
    Gerador.wGrupoNFSe('/emit')
  else
    Gerador.wGrupoNFSe('/prest');
end;

procedure TNFSeW_Infisc.GerarTomador;
begin
  Gerador.wGrupoNFSe('TomS');

  if Length(NFSe.Tomador.IdentificacaoTomador.CpfCnpj) = 11 then
    Gerador.wCampoNFSe(tcStr, '', 'CPF' , 01, 11, 1, NFSe.Tomador.IdentificacaoTomador.CpfCnpj, '')
  else
    Gerador.wCampoNFSe(tcStr, '', 'CNPJ', 01, 14, 1, NFSe.Tomador.IdentificacaoTomador.CpfCnpj, '');

  Gerador.wCampoNFSe(tcStr, '', 'xNome', 01, 100, 1, NFSe.Tomador.RazaoSocial, '');

  Gerador.wGrupoNFSe('ender');
  Gerador.wCampoNFSe(tcStr, '', 'xLgr'   , 01, 100, 1, NFSe.Tomador.Endereco.Endereco, '');
  Gerador.wCampoNFSe(tcStr, '', 'nro'    , 01, 015, 1, NFSe.Tomador.Endereco.Numero, '');
  Gerador.wCampoNFSe(tcStr, '', 'xCpl'   , 01, 100, 1, NFSe.Tomador.Endereco.Complemento, '');
  Gerador.wCampoNFSe(tcStr, '', 'xBairro', 01, 100, 1, NFSe.Tomador.Endereco.Bairro, '');
  Gerador.wCampoNFSe(tcStr, '', 'cMun'   , 01, 007, 1, NFSe.Tomador.Endereco.CodigoMunicipio, '');
  Gerador.wCampoNFSe(tcStr, '', 'xMun'   , 01, 060, 1, copy(CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio,0)),
                                                        0, pos('/',CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio,0)))-1), '');
  Gerador.wCampoNFSe(tcStr, '', 'UF'     , 01, 002, 1, NFSe.Tomador.Endereco.UF, '');
  Gerador.wCampoNFSe(tcStr, '', 'CEP'    , 01, 008, 1, NFSe.Tomador.Endereco.CEP, '');
  Gerador.wCampoNFSe(tcInt, '', 'cPais'  , 01, 100, 1, NFSe.Tomador.Endereco.CodigoPais, '');
  Gerador.wCampoNFSe(tcStr, '', 'xPais'  , 01, 100, 1, NFSe.Tomador.Endereco.xPais, '');
  Gerador.wCampoNFSe(tcStr, '', 'fone'   , 01, 100, 1, NFSe.Tomador.Contato.Telefone, '');
  Gerador.wGrupoNFSe('/ender');

  Gerador.wCampoNFSe(tcStr, '', 'xEmail', 01, 100, 1, NFSe.Tomador.Contato.Email, '');
  Gerador.wCampoNFSe(tcStr, '', 'IM'    , 01, 015, 1, NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, '');
  Gerador.wCampoNFSe(tcStr, '', 'IE'    , 01, 015, 1, NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, '');

  if NFSe.Servico.MunicipioIncidencia <> 0 then
  begin
    if (NFSe.Servico.MunicipioIncidencia = 4303905) then
      Gerador.wCampoNFSe(tcStr, '', 'Praca', 01, 60, 1, 'Campo Bom-RS', '')
    else
      Gerador.wCampoNFSe(tcStr, '', 'Praca', 01, 60, 1, CodCidadeToCidade(NFSe.Servico.MunicipioIncidencia), '');
  end;

  Gerador.wGrupoNFSe('/TomS');
end;

procedure TNFSeW_Infisc.GerarIntermediarioServico;
begin
  inherited;

end;

procedure TNFSeW_Infisc.GerarServicoValores;
begin
  inherited;

end;

procedure TNFSeW_Infisc.GerarListaServicos;
var
  i: Integer;
  cServ, xServ: String;
begin
  dTotBCISS := 0;
  dTotISS   := 0;

  for i := 0 to NFSe.Servico.ItemServico.Count-1 do
  begin
    cServ := NFSe.Servico.ItemServico.Items[i].codServ; // cod. municipal
    xServ := NFSe.Servico.ItemServico.Items[i].Descricao;

    if (versaoNFSe = ve100) and (NFSe.Servico.ItemListaServico <> '') then
      xServ := xServ + ' (Class.: ' + NFSe.Servico.ItemListaServico + ')';

    Gerador.wGrupoNFSe('det');
    Gerador.wCampoNFSe(tcStr, '', 'nItem', 01, 02, 1, IntToStr(i+1), '');

    Gerador.wGrupoNFSe('serv');
    Gerador.wCampoNFSe(tcStr, '', 'cServ'            , 01, 002, 1, cServ, '');  // Código municipal do serviço, apenas números
    Gerador.wCampoNFSe(tcStr, '', 'cLCServ'          , 01, 004, 1, NFSe.Servico.ItemServico.Items[i].codLCServ, ''); // Código do serviço conforme lei compl. 116
    Gerador.wCampoNFSe(tcStr, '', 'xServ'            , 01, 120, 1, xServ, ''); // Discriminação do serviço
    Gerador.wCampoNFSe(tcStr, '', 'localTributacao'  , 01, 004, 1, IntToStr(NFSe.Servico.MunicipioIncidencia), ''); // Local tributacao conforme codificacao do IBGE
    Gerador.wCampoNFSe(tcStr, '', 'localVerifResServ', 01, 004, 1, '1', ''); // Local da verificacao: 1 - Brasil 2 - Exterior
    Gerador.wCampoNFSe(tcStr, '', 'uTrib'            , 01, 006, 1, NFSe.Servico.ItemServico.Items[i].Unidade, '');       // unidade
    Gerador.wCampoNFSe(tcDe2, '', 'qTrib'            , 01, 015, 1, NFSe.Servico.ItemServico.Items[i].Quantidade, '');    // quantidade
    Gerador.wCampoNFSe(tcDe2, '', 'vUnit'            , 01, 015, 1, NFSe.Servico.ItemServico.Items[i].ValorUnitario, ''); // formatacao com 2 casas
    // (valor unitario * quantidade) - desconto
    Gerador.wCampoNFSe(tcDe2, '', 'vServ'            , 01, 015, 1, NFSe.Servico.ItemServico.Items[i].ValorServicos, '');
    Gerador.wCampoNFSe(tcDe2, '', 'vDesc'            , 01, 015, 1, NFSe.Servico.ItemServico.Items[i].DescontoIncondicionado, '');
    Gerador.wCampoNFSe(tcDe2, '', 'vBCISS'           , 01, 015, 1, NFSe.Servico.ItemServico.Items[i].BaseCalculo, '');
    Gerador.wCampoNFSe(tcDe2, '', 'pISS'             , 01, 015, 1, NFSe.Servico.ItemServico.Items[i].Aliquota, '');
    Gerador.wCampoNFSe(tcDe2, '', 'vISS'             , 01, 015, 1, NFSe.Servico.ItemServico.Items[i].ValorIss, '');

    dTotBCISS := dTotBCISS + NFSe.Servico.ItemServico.Items[i].BaseCalculo;
    dTotISS   := dTotISS   + NFSe.Servico.ItemServico.Items[i].ValorIss;

    Gerador.wCampoNFSe(tcDe2, '', 'pRed', 01, 15, 1, 0, '');
    Gerador.wCampoNFSe(tcDe2, '', 'vRed', 01, 15, 1, 0, '');

    if NFSe.Servico.ItemServico.Items[i].ValorInss > 0 then
      Gerador.wCampoNFSe(tcDe2, '', 'vRetINSS', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].ValorInss, '');

    if (versaoNFSe = ve100) then
    begin
      if NFSe.Servico.ItemServico.Items[i].ValorIr > 0 then
      begin
        //Gerador.wCampoNFSe(tcStr, '', 'xRetIRF', 01, 02,  1, 'Retenção IRRF', '');
        Gerador.wCampoNFSe(tcDe2, '', 'vRetIRF', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].ValorIr, '');
      end;

      if NFSe.Servico.ItemServico.Items[i].ValorPis > 0 then
      begin
        //Gerador.wCampoNFSe(tcStr, '', 'xRetLei10833-PIS-PASEP', 01, 02,  1, 'Retenção PIS', '');
        Gerador.wCampoNFSe(tcDe2, '', 'vRetLei10833-PIS-PASEP', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].ValorPis, '');
      end;

      if NFSe.Servico.ItemServico.Items[i].ValorCofins > 0 then
      begin
        //Gerador.wCampoNFSe(tcStr, '', 'xRetLei10833-COFINS', 01, 02,  1, 'Retenção COFINS', '');
        Gerador.wCampoNFSe(tcDe2, '', 'vRetLei10833-COFINS', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].ValorCofins, '');
      end;

      if NFSe.Servico.ItemServico.Items[i].ValorCsll > 0 then
      begin
        //Gerador.wCampoNFSe(tcStr, '', 'xRetLei10833-CSLL', 01, 02,  1, 'Retenção CSLL', '');
        Gerador.wCampoNFSe(tcDe2, '', 'vRetLei10833-CSLL', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].ValorCsll, '');
      end;
    end
    else begin
      if NFSe.Servico.ItemServico.Items[i].ValorIr > 0 then
      begin
        //Gerador.wCampoNFSe(tcStr, '', 'pRetIR', 01, 02, 1, 'Retenção IRRF', '');
        Gerador.wCampoNFSe(tcDe2, '', 'vRetIR', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].ValorIr, '');
      end;

      if NFSe.Servico.ItemServico.Items[i].ValorPis > 0 then
      begin
        //Gerador.wCampoNFSe(tcStr, '', 'pRetPISPASEP', 01, 02, 1, 'Retenção PIS', '');
        Gerador.wCampoNFSe(tcDe2, '', 'vRetPISPASEP', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].ValorPis, '');
      end;

      if NFSe.Servico.ItemServico.Items[i].ValorCofins > 0 then
      begin
        //Gerador.wCampoNFSe(tcStr, '', 'pRetCOFINS', 01, 02, 1, 'Retenção COFINS', '');
        Gerador.wCampoNFSe(tcDe2, '', 'vRetCOFINS', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].ValorCofins, '');
      end;

      if NFSe.Servico.ItemServico.Items[i].ValorCsll > 0 then
      begin
        //Gerador.wCampoNFSe(tcStr, '', 'pRetCSLL', 01, 02, 1, 'Retenção CSLL', '');
        Gerador.wCampoNFSe(tcDe2, '', 'vRetCSLL', 01, 15, 1, NFSe.Servico.ItemServico.Items[i].ValorCsll, '');
      end;
    end;

    Gerador.wGrupoNFSe('/serv');

    Gerador.wGrupoNFSe('ISSST');
    Gerador.wCampoNFSe(tcDe2, '', 'vISSST', 01, 15, 1, 0, '');
    Gerador.wGrupoNFSe('/ISSST');

    Gerador.wGrupoNFSe('/det');
  end;
end;

procedure TNFSeW_Infisc.GerarValoresServico;
begin
  Gerador.wGrupoNFSe('total');

  Gerador.wCampoNFSe(tcDe2, '', 'vServ'         , 01, 15, 1, NFSe.Servico.Valores.ValorServicos, '');
  Gerador.wCampoNFSe(tcDe2, '', 'vDesc'         , 01, 15, 1, NFSe.Servico.Valores.DescontoIncondicionado, '');
  Gerador.wCampoNFSe(tcDe2, '', 'vOutro'        , 01, 15, 1, 0, '');
  Gerador.wCampoNFSe(tcDe2, '', 'vtNF'          , 01, 15, 1,  NFSe.Servico.Valores.ValorLiquidoNfse, '');
  Gerador.wCampoNFSe(tcDe2, '', 'vtLiq'         , 01, 15, 1, NFSe.Servico.Valores.ValorLiquidoNfse, '');
  Gerador.wCampoNFSe(tcDe2, '', 'totalAproxTrib', 01, 15, 1, 0, '');

  if (NFSe.Servico.Valores.ValorIr + NFSe.Servico.Valores.ValorPis +
      NFSe.Servico.Valores.ValorCofins + NFSe.Servico.Valores.ValorCsll +
      NFSe.Servico.Valores.ValorInss) > 0 then
  begin
    Gerador.wGrupoNFSe('Ret');

    if versaoNFSe = ve100 then
    begin
      if NFSe.Servico.Valores.ValorIr > 0 then
      begin
        //Gerador.wCampoNFSe(tcStr, '', 'xRetIRF', 01, 02, 1, 'Retenção IRRF', '');
        Gerador.wCampoNFSe(tcDe2, '', 'vRetIRF', 01, 15, 1, NFSe.Servico.Valores.ValorIr, '');
      end;

      if NFSe.Servico.Valores.ValorPis > 0 then
      begin
        //Gerador.wCampoNFSe(tcStr, '', 'xRetLei10833-PIS-PASEP', 01, 02, 1, 'Retenção PIS', '');
        Gerador.wCampoNFSe(tcDe2, '', 'vRetLei10833-PIS-PASEP', 01, 15, 1, NFSe.Servico.Valores.ValorPis, '');
      end;

      if NFSe.Servico.Valores.ValorCofins > 0 then
      begin
        //Gerador.wCampoNFSe(tcStr, '', 'xRetLei10833-COFINS', 01, 02, 1, 'Retenção COFINS', '');
        Gerador.wCampoNFSe(tcDe2, '', 'vRetLei10833-COFINS', 01, 15, 1, NFSe.Servico.Valores.ValorCofins, '');
      end;

      if NFSe.Servico.Valores.ValorCsll > 0 then
      begin
        //Gerador.wCampoNFSe(tcStr, '', 'xRetLei10833-CSLL', 01, 02, 1, 'Retenção CSLL', '');
        Gerador.wCampoNFSe(tcDe2, '', 'vRetLei10833-CSLL', 01, 15, 1, NFSe.Servico.Valores.ValorCsll, '');
      end;
      if NFSe.Servico.Valores.ValorInss > 0 then
      begin
        //Gerador.wCampoNFSe(tcStr, '', 'xRetINSS', 01, 02, 1, 'Retenção INSS', '');
        Gerador.wCampoNFSe(tcDe2, '', 'vRetINSS', 01, 15, 1, NFSe.Servico.Valores.ValorInss, '');
      end;
    end
    else begin
      if NFSe.Servico.Valores.ValorIr > 0 then
      begin
        //Gerador.wCampoNFSe(tcStr, '', 'xRetIR', 01, 02, 1, 'Retenção IRRF', '');
        Gerador.wCampoNFSe(tcDe2, '', 'vRetIR', 01, 15, 1, NFSe.Servico.Valores.ValorIr, '');
      end;

      if NFSe.Servico.Valores.ValorPis > 0 then
      begin
        //Gerador.wCampoNFSe(tcStr, '', 'pRetPISPASEP', 01, 02, 1, 'Retenção PIS', '');
        Gerador.wCampoNFSe(tcDe2, '', 'vRetPISPASEP', 01, 15, 1, NFSe.Servico.Valores.ValorPis, '');
      end;

      if NFSe.Servico.Valores.ValorCofins > 0 then
      begin
        //Gerador.wCampoNFSe(tcStr, '', 'pRetCOFINS', 01, 02, 1, 'Retenção COFINS', '');
        Gerador.wCampoNFSe(tcDe2, '', 'vRetCOFINS', 01, 15, 1, NFSe.Servico.Valores.ValorCofins, '');
      end;

      if NFSe.Servico.Valores.ValorCsll > 0 then
      begin
        //Gerador.wCampoNFSe(tcStr, '', 'pRetCSLL', 01, 02, 1, 'Retenção CSLL', '');
        Gerador.wCampoNFSe(tcDe2, '', 'vRetCSLL', 01, 15, 1, NFSe.Servico.Valores.ValorCsll, '');
      end;

      if NFSe.Servico.Valores.ValorInss > 0 then
      begin
        //Gerador.wCampoNFSe(tcStr, '', 'xRetINSS', 01, 02, 1, 'Retenção INSS', '');
        Gerador.wCampoNFSe(tcDe2, '', 'vRetINSS', 01, 15, 1, NFSe.Servico.Valores.ValorInss, '');
      end;
    end;

    Gerador.wGrupoNFSe('/Ret');
  end;

  Gerador.wCampoNFSe(tcDe2, '', 'vtLiqFaturas', 01, 15, 1, NFSe.Servico.Valores.ValorLiquidoNfse, '');

  Gerador.wGrupoNFSe('ISS');
  Gerador.wCampoNFSe(tcDe2, '', 'vBCISS'  , 01, 15, 1, dTotBCISS, '');
  Gerador.wCampoNFSe(tcDe2, '', 'vISS'    , 01, 15, 1, dTotISS, '');
  Gerador.wCampoNFSe(tcDe2, '', 'vBCSTISS', 01, 15, 1, 0, '');
  Gerador.wCampoNFSe(tcDe2, '', 'vSTISS'  , 01, 15, 1, NFSe.Servico.Valores.ValorIssRetido, '');
  Gerador.wGrupoNFSe('/ISS');

  Gerador.wGrupoNFSe('/total');
end;

procedure TNFSeW_Infisc.GerarConstrucaoCivil;
begin
  inherited;

end;

procedure TNFSeW_Infisc.GerarCondicaoPagamento;
var
  i: Integer;
begin
  Gerador.wGrupoNFSe('faturas');

  for i := 0 to NFSe.CondicaoPagamento.Parcelas.Count-1 do
  begin
    Gerador.wGrupoNFSe('fat');
    Gerador.wCampoNFSe(tcStr, '', 'nItem', 01, 15, 1, IntToStr(i+1), '');
    Gerador.wCampoNFSe(tcStr, '', 'nFat' , 01, 15, 1, NFSe.CondicaoPagamento.Parcelas[i].Parcela, '');
    Gerador.wCampoNFSe(tcDat, '', 'dVenc', 01, 15, 1, NFSe.CondicaoPagamento.Parcelas[i].DataVencimento, DSC_DEMI);
    Gerador.wCampoNFSe(tcDe2, '', 'vFat' , 01, 15, 1, NFSe.CondicaoPagamento.Parcelas[i].Valor, '');
    Gerador.wGrupoNFSe('/fat');
  end;

  Gerador.wGrupoNFSe('/faturas');
end;

function TNFSeW_Infisc.GerarXml: Boolean;
var
  Gerar: Boolean;
begin
  Gerador.ArquivoFormatoXML := '';
  Gerador.Prefixo           := FPrefixo4;

  FDefTipos := FServicoEnviar;

  if (RightStr(FURL, 1) <> '/') and (FDefTipos <> '')
    then FDefTipos := '/' + FDefTipos;

  if Trim(FPrefixo4) <> ''
    then Atributo := ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FURL + FDefTipos + '"'
    else Atributo := ' xmlns="' + FURL + FDefTipos + '"';

  Gerador.wGrupo('Rps' + Atributo);

  FNFSe.InfID.ID := FNFSe.Numero;

  GerarXML_Infisc;

  if FOpcoes.GerarTagAssinatura <> taNunca then
  begin
    Gerar := true;
    if FOpcoes.GerarTagAssinatura = taSomenteSeAssinada then
      Gerar := ((NFSe.signature.DigestValue <> '') and
                (NFSe.signature.SignatureValue <> '') and
                (NFSe.signature.X509Certificate <> ''));
    if FOpcoes.GerarTagAssinatura = taSomenteParaNaoAssinada then
      Gerar := ((NFSe.signature.DigestValue = '') and
                (NFSe.signature.SignatureValue = '') and
                (NFSe.signature.X509Certificate = ''));
    if Gerar then
    begin
      FNFSe.signature.URI := FNFSe.InfID.ID;
      FNFSe.signature.Gerador.Opcoes.IdentarXML := Gerador.Opcoes.IdentarXML;
      FNFSe.signature.GerarXMLNFSe;
      Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                   FNFSe.signature.Gerador.ArquivoFormatoXML;
    end;
  end;

  Gerador.wGrupo('/Rps');

  Gerador.gtAjustarRegistros(NFSe.InfID.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

procedure TNFSeW_Infisc.GerarXML_Infisc;
begin
  Gerador.Prefixo := '';
  Gerador.wGrupoNFSe('?xml version=''1.0'' encoding=''utf-8''?');
  Gerador.wGrupoNFSe('envioLote versao="1.0"');
  Gerador.wCampoNFSe(tcStr, '', 'CNPJ'   , 01, 14, 1, NFSe.Prestador.Cnpj, '');
  Gerador.wCampoNFSe(tcStr, '', 'dhTrans', 01, 19, 1, FormatDateTime('yyyy-mm-dd hh:mm:ss', Now), '');

  Gerador.wGrupoNFSe('NFS-e');

  if versaoNFSe = ve110 then
    Gerador.wGrupoNFSe('infNFSe versao="1.1"') // para Caxias do Sul Versão XML = 1.1
  else
    Gerador.wGrupoNFSe('infNFSe versao="1.00"'); // demais cidades

  GerarIdentificacaoRPS;
  GerarPrestador;
  GerarTomador;
  GerarListaServicos;
  GerarValoresServico;
  GerarCondicaoPagamento;

  Gerador.wCampoNFSe(tcStr, '', 'infAdicLT', 01, 100,  1, NFSe.PrestadorServico.Endereco.CodigoMunicipio, '');

  if Trim(NFSe.OutrasInformacoes) <> '' then
  begin
    Gerador.wGrupoNFSe('Observacoes');
    Gerador.wCampoNFSe(tcStr, '', 'xinf', 01, 100, 1, copy(NFSe.OutrasInformacoes,1,100), '');
    Gerador.wGrupoNFSe('/Observacoes');
  end;

  Gerador.wGrupoNFSe('/infNFSe');

  Gerador.wGrupoNFSe('/NFS-e');
  Gerador.wGrupoNFSe('/envioLote');
end;

end.
