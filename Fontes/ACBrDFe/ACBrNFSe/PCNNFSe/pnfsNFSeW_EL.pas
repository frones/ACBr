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

unit pnfsNFSeW_EL;

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
  { TNFSeW_EL }

  TNFSeW_EL = class(TNFSeWClass)
  private
  protected

    procedure GerarIdentificacaoRPS;
    procedure GerarRPSSubstituido;

    procedure GerarPrestador;
    procedure GerarTomador;
    procedure GerarIntermediarioServico;

    procedure GerarListaServicos;
    procedure GerarValoresServico;

    procedure GerarXML_EL;

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
{ layout do EL.                                                                }
{ Sendo assim só será criado uma nova unit para um novo layout.                }
{==============================================================================}

{ TNFSeW_EL }

procedure TNFSeW_EL.GerarIdentificacaoRPS;
begin
  Gerador.wGrupo('IdentificacaoRps');
  Gerador.wCampo(tcStr, '#1', 'Numero', 01, 15, 1, OnlyNumber(NFSe.IdentificacaoRps.Numero), DSC_NUMRPS);
  Gerador.wCampo(tcStr, '#2', 'Serie ', 01, 05, 1, NFSe.IdentificacaoRps.Serie, DSC_SERIERPS);
  Gerador.wCampo(tcStr, '#3', 'Tipo  ', 01, 01, 1, TipoRPSToStr(NFSe.IdentificacaoRps.Tipo), DSC_TIPORPS);
  Gerador.wGrupo('/IdentificacaoRps');
end;

procedure TNFSeW_EL.GerarRPSSubstituido;
begin
  if NFSe.RpsSubstituido.Numero <> '' then
  begin
    Gerador.wGrupo('RpsSubstituido');
    Gerador.wCampo(tcStr, '#10', 'Numero', 01, 15, 1, OnlyNumber(NFSe.RpsSubstituido.Numero), DSC_NUMRPSSUB);
    Gerador.wCampo(tcStr, '#11', 'Serie ', 01, 05, 1, NFSe.RpsSubstituido.Serie, DSC_SERIERPSSUB);
    Gerador.wCampo(tcStr, '#12', 'Tipo  ', 01, 01, 1, TipoRPSToStr(NFSe.RpsSubstituido.Tipo), DSC_TIPORPSSUB);
    Gerador.wGrupo('/RpsSubstituido');
  end;
end;

procedure TNFSeW_EL.GerarPrestador;
var
  xMun: String;
begin
  Gerador.wGrupo('DadosPrestador');

  Gerador.wGrupo('IdentificacaoPrestador');
  Gerador.wCampo(tcStr, '#10', 'CpfCnpj'           , 11, 014, 1, OnlyNumber(NFSe.Prestador.Cnpj), '');
  Gerador.wCampo(tcStr, '#11', 'IndicacaoCpfCnpj'  , 01, 001, 1, '2', '');
  Gerador.wCampo(tcStr, '#12', 'InscricaoMunicipal', 01, 015, 0, NFSe.Prestador.InscricaoMunicipal, '');
  Gerador.wGrupo('/IdentificacaoPrestador');

  Gerador.wCampo(tcStr, '#13', 'RazaoSocial'             , 01, 115, 0, NFSe.PrestadorServico.RazaoSocial, '');
  Gerador.wCampo(tcStr, '#14', 'NomeFantasia'             , 01, 115, 0, NFSe.PrestadorServico.NomeFantasia, '');
  Gerador.wCampo(tcStr, '#15', 'IncentivadorCultural  '  , 01, 001, 1, SimNaoToStr(NFSe.IncentivadorCultural), '');
  Gerador.wCampo(tcStr, '#16', 'OptanteSimplesNacional'  , 01, 001, 1, SimNaoToStr(NFSe.OptanteSimplesNacional), '');
  Gerador.wCampo(tcStr, '#17', 'NaturezaOperacao'        , 01, 001, 1, NaturezaOperacaoToStr(NFSe.NaturezaOperacao), '');
  Gerador.wCampo(tcStr, '#18', 'RegimeEspecialTributacao', 01, 001, 0, RegimeEspecialTributacaoToStr(NFSe.RegimeEspecialTributacao), '');

  Gerador.wGrupo('Endereco');
  Gerador.wCampo(tcStr, '#19', 'LogradouroTipo'       , 01, 125, 0, NFSe.PrestadorServico.Endereco.TipoLogradouro, '');
  Gerador.wCampo(tcStr, '#20', 'Logradouro'           , 01, 125, 0, NFSe.PrestadorServico.Endereco.Endereco, '');
  Gerador.wCampo(tcStr, '#21', 'LogradouroNumero'     , 01, 010, 0, NFSe.PrestadorServico.Endereco.Numero, '');
  Gerador.wCampo(tcStr, '#22', 'LogradouroComplemento', 01, 060, 0, NFSe.PrestadorServico.Endereco.Complemento, '');
  Gerador.wCampo(tcStr, '#23', 'Bairro'               , 01, 060, 0, NFSe.PrestadorServico.Endereco.Bairro, '');
  Gerador.wCampo(tcStr, '#24', 'CodigoMunicipio'      , 07, 007, 0, OnlyNumber(NFSe.PrestadorServico.Endereco.CodigoMunicipio), '');

  if (Trim(NFSe.PrestadorServico.Endereco.xMunicipio) = '') then
  begin
    xMun := CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0));
    xMun := Copy(xMun,1,Length(xMun)-3);
    Gerador.wCampo(tcStr, '#25', 'Municipio', 01, 100, 0, UpperCase(xMun), '');
  end
  else
    Gerador.wCampo(tcStr, '#25', 'Municipio', 01, 100, 0, NFSe.PrestadorServico.Endereco.xMunicipio, '');

  Gerador.wCampo(tcStr, '#26', 'Uf' , 02, 002, 0, NFSe.PrestadorServico.Endereco.UF, '');
  Gerador.wCampo(tcStr, '#27', 'Cep', 08, 008, 0, OnlyNumber(NFSe.PrestadorServico.Endereco.CEP), '');
  Gerador.wGrupo('/Endereco');

  Gerador.wGrupo('Contato');
  Gerador.wCampo(tcStr, '#28', 'Telefone', 01, 011, 0, OnlyNumber(NFSe.PrestadorServico.Contato.Telefone), '');
  Gerador.wCampo(tcStr, '#29', 'Email   ', 01, 080, 1, NFSe.PrestadorServico.Contato.Email, '');
  Gerador.wGrupo('/Contato');

  Gerador.wGrupo('/DadosPrestador');
end;

procedure TNFSeW_EL.GerarTomador;
var
  xMun: String;
begin
  Gerador.wGrupo('DadosTomador');
  Gerador.wGrupo('IdentificacaoTomador');
  Gerador.wCampo(tcStr, '#34', 'CpfCnpj', 11, 014, 1, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), '');

  if Length(OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj)) <= 11 then
    Gerador.wCampo(tcStr, '#35', 'IndicacaoCpfCnpj', 01, 001, 1, '1', '')
  else
    Gerador.wCampo(tcStr, '#35', 'IndicacaoCpfCnpj', 01, 001, 1, '2', '');

  Gerador.wCampo(tcStr, '#36', 'InscricaoMunicipal', 01, 015, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, '');
  Gerador.wCampo(tcStr, '#37', 'InscricaoEstadual', 01, 015, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, '');

  Gerador.wGrupo('/IdentificacaoTomador');

  Gerador.wCampo(tcStr, '#38', 'RazaoSocial', 01, 115, 0, NFSe.Tomador.RazaoSocial, '');
  Gerador.wCampo(tcStr, '#39', 'NomeFantasia', 01, 115, 0, NFSe.Tomador.RazaoSocial, '');

  Gerador.wGrupo('Endereco');
  Gerador.wCampo(tcStr, '#40', 'LogradouroTipo'       , 01, 125, 0, NFSe.Tomador.Endereco.TipoLogradouro, '');
  Gerador.wCampo(tcStr, '#41', 'Logradouro'           , 01, 125, 0, NFSe.Tomador.Endereco.Endereco, '');
  Gerador.wCampo(tcStr, '#42', 'LogradouroNumero'     , 01, 010, 0, NFSe.Tomador.Endereco.Numero, '');
  Gerador.wCampo(tcStr, '#43', 'LogradouroComplemento', 01, 060, 0, NFSe.Tomador.Endereco.Complemento, '');
  Gerador.wCampo(tcStr, '#44', 'Bairro'               , 01, 060, 0, NFSe.Tomador.Endereco.Bairro, '');
  Gerador.wCampo(tcStr, '#45', 'CodigoMunicipio'      , 07, 007, 0, OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), '');

  if (Trim(NFSe.Tomador.Endereco.xMunicipio) = '') then
  begin
    xMun := CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0));
    xMun := Copy(xMun,1,Length(xMun)-3);
    Gerador.wCampo(tcStr, '#46', 'Municipio', 01, 100, 0, UpperCase(xMun), '');
  end
  else
    Gerador.wCampo(tcStr, '#46', 'Municipio', 01, 100, 0, NFSe.Tomador.Endereco.xMunicipio, '');

  Gerador.wCampo(tcStr, '#47', 'Uf', 02, 002, 0, NFSe.Tomador.Endereco.UF, '');
  Gerador.wCampo(tcStr, '#48', 'Cep', 08, 008, 0, OnlyNumber(NFSe.Tomador.Endereco.CEP), '');
  Gerador.wGrupo('/Endereco');

  Gerador.wGrupo('Contato');
  Gerador.wCampo(tcStr, '#49', 'Telefone', 01, 011, 0, OnlyNumber(NFSe.Tomador.Contato.Telefone), '');
  Gerador.wCampo(tcStr, '#50', 'Email   ', 01, 080, 1, NFSe.Tomador.Contato.Email, '');
  Gerador.wGrupo('/Contato');

  Gerador.wGrupo('/DadosTomador');
end;

procedure TNFSeW_EL.GerarIntermediarioServico;
begin
  if (NFSe.IntermediarioServico.RazaoSocial<>'') or
     (NFSe.IntermediarioServico.CpfCnpj <> '') then
  begin
    Gerador.wGrupo('IntermediarioServico');
    Gerador.wCampo(tcStr, '#55', 'RazaoSocial', 001, 115, 0, NFSe.IntermediarioServico.RazaoSocial, '');
    Gerador.wCampo(tcStr, '#56', 'CpfCnpj'    , 14, 14, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), '');

    if Length(OnlyNumber(NFSe.IntermediarioServico.CpfCnpj)) <= 11 then
      Gerador.wCampo(tcStr, '#57', 'IndicacaoCpfCnpj', 01, 01, 1, '1', '')
    else
      Gerador.wCampo(tcStr, '#57', 'IndicacaoCpfCnpj', 01, 01, 1, '2', '');

    Gerador.wCampo(tcStr, '#58', 'InscricaoMunicipal', 01, 15, 0, NFSe.IntermediarioServico.InscricaoMunicipal, '');
    Gerador.wGrupo('/IntermediarioServico');
  end;
end;

procedure TNFSeW_EL.GerarListaServicos;
var
  i: Integer;
begin
  Gerador.wGrupo('Servicos');

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Gerador.wGrupo('Servico');
    Gerador.wCampo(tcStr, '#59', 'CodigoCnae'             , 01, 007, 0, NFSe.Servico.CodigoCnae, '');
    Gerador.wCampo(tcStr, '#60', 'CodigoServico116'       , 01, 005, 1, NFSe.Servico.ItemServico[i].CodLCServ, '');   //NFSe.Servico.ItemListaServico
    Gerador.wCampo(tcStr, '#61', 'CodigoServicoMunicipal' , 01, 020, 1, NFSe.Servico.ItemServico[i].CodServ, '');  //NFSe.Servico.CodigoTributacaoMunicipio
    Gerador.wCampo(tcDe4, '#62', 'Quantidade'             , 01, 005, 1, NFSe.Servico.ItemServico[i].Quantidade, '');
    Gerador.wCampo(tcStr, '#63', 'Unidade'                , 01, 020, 1, NFSe.Servico.ItemServico[i].Unidade, '');
    Gerador.wCampo(tcStr, '#64', 'Descricao'              , 01, 255, 1, NFSe.Servico.ItemServico[i].Discriminacao, '');
    Gerador.wCampo(tcDe4, '#65', 'Aliquota'               , 01, 005, 1, NFSe.Servico.ItemServico[i].Aliquota / 100, '');
    Gerador.wCampo(tcDe4, '#66', 'ValorServico'           , 01, 015, 1, NFSe.Servico.ItemServico[i].ValorServicos, '');
    Gerador.wCampo(tcDe4, '#67', 'ValorIssqn'             , 01, 015, 1, NFSe.Servico.ItemServico[i].ValorIss, '');
    Gerador.wCampo(tcDe2, '#68', 'ValorDesconto'          , 01, 015, 0, NFSe.Servico.ItemServico[i].ValorDeducoes, '');
    Gerador.wCampo(tcStr, '#69', 'NumeroAlvara'           , 01, 015, 0, '', '');
    Gerador.wGrupo('/Servico');
  end;

  Gerador.wGrupo('/Servicos');
end;

procedure TNFSeW_EL.GerarValoresServico;
begin
  Gerador.wGrupo('Valores');
  Gerador.wCampo(tcDe2, '#70', 'ValorServicos'       , 01, 15, 1, NFSe.Servico.Valores.ValorServicos, '');
  Gerador.wCampo(tcDe2, '#71', 'ValorDeducoes'       , 01, 15, 0, NFSe.Servico.Valores.ValorDeducoes, '');
  Gerador.wCampo(tcDe2, '#72', 'ValorPis'            , 01, 15, 0, NFSe.Servico.Valores.ValorPis, '');
  Gerador.wCampo(tcDe2, '#73', 'ValorCofins'         , 01, 15, 0, NFSe.Servico.Valores.ValorCofins, '');
  Gerador.wCampo(tcDe2, '#74', 'ValorInss'           , 01, 15, 0, NFSe.Servico.Valores.ValorInss, '');
  Gerador.wCampo(tcDe2, '#75', 'ValorIr'             , 01, 15, 0, NFSe.Servico.Valores.ValorIr, '');
  Gerador.wCampo(tcDe2, '#76', 'ValorCsll'           , 01, 15, 0, NFSe.Servico.Valores.ValorCsll, '');
  Gerador.wCampo(tcDe2, '#77', 'ValorIss'            , 01, 15, 0, NFSe.Servico.Valores.ValorIss, '');
  Gerador.wCampo(tcDe2, '#78', 'ValorOutrasRetencoes', 01, 05, 0, NFSe.Servico.Valores.OutrasRetencoes, '');
  Gerador.wCampo(tcDe2, '#79', 'ValorLiquidoNfse'    , 01, 15, 0, NFSe.Servico.Valores.ValorLiquidoNfse, '');
  Gerador.wCampo(tcDe2, '#80', 'ValorIssRetido'      , 01, 15, 0, NFSe.Servico.Valores.ValorIssRetido, '');
  Gerador.wCampo(tcDe2, '#81', 'OutrosDescontos'     , 01, 15, 0, NFSe.Servico.Valores.OutrosDescontos, '');

  Gerador.wGrupo('/Valores');
end;

procedure TNFSeW_EL.GerarXML_EL;
var
  LocPrest: String;
begin
  FIdentificador := 'Id';
  Gerador.wCampo(tcStr, '#01', FIdentificador, 1, 15, 1, NFSe.InfID.ID, '');

  LocPrest := '2';
  if NFSe.NaturezaOperacao = no2 then
    LocPrest := '1';

  // Código para identificação do local de prestação do serviço 1-Fora do município 2-No município
  Gerador.wCampo(tcStr, '#02', 'LocalPrestacao', 1, 1, 1, LocPrest, '');
  //IssRetido no provedor EL é ao contrario (1 = normal, 2 retido) por isso não da de usar SituacaoTributariaToStr
  //Gerador.wCampo(tcStr   , '#03', 'IssRetido'     , 001, 001, 1, SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido), '');

  if NFSe.Servico.Valores.IssRetido = stRetencao then
    Gerador.wCampo(tcStr, '#03', 'IssRetido', 1, 1, 1, '2', '')
  else
    Gerador.wCampo(tcStr, '#03', 'IssRetido', 1, 1, 1, '1', '');

  Gerador.wCampo(tcDatHor, '#04', 'DataEmissao', 19, 19, 1, NFSe.DataEmissao, DSC_DEMI);

  GerarIdentificacaoRPS;

  GerarPrestador;
  GerarTomador;
  GerarIntermediarioServico;
  GerarListaServicos;
  GerarValoresServico;
  GerarRPSSubstituido;

  Gerador.wCampo(tcStr, '#90', 'Observacao', 1, 255, 0, NFSe.OutrasInformacoes, '');
  Gerador.wCampo(tcStr, '#91', 'Status'    , 1, 001, 1, StatusRPSToStr(NFSe.Status), '');

  Gerador.wCampo(tcStr, '#91', 'CodigoMunicipioPrestacao', 7, 7, 0,
                OnlyNumber(NFSe.PrestadorServico.Endereco.CodigoMunicipio), '');
end;

constructor TNFSeW_EL.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);

end;

function TNFSeW_EL.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

function TNFSeW_EL.GerarXml: Boolean;
begin
  Gerador.ListaDeAlertas.Clear;

  Gerador.ArquivoFormatoXML := '';
  Gerador.Prefixo           := FPrefixo4;

  Gerador.Opcoes.QuebraLinha := FQuebradeLinha;

  if (RightStr(FURL, 1) <> '/') and (FDefTipos <> '')
    then FDefTipos := '/' + FDefTipos;

  if Trim(FPrefixo4) <> ''
    then Atributo := ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FURL + FDefTipos + '"'
    else Atributo := ' xmlns="' + FURL + FDefTipos + '"';

  Gerador.wGrupo('Rps');

  FNFSe.InfID.ID := StringOfChar('0', 15) +
                    OnlyNumber(FNFSe.IdentificacaoRps.Numero) +
                    FNFSe.IdentificacaoRps.Serie;
  FNFSe.InfID.ID := copy(FNFSe.InfID.ID, length(FNFSe.InfID.ID) - 15 + 1, 15);

  GerarXML_EL;

  Gerador.wGrupo('/Rps');

  Gerador.gtAjustarRegistros(NFSe.InfID.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.