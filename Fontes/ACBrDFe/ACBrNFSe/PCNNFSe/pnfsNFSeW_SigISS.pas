{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Adriano Luiz Alves                              }
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

unit pnfsNFSeW_SigISS;

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
  pnfsNFSe, pnfsConversao, pnfsConsts;

type
  { TNFSeW_SigISS }

  TNFSeW_SigISS = class(TNFSeWClass)
  private
  protected

    procedure GerarPrestador;
    procedure GerarIdentificacaoRPS;




    procedure GerarRPSSubstituido;
    procedure GerarTomador;
    procedure GerarIntermediarioServico;
    procedure GerarServicoValores;
    procedure GerarListaServicos;
    procedure GerarValoresServico;
    procedure GerarConstrucaoCivil;
    procedure GerarCondicaoPagamento;

    procedure GerarXML_SigISS;

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
{ layout do SigISS.                                                                }
{ Sendo assim só será criado uma nova unit para um novo layout.                }
{==============================================================================}

{ TNFSeW_SigISS }

procedure TNFSeW_SigISS.GerarIdentificacaoRPS;
begin
  Gerador.wGrupo('DescricaoRps');
  Gerador.wCampo(tcStr, '#1', 'id_sis_legado', 01, 015, 0, NFSe.id_sis_legado, '');
  Gerador.wCampo(tcStr, '#2', 'servico'      , 01, 015, 1, OnlyNumber(NFSe.Servico.CodigoTributacaoMunicipio), '');
  //Situação pode ser
  //tp – Tributada no prestador;
  //tt – Tributada no tomador;
  //is – Isenta;
  //im – Imune;
  //nt – Não tributada.
  Gerador.wCampo(tcStr, '#2', 'situacao'     , 01, 002, 1, NFSe.Situacao, '');
  Gerador.wCampo(tcDe4, '#3', 'valor'        , 01, 015, 1, NFSe.Servico.Valores.ValorServicos, '');
  Gerador.wCampo(tcDe4, '#4', 'base'         , 01, 015, 1, NFSe.Servico.Valores.BaseCalculo, '');
  Gerador.wCampo(tcStr, '#5', 'descricaoNF'  , 01, 150, 0, NFSe.Servico.Discriminacao, '');
  //Tipo do tomador que se quer escriturar:
  //1 – PFNI - Pessoa Física não identificada;
  //2 – Pessoa Física;
  //3 – Jurídica do Munici´pio;
  //4 – Jurídica de Fora; 5 – Juri´dica de Fora do País.
  if NFSe.Tomador.IdentificacaoTomador.Tipo<>'' then
     Gerador.wCampo(tcStr, '#6', 'tomador_tipo'        , 01, 001, 1, NFSe.Tomador.IdentificacaoTomador.Tipo, '')
  else if NFSe.Tomador.IdentificacaoTomador.CpfCnpj='' then
     Gerador.wCampo(tcStr, '#6', 'tomador_tipo'        , 01, 001, 1, '1', '')
  else if Length(NFSe.Tomador.IdentificacaoTomador.CpfCnpj)=11 then
     Gerador.wCampo(tcStr, '#6', 'tomador_tipo'        , 01, 001, 1, '2', '')
  else if (Length(NFSe.Tomador.IdentificacaoTomador.CpfCnpj)=14) and
          ( OnlyNumber(NFSe.Prestador.Endereco.CodigoMunicipio) = OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio)) then
     Gerador.wCampo(tcStr, '#6', 'tomador_tipo'        , 01, 001, 1, '3', '')
  else if (Length(NFSe.Tomador.IdentificacaoTomador.CpfCnpj)=14) and
          ( OnlyNumber(NFSe.Prestador.Endereco.CodigoMunicipio) <> OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio)) then
     Gerador.wCampo(tcStr, '#6', 'tomador_tipo'        , 01, 001, 1, '4', '')
  else
     Gerador.wCampo(tcStr, '#6', 'tomador_tipo'        , 01, 001, 1, '', '');
  Gerador.wCampo(tcStr, '#7', 'tomador_cnpj'        , 01, 015, 1, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), '');
  Gerador.wCampo(tcStr, '#8', 'tomador_email'       , 01, 100, 0, NFSe.Tomador.Contato.Email, '');
  Gerador.wCampo(tcStr, '#9', 'tomador_ie'          , 01, 015, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, '');
  Gerador.wCampo(tcStr, '#10','tomador_im'          , 01, 015, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, '');
  Gerador.wCampo(tcStr, '#11','tomador_razao'       , 01, 100, 1, NFSe.Tomador.RazaoSocial, '');
  //Gerador.wCampo(tcStr, '#11','tomador_fanasia'   , 01, 100, 1, NFSe.Tomador.Contato.InscricaoMunicipal, '');
  Gerador.wCampo(tcStr, '#12','tomador_endereco'    , 01, 100, 1, NFSe.Tomador.Endereco.Endereco, '');
  Gerador.wCampo(tcStr, '#13','tomador_numero'      , 01, 010, 0, NFSe.Tomador.Endereco.Numero, '');
  Gerador.wCampo(tcStr, '#13','tomador_complemento' , 01, 050, 0, NFSe.Tomador.Endereco.Complemento, '');
  Gerador.wCampo(tcStr, '#14','tomador_bairro'      , 01, 100, 1, NFSe.Tomador.Endereco.Bairro, '');
  Gerador.wCampo(tcStr, '#15','tomador_CEP'         , 01, 008, 1, OnlyNumber(NFSe.Tomador.Endereco.CEP), '');
  Gerador.wCampo(tcStr, '#16','tomador_cod_cidade'  , 01, 007, 1, OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), '');
  Gerador.wCampo(tcStr, '#17','tomador_fone'        , 01, 015, 0, NFSe.Tomador.Contato.Telefone, '');
  //Gerador.wCampo(tcStr, '#18','tomador_ramal'     , 01, 100, 0, NFSe.Tomador.Contato.Bairro, '');
  //Gerador.wCampo(tcStr, '#18','tomador_fax'       , 01, 100, 0, NFSe.Tomador.Contato.Bairro, '');
  Gerador.wCampo(tcStr, '#18','rps_num'             , 01, 015, 0, OnlyNumber(NFSe.IdentificacaoRps.Numero), '');
  Gerador.wCampo(tcStr, '#19','rps_serie'           , 01, 003, 0, NFSe.IdentificacaoRps.Serie, '');
  Gerador.wCampo(tcStr, '#20','rps_dia'             , 01, 002, 0, FormatDateTime('dd',NFSe.DataEmissaoRps), '');
  Gerador.wCampo(tcStr, '#21','rps_mes'             , 01, 002, 0, FormatDateTime('MM',NFSe.DataEmissaoRps), '');
  Gerador.wCampo(tcStr, '#22','rps_ano'             , 01, 004, 0, FormatDateTime('yyyy',NFSe.DataEmissaoRps), '');
  Gerador.wCampo(tcDe4, '#23','valor_inss'          , 01, 015, 0, NFSe.Servico.Valores.ValorInss, '');
  Gerador.wCampo(tcDe4, '#24','valor_ir'            , 01, 015, 0, NFSe.Servico.Valores.ValorIr, '');
  Gerador.wCampo(tcDe4, '#25','valor_pis'           , 01, 015, 0, NFSe.Servico.Valores.ValorPis, '');
  Gerador.wCampo(tcDe4, '#26','valor_cofins'        , 01, 015, 0, NFSe.Servico.Valores.ValorCofins, '');
  Gerador.wCampo(tcDe4, '#27','valor_csll'          , 01, 015, 0, NFSe.Servico.Valores.ValorCsll, '');
  //Gerador.wCampo(tcDe4, '#22','valor_iss_fora'      , 01, 015, 0, NFSe.Servico.Valores.ValorRepasse, '');
  //Gerador.wCampo(tcStr, '#22','codig_cidade_local_servico', 01, 007, 0, NFSe.Servico.ItemServico[0].ValorISSST, '');
  Gerador.wGrupo('/DescricaoRps');
end;

procedure TNFSeW_SigISS.GerarRPSSubstituido;
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

procedure TNFSeW_SigISS.GerarPrestador;
var
  xMun: String;
begin
  Gerador.wGrupo('DadosPrestador');
  Gerador.wCampo(tcStr, '#01', 'ccm', 01, 015, 0, NFSe.Prestador.Usuario, '');
  Gerador.wCampo(tcStr, '#02', 'cnpj'           , 11, 014, 1, OnlyNumber(NFSe.Prestador.Cnpj), '');
  Gerador.wCampo(tcStr, '#03', 'senha'  , 01, 010, 1, NFSe.Prestador.Senha, '');
  Gerador.wCampo(tcStr, '#04', 'crc'  , 01, 010, 0, NFSE.Prestador.crc, '');
  Gerador.wCampo(tcStr, '#05', 'crc_estado'  , 01, 002, 0, NFSE.Prestador.crc_estado, '');
  if NFSE.Servico.Valores.AliquotaSN>0 then
     Gerador.wCampo(tcDe2, '#06', 'aliquota_simples'  , 01, 015, 0, NFSE.Servico.Valores.AliquotaSN, '');
  Gerador.wGrupo('/DadosPrestador');
end;

procedure TNFSeW_SigISS.GerarTomador;
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

procedure TNFSeW_SigISS.GerarIntermediarioServico;
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

procedure TNFSeW_SigISS.GerarServicoValores;
begin
  // Não Definido
end;

procedure TNFSeW_SigISS.GerarListaServicos;
var
  i: Integer;
begin
  Gerador.wGrupo('Servicos');

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Gerador.wGrupo('Servico');
    Gerador.wCampo(tcStr, '#59', 'CodigoCnae'             , 01, 007, 0, NFSe.Servico.CodigoCnae, '');
    Gerador.wCampo(tcStr, '#60', 'CodigoServico116'       , 01, 005, 1, NFSe.Servico.ItemListaServico, '');
    Gerador.wCampo(tcStr, '#61', 'CodigoServicoMunicipal' , 01, 020, 1, NFSe.Servico.CodigoTributacaoMunicipio, '');
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

procedure TNFSeW_SigISS.GerarValoresServico;
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

procedure TNFSeW_SigISS.GerarConstrucaoCivil;
begin
  // Não Definido
end;

procedure TNFSeW_SigISS.GerarCondicaoPagamento;
begin
  // Não Definido
end;

procedure TNFSeW_SigISS.GerarXML_SigISS;
begin
  //Este provedor não segue o padrao dos outros provedores
  //Existe apenas dois grupos, um com dados básicos do prestador e outro com os dados da RPS
  //<Dados Prestador>
  GerarPrestador;
  //</DadosPrestador>

  //<DecricaoRPS>
  GerarIdentificacaoRPS;
  //</DecricaoRPS>

  {
  GerarTomador;
  GerarIntermediarioServico;
  GerarValoresServico;
  GerarRPSSubstituido;

  Gerador.wCampo(tcStr, '#90', 'Observacao', 001, 255, 0, NFSe.OutrasInformacoes, '');
  Gerador.wCampo(tcStr, '#91', 'Status'    , 001, 001, 1, StatusRPSToStr(NFSe.Status), '');
  Gerador.wGrupo('/DescricaoRPS');
  }
end;

constructor TNFSeW_SigISS.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);

end;

function TNFSeW_SigISS.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

function TNFSeW_SigISS.GerarXml: Boolean;
begin
  Gerador.ListaDeAlertas.Clear;

  Gerador.ArquivoFormatoXML := '';
  Gerador.Prefixo           := FPrefixo4;

  Gerador.Opcoes.QuebraLinha := FQuebradeLinha;
  Gerador.Opcoes.DecimalChar := ',';

  if (RightStr(FURL, 1) <> '/') and (FDefTipos <> '')
    then FDefTipos := '/' + FDefTipos;

  if Trim(FPrefixo4) <> ''
    then Atributo := ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FURL + FDefTipos + '"'
    else Atributo := ' xmlns="' + FURL + FDefTipos + '"';

  Gerador.wGrupo('GerarNota');

  FNFSe.InfID.ID := StringOfChar('0', 15) +
                    OnlyNumber(FNFSe.IdentificacaoRps.Numero) +
                    FNFSe.IdentificacaoRps.Serie;
  FNFSe.InfID.ID := copy(FNFSe.InfID.ID, length(FNFSe.InfID.ID) - 15 + 1, 15);

  GerarXML_SigISS;

  Gerador.wGrupo('/GerarNota');

  Gerador.gtAjustarRegistros(NFSe.InfID.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.