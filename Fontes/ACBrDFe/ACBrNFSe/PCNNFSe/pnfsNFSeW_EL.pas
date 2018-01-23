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

unit pnfsNFSeW_EL;

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
  { TNFSeW_EL }

  TNFSeW_EL = class(TNFSeWClass)
  private
  protected

    procedure GerarIdentificacaoRPS;
    procedure GerarRPSSubstituido;

    procedure GerarPrestador;
    procedure GerarTomador;
    procedure GerarIntermediarioServico;

    procedure GerarServicoValores;
    procedure GerarListaServicos;
    procedure GerarValoresServico;

    procedure GerarConstrucaoCivil;
    procedure GerarCondicaoPagamento;

    procedure GerarXML_EL;

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
{ layout do EL.                                                                }
{ Sendo assim só será criado uma nova unit para um novo layout.                }
{==============================================================================}

{ TNFSeW_EL }

procedure TNFSeW_EL.GerarIdentificacaoRPS;
begin
  Gerador.wGrupoNFSe('IdentificacaoRps');
  Gerador.wCampoNFSe(tcStr, '#1', 'Numero', 01, 15, 1, OnlyNumber(NFSe.IdentificacaoRps.Numero), DSC_NUMRPS);
  Gerador.wCampoNFSe(tcStr, '#2', 'Serie ', 01, 05, 1, NFSe.IdentificacaoRps.Serie, DSC_SERIERPS);
  Gerador.wCampoNFSe(tcStr, '#3', 'Tipo  ', 01, 01, 1, TipoRPSToStr(NFSe.IdentificacaoRps.Tipo), DSC_TIPORPS);
  Gerador.wGrupoNFSe('/IdentificacaoRps');
end;

procedure TNFSeW_EL.GerarRPSSubstituido;
begin
  if NFSe.RpsSubstituido.Numero <> '' then
  begin
    Gerador.wGrupoNFSe('RpsSubstituido');
    Gerador.wCampoNFSe(tcStr, '#10', 'Numero', 01, 15, 1, OnlyNumber(NFSe.RpsSubstituido.Numero), DSC_NUMRPSSUB);
    Gerador.wCampoNFSe(tcStr, '#11', 'Serie ', 01, 05, 1, NFSe.RpsSubstituido.Serie, DSC_SERIERPSSUB);
    Gerador.wCampoNFSe(tcStr, '#12', 'Tipo  ', 01, 01, 1, TipoRPSToStr(NFSe.RpsSubstituido.Tipo), DSC_TIPORPSSUB);
    Gerador.wGrupoNFSe('/RpsSubstituido');
  end;
end;

procedure TNFSeW_EL.GerarPrestador;
var
  xMun: String;
begin
  Gerador.wGrupoNFSe('DadosPrestador');

  Gerador.wGrupoNFSe('IdentificacaoPrestador');
  Gerador.wCampoNFSe(tcStr, '#10', 'CpfCnpj'           , 11, 014, 1, OnlyNumber(NFSe.Prestador.Cnpj), '');
  Gerador.wCampoNFSe(tcStr, '#11', 'IndicacaoCpfCnpj'  , 01, 001, 1, '2', '');
  Gerador.wCampoNFSe(tcStr, '#12', 'InscricaoMunicipal', 01, 015, 0, NFSe.Prestador.InscricaoMunicipal, '');
  Gerador.wGrupoNFSe('/IdentificacaoPrestador');

  Gerador.wCampoNFSe(tcStr, '#13', 'RazaoSocial'             , 01, 115, 0, NFSe.PrestadorServico.RazaoSocial, '');
  Gerador.wCampoNFSe(tcStr, '#14', 'NomeFantasia'             , 01, 115, 0, NFSe.PrestadorServico.NomeFantasia, '');
  Gerador.wCampoNFSe(tcStr, '#15', 'IncentivadorCultural  '  , 01, 001, 1, SimNaoToStr(NFSe.IncentivadorCultural), '');
  Gerador.wCampoNFSe(tcStr, '#16', 'OptanteSimplesNacional'  , 01, 001, 1, SimNaoToStr(NFSe.OptanteSimplesNacional), '');
  Gerador.wCampoNFSe(tcStr, '#17', 'NaturezaOperacao'        , 01, 001, 1, NaturezaOperacaoToStr(NFSe.NaturezaOperacao), '');
  Gerador.wCampoNFSe(tcStr, '#18', 'RegimeEspecialTributacao', 01, 001, 0, RegimeEspecialTributacaoToStr(NFSe.RegimeEspecialTributacao), '');

  Gerador.wGrupoNFSe('Endereco');
  Gerador.wCampoNFSe(tcStr, '#19', 'LogradouroTipo'       , 01, 125, 0, NFSe.PrestadorServico.Endereco.TipoLogradouro, '');
  Gerador.wCampoNFSe(tcStr, '#20', 'Logradouro'           , 01, 125, 0, NFSe.PrestadorServico.Endereco.Endereco, '');
  Gerador.wCampoNFSe(tcStr, '#21', 'LogradouroNumero'     , 01, 010, 0, NFSe.PrestadorServico.Endereco.Numero, '');
  Gerador.wCampoNFSe(tcStr, '#22', 'LogradouroComplemento', 01, 060, 0, NFSe.PrestadorServico.Endereco.Complemento, '');
  Gerador.wCampoNFSe(tcStr, '#23', 'Bairro'               , 01, 060, 0, NFSe.PrestadorServico.Endereco.Bairro, '');
  Gerador.wCampoNFSe(tcStr, '#24', 'CodigoMunicipio'      , 07, 007, 0, OnlyNumber(NFSe.PrestadorServico.Endereco.CodigoMunicipio), '');

  if (Trim(NFSe.PrestadorServico.Endereco.xMunicipio) = '') then
  begin
    xMun := CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0));
    xMun := Copy(xMun,1,Length(xMun)-3);
    Gerador.wCampoNFSe(tcStr, '#25', 'Municipio', 01, 100, 0, UpperCase(xMun), '');
  end
  else
    Gerador.wCampoNFSe(tcStr, '#25', 'Municipio', 01, 100, 0, NFSe.PrestadorServico.Endereco.xMunicipio, '');

  Gerador.wCampoNFSe(tcStr, '#26', 'Uf' , 02, 002, 0, NFSe.PrestadorServico.Endereco.UF, '');
  Gerador.wCampoNFSe(tcStr, '#27', 'Cep', 08, 008, 0, OnlyNumber(NFSe.PrestadorServico.Endereco.CEP), '');
  Gerador.wGrupoNFSe('/Endereco');

  Gerador.wGrupoNFSe('Contato');
  Gerador.wCampoNFSe(tcStr, '#28', 'Telefone', 01, 011, 0, OnlyNumber(NFSe.PrestadorServico.Contato.Telefone), '');
  Gerador.wCampoNFSe(tcStr, '#29', 'Email   ', 01, 080, 1, NFSe.PrestadorServico.Contato.Email, '');
  Gerador.wGrupoNFSe('/Contato');

  Gerador.wGrupoNFSe('/DadosPrestador');
end;

procedure TNFSeW_EL.GerarTomador;
var
  xMun: String;
begin
  Gerador.wGrupoNFSe('DadosTomador');
  Gerador.wGrupoNFSe('IdentificacaoTomador');
  Gerador.wCampoNFSe(tcStr, '#34', 'CpfCnpj', 11, 014, 1, SomenteNumeros(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), '');

  if Length(SomenteNumeros(NFSe.Tomador.IdentificacaoTomador.CpfCnpj)) <= 11 then
    Gerador.wCampoNFSe(tcStr, '#35', 'IndicacaoCpfCnpj', 01, 001, 1, '1', '')
  else
    Gerador.wCampoNFSe(tcStr, '#35', 'IndicacaoCpfCnpj', 01, 001, 1, '2', '');

  // Gumercino 19/01/2018
  Gerador.wCampoNFSe(tcStr, '#36', 'InscricaoMunicipal', 01, 015, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, '');
  Gerador.wCampoNFSe(tcStr, '#37', 'InscricaoEstadual', 01, 015, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, '');
  Gerador.wGrupoNFSe('/IdentificacaoTomador');
  Gerador.wCampoNFSe(tcStr, '#38', 'RazaoSocial', 01, 115, 0, NFSe.Tomador.RazaoSocial, '');
  Gerador.wCampoNFSe(tcStr, '#39', 'NomeFantasia', 01, 115, 0, NFSe.Tomador.RazaoSocial, '');

  Gerador.wGrupoNFSe('Endereco');
  Gerador.wCampoNFSe(tcStr, '#40', 'LogradouroTipo'       , 01, 125, 0, NFSe.Tomador.Endereco.TipoLogradouro, '');
  Gerador.wCampoNFSe(tcStr, '#41', 'Logradouro'           , 01, 125, 0, NFSe.Tomador.Endereco.Endereco, '');
  Gerador.wCampoNFSe(tcStr, '#42', 'LogradouroNumero'     , 01, 010, 0, NFSe.Tomador.Endereco.Numero, '');
  Gerador.wCampoNFSe(tcStr, '#43', 'LogradouroComplemento', 01, 060, 0, NFSe.Tomador.Endereco.Complemento, '');
  Gerador.wCampoNFSe(tcStr, '#44', 'Bairro'               , 01, 060, 0, NFSe.Tomador.Endereco.Bairro, '');
  Gerador.wCampoNFSe(tcStr, '#45', 'CodigoMunicipio'      , 07, 007, 0, SomenteNumeros(NFSe.Tomador.Endereco.CodigoMunicipio), '');

  if (Trim(NFSe.Tomador.Endereco.xMunicipio) = '') then
  begin
    xMun := CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0));
    xMun := Copy(xMun,1,Length(xMun)-3);
    Gerador.wCampoNFSe(tcStr, '#46', 'Municipio', 01, 100, 0, UpperCase(xMun), '');
  end
  else
    Gerador.wCampoNFSe(tcStr, '#46', 'Municipio', 01, 100, 0, NFSe.Tomador.Endereco.xMunicipio, '');

  Gerador.wCampoNFSe(tcStr, '#47', 'Uf', 02, 002, 0, NFSe.Tomador.Endereco.UF, '');
  Gerador.wCampoNFSe(tcStr, '#48', 'Cep', 08, 008, 0, SomenteNumeros(NFSe.Tomador.Endereco.CEP), '');
  Gerador.wGrupoNFSe('/Endereco');

  Gerador.wGrupoNFSe('Contato');
  Gerador.wCampoNFSe(tcStr, '#49', 'Telefone', 01, 011, 0, SomenteNumeros(NFSe.Tomador.Contato.Telefone), '');
  Gerador.wCampoNFSe(tcStr, '#50', 'Email   ', 01, 080, 1, NFSe.Tomador.Contato.Email, '');
  Gerador.wGrupoNFSe('/Contato');

  Gerador.wGrupoNFSe('/DadosTomador');
end;

procedure TNFSeW_EL.GerarIntermediarioServico;
begin
  if (NFSe.IntermediarioServico.RazaoSocial<>'') or
     (NFSe.IntermediarioServico.CpfCnpj <> '') then
  begin
    Gerador.wGrupoNFSe('IntermediarioServico');
    Gerador.wCampoNFSe(tcStr, '#55', 'RazaoSocial', 001, 115, 0, NFSe.IntermediarioServico.RazaoSocial, '');
    Gerador.wCampoNFSe(tcStr, '#56', 'CpfCnpj'    , 14, 14, 1, SomenteNumeros(NFSe.IntermediarioServico.CpfCnpj), '');

    if Length(SomenteNumeros(NFSe.IntermediarioServico.CpfCnpj)) <= 11 then
      Gerador.wCampoNFSe(tcStr, '#57', 'IndicacaoCpfCnpj', 01, 01, 1, '1', '')
    else
      Gerador.wCampoNFSe(tcStr, '#57', 'IndicacaoCpfCnpj', 01, 01, 1, '2', '');

    Gerador.wCampoNFSe(tcStr, '#58', 'InscricaoMunicipal', 01, 15, 0, NFSe.IntermediarioServico.InscricaoMunicipal, '');
    Gerador.wGrupoNFSe('/IntermediarioServico');
  end;
end;

procedure TNFSeW_EL.GerarServicoValores;
begin
  // Não Definido
end;

procedure TNFSeW_EL.GerarListaServicos;
var
  i: Integer;
begin
  Gerador.wGrupoNFSe('Servicos');

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Gerador.wGrupoNFSe('Servico');
    Gerador.wCampoNFSe(tcStr, '#59', 'CodigoCnae'             , 01, 007, 0, NFSe.Servico.CodigoCnae, '');
    Gerador.wCampoNFSe(tcStr, '#60', 'CodigoServico116'       , 01, 005, 1, NFSe.Servico.ItemListaServico, '');
    Gerador.wCampoNFSe(tcStr, '#61', 'CodigoServicoMunicipal' , 01, 020, 1, NFSe.Servico.CodigoTributacaoMunicipio, '');
    Gerador.wCampoNFSe(tcInt, '#62', 'Quantidade'             , 01, 005, 1, NFSe.Servico.ItemServico[i].Quantidade, '');
    Gerador.wCampoNFSe(tcStr, '#63', 'Unidade'                , 01, 020, 1, 'UN', '');
    Gerador.wCampoNFSe(tcStr, '#64', 'Descricao'              , 01, 255, 1, NFSe.Servico.ItemServico[i].Discriminacao, '');
    Gerador.wCampoNFSe(tcDe2, '#65', 'Aliquota'               , 01, 005, 1, NFSe.Servico.ItemServico[i].Aliquota/100, '');
    Gerador.wCampoNFSe(tcDe4, '#66', 'ValorServico'           , 01, 015, 1, NFSe.Servico.ItemServico[i].ValorServicos, '');
    Gerador.wCampoNFSe(tcDe4, '#67', 'ValorIssqn'             , 01, 015, 1, NFSe.Servico.ItemServico[i].ValorIss, '');
    Gerador.wCampoNFSe(tcDe2, '#68', 'ValorDesconto'          , 01, 015, 0, NFSe.Servico.ItemServico[i].ValorDeducoes, '');
    Gerador.wCampoNFSe(tcStr, '#69', 'NumeroAlvara'           , 01, 015, 0, '', '');
    Gerador.wGrupoNFSe('/Servico');
  end;

  Gerador.wGrupoNFSe('/Servicos');
end;

procedure TNFSeW_EL.GerarValoresServico;
begin
  Gerador.wGrupoNFSe('Valores');
  Gerador.wCampoNFSe(tcDe2, '#70', 'ValorServicos'       , 01, 15, 1, NFSe.Servico.Valores.ValorServicos, '');
  Gerador.wCampoNFSe(tcDe2, '#71', 'ValorDeducoes'       , 01, 15, 0, NFSe.Servico.Valores.ValorDeducoes, '');
  Gerador.wCampoNFSe(tcDe2, '#72', 'ValorPis'            , 01, 15, 0, NFSe.Servico.Valores.ValorPis, '');
  Gerador.wCampoNFSe(tcDe2, '#73', 'ValorCofins'         , 01, 15, 0, NFSe.Servico.Valores.ValorCofins, '');
  Gerador.wCampoNFSe(tcDe2, '#74', 'ValorInss'           , 01, 15, 0, NFSe.Servico.Valores.ValorInss, '');
  Gerador.wCampoNFSe(tcDe2, '#75', 'ValorIr'             , 01, 15, 0, NFSe.Servico.Valores.ValorIr, '');
  Gerador.wCampoNFSe(tcDe2, '#76', 'ValorCsll'           , 01, 15, 0, NFSe.Servico.Valores.ValorCsll, '');
  Gerador.wCampoNFSe(tcDe2, '#77', 'ValorIss'            , 01, 15, 0, NFSe.Servico.Valores.ValorIss, '');
  Gerador.wCampoNFSe(tcDe2, '#78', 'ValorOutrasRetencoes', 01, 05, 0, NFSe.Servico.Valores.OutrasRetencoes, '');
  Gerador.wCampoNFSe(tcDe2, '#79', 'ValorLiquidoNfse'    , 01, 15, 0, NFSe.Servico.Valores.ValorLiquidoNfse, '');
  Gerador.wCampoNFSe(tcDe2, '#80', 'ValorIssRetido'      , 01, 15, 0, NFSe.Servico.Valores.ValorIssRetido, '');

  // Gumercino 16/01/2018 - Provedor EL
  Gerador.wCampoNFSe(tcDe2, '#81', 'OutrosDescontos'     , 01, 15, 0, NFSe.Servico.Valores.OutrosDescontos, '');

  Gerador.wGrupoNFSe('/Valores');
end;

procedure TNFSeW_EL.GerarConstrucaoCivil;
begin
  // Não Definido
end;

procedure TNFSeW_EL.GerarCondicaoPagamento;
begin
  // Não Definido
end;

procedure TNFSeW_EL.GerarXML_EL;
var
  LocPrest: String;
begin
  FIdentificador := 'Id';
  Gerador.wCampoNFSe(tcStr, '#01', FIdentificador, 001, 015, 1, NFSe.InfID.ID, '');

  LocPrest := '2';
  if NFSe.NaturezaOperacao = no2 then
    LocPrest := '1';

  // Código para identificação do local de prestação do serviço 1-Fora do município 2-No município
  Gerador.wCampoNFSe(tcStr   , '#02', 'LocalPrestacao', 001, 001, 1, LocPrest, '');
  //IssRetido no provedor EL é ao contrario (1 = normal, 2 retido) por isso não da de usar SituacaoTributariaToStr
  //Gerador.wCampoNFSe(tcStr   , '#03', 'IssRetido'     , 001, 001, 1, SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido), '');
  if NFSe.Servico.Valores.IssRetido = stRetencao then
    Gerador.wCampoNFSe(tcStr   , '#03', 'IssRetido'   , 001, 001, 1, '2', '')
  else
    Gerador.wCampoNFSe(tcStr   , '#03', 'IssRetido'   , 001, 001, 1, '1', '');
  Gerador.wCampoNFSe(tcDatHor, '#04', 'DataEmissao'   , 019, 019, 1, NFSe.DataEmissao, DSC_DEMI);

  GerarIdentificacaoRPS;

  GerarPrestador;
  GerarTomador;
  GerarIntermediarioServico;
  GerarListaServicos;
  GerarValoresServico;
  GerarRPSSubstituido;

  Gerador.wCampoNFSe(tcStr, '#90', 'Observacao', 001, 255, 0, NFSe.OutrasInformacoes, '');
  Gerador.wCampoNFSe(tcStr, '#91', 'Status'    , 001, 001, 1, StatusRPSToStr(NFSe.Status), '');
end;

////////////////////////////////////////////////////////////////////////////////

constructor TNFSeW_EL.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);

end;

function TNFSeW_EL.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

function TNFSeW_EL.GerarXml: Boolean;
var
  Gerar: Boolean;
begin
  Gerador.ListaDeAlertas.Clear;
  Gerador.ArquivoFormatoXML := '';
  Gerador.Prefixo           := FPrefixo4;

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

end.
