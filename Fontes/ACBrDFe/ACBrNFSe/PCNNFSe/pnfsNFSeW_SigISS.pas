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
    procedure GerarPrestador(GerarGrupo: Boolean = True);
    procedure GerarIdentificacaoRPS;

    procedure GerarXML_SigISS;
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
{ layout do SigISS.                                                            }
{ Sendo assim só será criado uma nova unit para um novo layout.                }
{==============================================================================}

{ TNFSeW_SigISS }

procedure TNFSeW_SigISS.GerarIdentificacaoRPS;
begin
  Gerador.wGrupo('DescricaoRps');

  // Para Governador Valadares, os dados do prestador são incluídos diretamente
  // dentro do grupo DescricaoRps
  if NFSe.Prestador.Endereco.CodigoMunicipio = '3127701' then
    GerarPrestador(False);

  // Id do sistema legado não pode se repetir de forma alguma em Governador Valadares.
  // Caso já tenha sido utilizado em outro sitema a nota não será emitida.
  // Então é melhor remover esse campo.
  if NFSe.id_sis_legado <> 0 then
    Gerador.wCampo(tcStr, '#1', 'id_sis_legado', 01, 015, 0, NFSe.id_sis_legado, '');

  Gerador.wCampo(tcStr, '#2', 'servico', 01, 015, 1, OnlyNumber(NFSe.Servico.CodigoTributacaoMunicipio), '');
  //Situação pode ser
  //tp – Tributada no prestador;
  //tt – Tributada no tomador;
  //is – Isenta;
  //im – Imune;
  //nt – Não tributada.
  Gerador.wCampo(tcStr, '#2', 'situacao'   , 01, 002, 1, NFSe.Situacao, '');
  Gerador.wCampo(tcDe4, '#3', 'valor'      , 01, 015, 1, NFSe.Servico.Valores.ValorServicos, '');
  Gerador.wCampo(tcDe4, '#4', 'base'       , 01, 015, 1, NFSe.Servico.Valores.BaseCalculo, '');
  Gerador.wCampo(tcStr, '#5', 'descricaoNF', 01, 150, 0, NFSe.Servico.Discriminacao, '');
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

  Gerador.wCampo(tcStr, '#7', 'tomador_cnpj'       , 01, 015, 1, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), '');
  Gerador.wCampo(tcStr, '#8', 'tomador_email'      , 01, 100, 0, NFSe.Tomador.Contato.Email, '');
  Gerador.wCampo(tcStr, '#9', 'tomador_ie'         , 01, 015, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, '');
  Gerador.wCampo(tcStr, '#10','tomador_im'         , 01, 015, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, '');
  Gerador.wCampo(tcStr, '#11','tomador_razao'      , 01, 100, 1, NFSe.Tomador.RazaoSocial, '');
  //Gerador.wCampo(tcStr, '#11','tomador_fanasia'  , 01, 100, 1, NFSe.Tomador.Contato.InscricaoMunicipal, '');
  Gerador.wCampo(tcStr, '#12','tomador_endereco'   , 01, 100, 1, NFSe.Tomador.Endereco.Endereco, '');
  Gerador.wCampo(tcStr, '#13','tomador_numero'     , 01, 010, 0, NFSe.Tomador.Endereco.Numero, '');
  Gerador.wCampo(tcStr, '#13','tomador_complemento', 01, 050, 0, NFSe.Tomador.Endereco.Complemento, '');
  Gerador.wCampo(tcStr, '#14','tomador_bairro'     , 01, 100, 1, NFSe.Tomador.Endereco.Bairro, '');
  Gerador.wCampo(tcStr, '#15','tomador_CEP'        , 01, 008, 1, OnlyNumber(NFSe.Tomador.Endereco.CEP), '');
  Gerador.wCampo(tcStr, '#16','tomador_cod_cidade' , 01, 007, 1, OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), '');
  Gerador.wCampo(tcStr, '#17','tomador_fone'       , 01, 015, 0, NFSe.Tomador.Contato.Telefone, '');
  //Gerador.wCampo(tcStr, '#18','tomador_ramal'    , 01, 100, 0, NFSe.Tomador.Contato.Bairro, '');
  //Gerador.wCampo(tcStr, '#18','tomador_fax'      , 01, 100, 0, NFSe.Tomador.Contato.Bairro, '');
  Gerador.wCampo(tcStr, '#18','rps_num'            , 01, 015, 0, OnlyNumber(NFSe.IdentificacaoRps.Numero), '');
  Gerador.wCampo(tcStr, '#19','rps_serie'          , 01, 003, 0, NFSe.IdentificacaoRps.Serie, '');
  Gerador.wCampo(tcStr, '#20','rps_dia'            , 01, 002, 0, FormatDateTime('dd',NFSe.DataEmissaoRps), '');
  Gerador.wCampo(tcStr, '#21','rps_mes'            , 01, 002, 0, FormatDateTime('MM',NFSe.DataEmissaoRps), '');
  Gerador.wCampo(tcStr, '#22','rps_ano'            , 01, 004, 0, FormatDateTime('yyyy',NFSe.DataEmissaoRps), '');
  Gerador.wCampo(tcDe4, '#23','valor_inss'         , 01, 015, 0, NFSe.Servico.Valores.ValorInss, '');
  Gerador.wCampo(tcDe4, '#24','valor_ir'           , 01, 015, 0, NFSe.Servico.Valores.ValorIr, '');
  Gerador.wCampo(tcDe4, '#25','valor_pis'          , 01, 015, 0, NFSe.Servico.Valores.ValorPis, '');
  Gerador.wCampo(tcDe4, '#26','valor_cofins'       , 01, 015, 0, NFSe.Servico.Valores.ValorCofins, '');
  Gerador.wCampo(tcDe4, '#27','valor_csll'         , 01, 015, 0, NFSe.Servico.Valores.ValorCsll, '');
  //Gerador.wCampo(tcDe4, '#22','valor_iss_fora'   , 01, 015, 0, NFSe.Servico.Valores.ValorRepasse, '');
  //Gerador.wCampo(tcStr, '#22','codig_cidade_local_servico', 01, 007, 0, NFSe.Servico.ItemServico[0].ValorISSST, '');
  Gerador.wGrupo('/DescricaoRps');
end;

procedure TNFSeW_SigISS.GerarPrestador(GerarGrupo: Boolean = True);
begin
  if GerarGrupo then
    Gerador.wGrupo('DadosPrestador');

  Gerador.wCampo(tcStr, '#01', 'ccm'       , 01, 15, 0, NFSe.Prestador.Usuario, '');
  Gerador.wCampo(tcStr, '#02', 'cnpj'      , 11, 14, 1, OnlyNumber(NFSe.Prestador.Cnpj), '');
  Gerador.wCampo(tcStr, '#03', 'senha'     , 01, 10, 1, NFSe.Prestador.Senha, '');
  Gerador.wCampo(tcStr, '#04', 'crc'       , 01, 10, 0, NFSE.Prestador.crc, '');
  Gerador.wCampo(tcStr, '#05', 'crc_estado', 01, 02, 0, NFSE.Prestador.crc_estado, '');

  Gerador.wCampo(tcDe2, '#06', 'aliquota_simples', 01, 15, 0, NFSE.Servico.Valores.AliquotaSN, '');

  if GerarGrupo then
    Gerador.wGrupo('/DadosPrestador');
end;

procedure TNFSeW_SigISS.GerarXML_SigISS;
begin
  //Este provedor não segue o padrao dos outros provedores
  //Existe apenas dois grupos, um com dados básicos do prestador e outro com os dados da RPS

  if NFSe.Prestador.Endereco.CodigoMunicipio <> '3127701' then
   // Para governador valadares, os dados do prestador são inseridos no grupo "DescricaoRPS"
    GerarPrestador;

  GerarIdentificacaoRPS;
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

  if NFSe.Prestador.Endereco.CodigoMunicipio = '3127701' then
    Gerador.Opcoes.QuebraLinha := #13#10
  else
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
