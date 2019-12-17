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

unit pnfsNFSeW_Lencois;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ELSE}

{$ENDIF}
  SysUtils, Classes, StrUtils,
  ACBrConsts,
  pnfsNFSeW, pcnAuxiliar, pcnConversao, pcnGerador, pnfsNFSe, pnfsConversao;

type
  { TNFSeW_Lencois }

  TNFSeW_Lencois = class(TNFSeWClass)
  protected
    procedure GerarTomador;
    procedure GerarServicoValores;
    procedure GerarXML_Lencois;
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
{ layout do provedor Lencois.                                                  }
{ Sendo assim só será criado uma nova unit para um novo layout.                }
{==============================================================================}

function _RegimeEspecialTributacaoToStr(const t: TnfseRegimeEspecialTributacao): String;
begin
  result := EnumeradoToStr(t,
                           ['','-2','-4','-5','-6'],
                           [retNenhum, retEstimativa, retCooperativa, retMicroempresarioIndividual, retMicroempresarioEmpresaPP]);
end;

function SimNaoToStr(const t: TnfseSimNao): String;
begin
  result := EnumeradoToStr(t,
                           ['1','0'],
                           [snSim, snNao]);
end;

function _ExigibilidadeISSToStr(const t: TnfseExigibilidadeISS): String;
begin
  // -8 = Fixo
  result := EnumeradoToStr(t,
                           ['-1','-2','-3','-4','-5','-6','-7'],
                           [exiExigivel, exiNaoIncidencia, exiIsencao, exiExportacao, exiImunidade,
                            exiSuspensaDecisaoJudicial, exiSuspensaProcessoAdministrativo]);
end;

function _TipoRPSToStr(const t: TnfseTipoRPS): String;
begin
  result := EnumeradoToStr(t,
                           ['-2','-4','-5'],
                           [trRPS, trNFConjugada, trCupom]);
end;

function _ResponsavelRetencaoToStr(const T: TnfseResponsavelRetencao): String;
begin
  result := EnumeradoToStr(t,
                           ['-1', '-2', '-3'],
                           [ptTomador, rtIntermediario, rtPrestador]);
end;
{ TNFSeW_Lencois }

procedure TNFSeW_Lencois.GerarTomador;
begin
   if (NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '') or (NFSe.Tomador.RazaoSocial <> '') or
      (NFSe.Tomador.Endereco.Endereco <> '') or (NFSe.Tomador.Contato.Telefone <> '') or (NFSe.Tomador.Contato.Email <> '') then begin
      Gerador.wGrupoNFSe('Tomador');
      //
      if NFSe.Tomador.Endereco.UF <> 'EX' then begin
         Gerador.wCampoNFSe(tcStr, '', 'CPF_CNPJ', 11, 14, 1, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), '');
      end;
      //
      Gerador.wCampoNFSe(tcStr, '', 'Nome', 001, 50, 0, NFSe.Tomador.RazaoSocial, '');
      //
      Gerador.wGrupoNFSe('Endereco');
      Gerador.wCampoNFSe(tcStr, '', 'Logradouro ', 001, 095, 1, NFSe.Tomador.Endereco.Endereco, '');
      Gerador.wCampoNFSe(tcStr, '', 'Numero     ', 001, 015, 1, NFSe.Tomador.Endereco.Numero, '');
      Gerador.wCampoNFSe(tcStr, '', 'Complemento', 001, 020, 1, NFSe.Tomador.Endereco.Complemento, '');
      Gerador.wCampoNFSe(tcStr, '', 'Bairro     ', 001, 075, 1, NFSe.Tomador.Endereco.Bairro, '');
      Gerador.wCampoNFSe(tcStr, '', 'Municipio  ', 7, 7, 1, OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), '');
      Gerador.wGrupoNFSe('/Endereco');
      //
      if (NFSe.Tomador.Contato.Email <> '') then begin
         Gerador.wCampoNFSe(tcStr, '', 'Email   ', 01, 050, 0, NFSe.Tomador.Contato.Email, '');
      end;
      //
      if length(OnlyNumber(NFSe.Prestador.Cnpj)) <= 11 then begin
         Gerador.wCampoNFSe(tcStr, '', 'Particular', 001, 001, 1, '1', '');
      end else begin
         Gerador.wCampoNFSe(tcStr, '', 'Particular', 001, 001, 1, '0', '');
      end;
      //
      Gerador.wGrupoNFSe('/Tomador');
   end else begin
      Gerador.wCampoNFSe(tcStr, '', 'Tomador', 0, 1, 1, '', '');
   end;
end;

procedure TNFSeW_Lencois.GerarServicoValores;
begin
  Gerador.wCampoNFSe(tcDe2, '', 'ValorTotal  ', 01, 15, 1, NFSe.Servico.Valores.ValorServicos, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorDeducao', 01, 15, 1, NFSe.Servico.Valores.ValorDeducoes, '');
  //
  Gerador.wCampoNFSe(tcDe6, '', 'Aliquota      ', 01, 07, 1, NFSe.Servico.Valores.Aliquota, '');
  //
  if (NFSe.OptanteSimplesNacional = snNao) then begin
     Gerador.wCampoNFSe(tcDe2, '', 'ValorPIS      ', 01, 15, 1, NFSe.Servico.Valores.ValorPis, '');
     Gerador.wCampoNFSe(tcDe2, '', 'ValorCOFINS   ', 01, 15, 1, NFSe.Servico.Valores.ValorCofins, '');
  end;
  //
  // Retenções
  Gerador.wCampoNFSe(tcDe2, '', 'RetencaoIRRF  ', 01, 15, 1, NFSe.Servico.Valores.ValorIr, '');
  Gerador.wCampoNFSe(tcDe2, '', 'RetencaoINSS  ', 01, 15, 1, NFSe.Servico.Valores.ValorInss, '');
  Gerador.wCampoNFSe(tcDe2, '', 'RetencaoPIS   ', 01, 15, 1, NFSe.Servico.Valores.ValorPis, '');
  Gerador.wCampoNFSe(tcDe2, '', 'RetencaoCOFINS', 01, 15, 1, NFSe.Servico.Valores.ValorCofins, '');
  Gerador.wCampoNFSe(tcDe2, '', 'RetencaoCSLL  ', 01, 15, 1, NFSe.Servico.Valores.ValorCsll, '');
  //
  Gerador.wCampoNFSe(tcInt, '', 'EnviarEmail  ', 01, 01, 1, '1', '');
  //
  Gerador.wCampoNFSe(tcInt, '', 'TributacaoISS ', 01, 02, 1, '0', '');
  //
  if (NFSe.Servico.MunicipioIncidencia > 0) then begin
     Gerador.wGrupoNFSe('RecolhimentoFora');
     Gerador.wCampoNFSe(tcDe6, '', 'Aliquota  ', 01, 08, 1, NFSe.Servico.Valores.Aliquota, '');
     case NFSe.Servico.ResponsavelRetencao of
        ptTomador: Gerador.wCampoNFSe(tcInt, '', 'Obrigacao ', 01, 01, 1, '1', '');
        else Gerador.wCampoNFSe(tcInt, '', 'Obrigacao ', 01, 01, 1, '0', '');;
     end;
     Gerador.wGrupoNFSe('/RecolhimentoFora');
  end;
end;

procedure TNFSeW_Lencois.GerarXML_Lencois;
begin
  Gerador.wCampoNFSe(tcStr, '', 'Versao', 01, 30, 1, '1.1', '');
  Gerador.wCampoNFSe(tcStr, '', 'InscricaoMunicipal', 01, 5, 1, NFSe.Prestador.InscricaoMunicipal, '');
  //
  Gerador.wGrupoNFSe('PASNF');
  Gerador.wCampoNFSe(tcInt, '', 'Numero', 01, 5, 1, NFSe.Numero, '');
  Gerador.wCampoNFSe(tcDat, '', 'Data', 10, 10, 1, NFSe.DataEmissao, '');
  Gerador.wGrupoNFSe('/PASNF');
  //
  GerarTomador;
  //
  if (NFSe.Servico.CodigoMunicipio<>'') then begin
     Gerador.wCampoNFSe(tcInt, '', 'CidadeExecucao', 1, 7, 1, NFSe.Servico.CodigoMunicipio, '');
  end;
  //
  Gerador.wCampoNFSe(tcStr, '', 'Descricao', 1, 1000, 1, NFSe.Servico.Descricao, '');
  //
  GerarServicoValores;
end;

constructor TNFSeW_Lencois.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);
end;

function TNFSeW_Lencois.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

function TNFSeW_Lencois.GerarXml: Boolean;
begin
  Gerador.ListaDeAlertas.Clear;
  Gerador.ArquivoFormatoXML := '';
  Gerador.Prefixo           := FPrefixo4;

  FDefTipos := FServicoEnviar;

  if (RightStr(FURL, 1) <> '/') and (FDefTipos <> '')
    then FDefTipos := '/' + FDefTipos;

  if Trim(FPrefixo4) <> ''
    then Atributo := ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FURL + FDefTipos + '"'
    else Atributo := ' xmlns="' + FURL + FDefTipos + '"';

  Gerador.wGrupo('Nota' + Atributo);

  FNFSe.InfID.ID := OnlyNumber(FNFSe.IdentificacaoRps.Numero) + FNFSe.IdentificacaoRps.Serie;

  GerarXML_Lencois;

  Gerador.wGrupo('/Nota');

  Gerador.gtAjustarRegistros(NFSe.InfID.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.
