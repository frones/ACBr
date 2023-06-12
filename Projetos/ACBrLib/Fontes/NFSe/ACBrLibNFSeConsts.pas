{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }
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

unit ACBrLibNFSeConsts;

interface

uses
  Classes, SysUtils;

const
  CLibNFSeNome = 'ACBrLibNFSe';
  CLibNFSeVersao = '1.0.0.9';

  CSessaoRespEnvio = 'Envio';
  CSessaoRespSituacao = 'Situacao';
  CSessaoRespConsultaLote = 'ConsultaLote';
  CSessaoRespConsultaLoteRps = 'ConsultaLoteRps';
  CSessaoRespConsultaNFSePorRps = 'ConsultaNFSePorRps';
  CSessaoRespSubstituirNFSe = 'SubstituirNFSe';
  CSessaoRespGerarToken = 'GerarToken';
  CSessaoRespConsultaNFSe = 'ConsultaNFSe';
  CSessaoRespCancelarNFSe = 'CancelarNFSe';
  CSessaoRespLinkNFSe = 'LinkNFSe';
  CSessaoRespGerarLote = 'GerarLote';
  CSessaoRespEnviarEvento = 'EnviarEvento';
  CSessaoRespConsultarEvento = 'ConsultarEvento';
  CSessaoRespConsultarDFe = 'ConsultarDFe';
  CSessaoRespConsultarParametros = 'ConsultarParametros';
  CSessaoRespConsultaNFSePorNumero = 'ConsultaNFSePorNumero';

  CSessaoRespErro = 'Erro';
  CSessaoRespAlerta = 'Alerta';

  CSessaoDANFSE = 'DANFSe';
  CChavePLogo = 'Prestador.Logo';
  CChavePRazaoSocial = 'Prestador.RazaoSocial';
  CChavePNomeFantasia = 'Prestador.NomeFantasia';
  CChavePEndereco = 'Prestador.Endereco';
  CChavePComplemento = 'Prestador.Complemento';
  CChavePFone = 'Prestador.Fone';
  CChavePMunicipio = 'Prestador.Municipio';
  CChavePIM = 'Prestador.InscricaoMunicipal';
  CChavePEMail = 'Prestador.EMail';
  CChavePUF = 'Prestador.UF';

  CChaveTIE = 'Tomador.InscricaoEstadual';
  CChaveTIM = 'Tomador.InscricaoMunicipal';
  CChaveTFone = 'Tomador.Fone';
  CChaveTEndereco = 'Tomador.Endereco';
  CChaveTComplemento = 'Tomador.Complemento';
  CChaveTEMail = 'Tomador.EMail';

  CChavePrefeitura = 'Prefeitura';
  CChaveTamanhoFonte = 'TamanhoFonte';
  CChaveOutrasInformacaoesImp = 'OutrasInformacaoesImp';
  CChaveAtividade = 'Atividade';
  CChaveFmtNroNFSe = 'FormatarNumeroDocumentoNFSe';
  CChaveNFSeCancelada = 'Cancelada';
  CChaveDetalharServico = 'DetalharServico';
  CChaveProducao = 'Producao';

  ErrValidacaoNFSe = -11;
  ErrChaveNFSe = -12;
  ErrAssinarNFSe = -13;
  ErrConsulta = -14;
  ErrCNPJ = -15;
  ErrRetorno = -16;
  ErrEnvio = -17;
  ErrEnvioEvento = -18;

Resourcestring
  SInfNFSeCarregadas = '%d NFSe(s)/RPS(s) Carregada(s)';


implementation

end.

