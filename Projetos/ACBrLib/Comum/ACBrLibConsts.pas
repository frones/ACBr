{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Rafael Teno Dias                                }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{        Rua Cel.Aureliano de Camargo, 973 - Tatuí - SP - 18270-170            }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrLibConsts;

interface

uses
  Classes, SysUtils;

const
  CLibChaveCrypt = 'tYk*5W@';

  CNomeArqConf = 'acbrlib.ini';
  CSessaoPrincipal = 'Principal';
  CChaveChave = 'Chave';
  CChaveLogNivel = 'LogNivel';
  CChaveLogPath = 'LogPath';
  CChaveTimeOut = 'Timeout';
  CChaveNome = 'Nome';
  CChaveServidor = 'Servidor';
  CChaveUsuario = 'Usuario';
  CChaveSenha = 'Senha';
  CChavePorta = 'Porta';

  CSessaoSistema = 'Sistema';
  CChaveVersao = 'Versao';
  CChaveDescricao = 'Descricao';
  CChaveData = 'Data';

  CSessaoProxy = 'Proxy';
  CSessaoEmail = 'Email';
  CChaveEmailConta = 'Conta';
  CChaveEmailCodificacao = 'Codificacao';
  CChaveEmailSSL = 'SSL';
  CChaveEmailTLS = 'TLS';
  CChaveEmailConfirmacao = 'Confirmacao';
  CChaveEmailSegundoPlano = 'SegundoPlano';

  CSessaoSwHouse = 'SofwareHouse';
  CSessaoEmissor = 'Emissor';

  CChaveNomeFantasia = 'NomeFantasia';
  CChaveRazaoSocial = 'RazaoSocial';
  CChaveCNPJ = 'CNPJ';
  CChaveWebSite = 'WebSite';
  CChaveEmail = 'Email';
  CChaveTelefone = 'Telefone';
  CChaveResponsavel = 'Responsavel';

  ErrOK = 0;
  ErrLibNaoInicializada = -1;
  ErrLibNaoFinalizada = -2;
  ErrConfigNaoInicializado = -3;
  ErrConfigNaoExiste = -3;
  ErrConfigLer = -4;
  ErrConfigGravar = -5;

Resourcestring
  SErrLibJaInicializada = '%s já inicializada.';
  SErrLibNaoInicializada = '%s não foi inicializada';
  SErrLibNaoFinalizada = '%s não foi finalizada';

  SErrDiretorioInvalido = 'Diretório Invalido: %s';
  SErrConfArqNaoDefinido = 'Arquivo de configuração não definido';
  SErrConfArqNaoEncontrado = 'Arquivo de configuração não encontrado';
  SErrConfNaoInstanciado = 'Classe de Configuração não foi instanciada';


implementation

end.

