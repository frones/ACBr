{ criar uma interface de comunicação com equipamentos de automacao comercial.   }
{                                                                               }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: José M S Junior                                  }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************} 

{$I ACBr.inc}

unit ACBrLibConsultaCNPJConsts;

interface

uses
  Classes, SysUtils;

const
  CLibConsultaCNPJNome = 'ACBrLibConsultaCNPJ';
  CLibConsultaCNPJVersao = '0.0.0.0';

  CSessaoRespConsulta = 'Consulta';

  CSessaoConsultaCNPJ = 'ConsultaCNPJ';

  CChaveAbertura = 'Abertura';
  CChaveEndereco = 'Endereco';
  CChaveNumero   = 'Numero';
  CChaveComplemento = 'Complemento';
  CChaveBairo = 'Bairro';
  CChaveCidade = 'Cidade';
  CChaveUF = 'UF';
  CChaveCEP = 'CEP';
  CChaveSituacao = 'Situacao';
  CChaveCNAE1 = 'CNAE1';
  CChaveCNAE2 = 'CNAE2';
  CChaveNaturezaJuridica = 'NaturezaJuridica';
  CChaveProvedor = 'Provedor';


implementation

end.

