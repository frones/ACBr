{*******************************************************************************}
{ Projeto: Componentes ACBr                                                     }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa-  }
{ mentos de Automação Comercial utilizados no Brasil                            }
{                                                                               }
{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                 }
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

unit ACBrLibMDFeConsts;

interface

uses
  Classes, SysUtils;

const
  CSessaoDAMDFe = 'DAMDFe';

  CChaveTipoRelatorioEvento = 'TipoRelatorioEvento';
  CChaveTipoDAMDFe = 'TipoDAMDFe';
  CChaveCancelada = 'Cancelada';
  CChaveEncerrado = 'Encerrado';
  CChaveImprimeHoraSaida = 'ImprimeHoraSaida';
  CChavePrintDialog = 'PrintDialog';
  CChaveImprimeHoraSaida_Hora = 'ImprimeHoraSaida_Hora';
  CChaveProtocolo = 'Protocolo';
  CChaveTamanhoPapel = 'TamanhoPapel';

  CSessaoRespEnvio = 'Envio';
  CSessaoRespStatus = 'Status';
  CSessaoRespConsulta = 'Consulta';
  CSessaoRespCancelamento = 'Cancelamento';
  CSessaoRespEncerramento = 'Encerramento';
  CSessaoNaoEncerrados = 'NAOENCERRADOS';
  CSessaoRespNaoEncerrados = 'NaoEncerrados';
  CSessaoRespEvento = 'Evento';

  ErrValidacaoMDFe = -11;
  ErrChaveMDFe = -12;
  ErrAssinarMDFe = -13;
  ErrConsulta = -14;
  ErrCNPJ = -15;
  ErrRetorno = -16;
  ErrEnvio = -17;
  ErrEnvioEvento = -18;

Resourcestring
  SInfMDFeCarregados = '%d MDFe(s) Carregado(s)';
  SInfEventosCarregados = '%d Evento(s) Carregado(s)';

  SErrChaveInvalida = 'Chave % inválida.';
  SErrCNPJInvalido = 'CNPJ % inválido.';
  SErrCNPJCPFInvalido = 'CNPJ/CPF % inválido.';

function SetRetornoMDFeCarregados(const NumMDFe: Integer): Integer;
function SetRetornoEventoCarregados(const NumEventos: Integer): Integer;

implementation

uses
  ACBrLibComum;

function SetRetornoMDFeCarregados(const NumMDFe: Integer): Integer;
begin
  Result := SetRetorno( 0, {NumMDFe,} Format(SInfMDFeCarregados, [NumMDFe]));
end;

function SetRetornoEventoCarregados(const NumEventos: Integer): Integer;
begin
  Result := SetRetorno( 0, {NumEventos,} Format(SInfEventosCarregados, [NumEventos]));
end;

end.

