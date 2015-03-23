{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  André Ferreira Moraes                          }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrSATEmuladorSPstatic ;

interface

uses
  Classes, SysUtils, ACBrSATClass ;

function AssociarAssinatura( numeroSessao : Longint; codigoDeAtivacao: PAnsiChar; CNPJvalue : PAnsiChar; assinaturaCNPJs : PAnsiChar ) : PAnsiChar ; cdecl; External cLIBSAT;
function AtivarSAT( numeroSessao: Longint; subComando : Longint; codigoDeAtivacao: PAnsiChar; CNPJ: PAnsiChar; cUF : Longint ) : PAnsiChar ; cdecl; External cLIBSAT;
function AtualizarSoftwareSAT( numeroSessao : Longint; codigoDeAtivacao : PAnsiChar ) : PAnsiChar ; cdecl; External cLIBSAT;
function BloquearSAT( numeroSessao : Longint; codigoDeAtivacao : PAnsiChar ) : PAnsiChar ; cdecl; External cLIBSAT;
function CancelarUltimaVenda(numeroSessao : Longint; codigoAtivacao: PAnsiChar; chave: PAnsiChar; dadosCancelamento : PAnsiChar) : PAnsiChar ; cdecl;  External cLIBSAT;
function ComunicarCertificadoICPBRASIL( numeroSessao : Longint; codigoDeAtivacao : PAnsiChar; certificado : PAnsiChar ) : PAnsiChar ; cdecl; External cLIBSAT;
function ConfigurarInterfaceDeRede( numeroSessao : Longint; codigoDeAtivacao : PAnsiChar; dadosConfiguracao : PAnsiChar ) : PAnsiChar ; cdecl; External cLIBSAT;
function ConsultarNumeroSessao(numeroSessao : Longint; cNumeroDeSessao : Longint) : PAnsiChar ; cdecl;  External cLIBSAT;
function ConsultarSAT( numeroSessao : Longint ) : PAnsiChar ; cdecl; External cLIBSAT;
function ConsultarStatusOperacional( numeroSessao : Longint; codigoDeAtivacao : PAnsiChar ) : PAnsiChar ; cdecl; External cLIBSAT;
function DesbloquearSAT( numeroSessao : Integer; codigoDeAtivacao : PAnsiChar ) : PAnsiChar ; cdecl; External cLIBSAT;
function DesligarSAT : PAnsiChar ; cdecl; External cLIBSAT;
function EnviarDadosVenda(numeroSessao : Longint; codigoDeAtivacao: PAnsiChar; dadosVenda : PAnsiChar) : PAnsiChar ; cdecl; External cLIBSAT;
function ExtrairLogs( numeroSessao : Longint; codigoDeAtivacao : PAnsiChar ) : PAnsiChar ; cdecl; External cLIBSAT;
function TesteFimAFim(numeroSessao : Longint; codigoDeAtivacao: PAnsiChar; dadosVenda : PAnsiChar) : PAnsiChar ; cdecl; External cLIBSAT;
function TrocarCodigoDeAtivacao( numeroSessao : Longint; codigoDeAtivacao : PAnsiChar; opcao : Longint; novoCodigo : PAnsiChar; confNovoCodigo : PAnsiChar ) : PAnsiChar ; cdecl; External cLIBSAT;

implementation

end.

