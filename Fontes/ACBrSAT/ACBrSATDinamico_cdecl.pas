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

unit ACBrSATDinamico_cdecl ;

interface

uses
  Classes, SysUtils, ACBrSATClass;

type

   { TACBrSATDinamico_cdecl }

   TACBrSATDinamico_cdecl = class( TACBrSATClass )
   private
     xSAT_AssociarAssinatura : function ( numeroSessao : LongInt;
        codigoDeAtivacao, CNPJvalue, assinaturaCNPJs : PAnsiChar ) : PAnsiChar ;
        cdecl;
     xSAT_AtivarSAT : function ( numeroSessao, subComando : LongInt;
        codigoDeAtivacao, CNPJ: PAnsiChar; cUF : LongInt ) : PAnsiChar ; cdecl;
     xSAT_AtualizarSoftwareSAT : function ( numeroSessao : LongInt;
        codigoDeAtivacao : PAnsiChar ) : PAnsiChar ; cdecl;
     xSAT_BloquearSAT : function ( numeroSessao : LongInt;
        codigoDeAtivacao : PAnsiChar ) : PAnsiChar ; cdecl;
     xSAT_CancelarUltimaVenda : function (numeroSessao : LongInt;
        codigoAtivacao, chave, dadosCancelamento : PAnsiChar ) : PAnsiChar ; cdecl;
     xSAT_ComunicarCertificadoICPBRASIL : function ( numeroSessao : LongInt;
        codigoDeAtivacao, certificado : PAnsiChar ) : PAnsiChar ; cdecl;
     xSAT_ConfigurarInterfaceDeRede : function ( numeroSessao : LongInt;
        codigoDeAtivacao, dadosConfiguracao : PAnsiChar ) : PAnsiChar ; cdecl;
     xSAT_ConsultarNumeroSessao : function (numeroSessao : LongInt;
        codigoDeAtivacao : PAnsiChar; cNumeroDeSessao : LongInt) : PAnsiChar ; cdecl;
     xSAT_ConsultarSAT : function ( numeroSessao : LongInt ) : PAnsiChar ; cdecl;
     xSAT_ConsultarStatusOperacional : function ( numeroSessao : LongInt;
        codigoDeAtivacao : PAnsiChar ) : PAnsiChar ; cdecl;
     xSAT_DesbloquearSAT : function ( numeroSessao : LongInt;
        codigoDeAtivacao : PAnsiChar ) : PAnsiChar ; cdecl;
     xSAT_EnviarDadosVenda : function ( numeroSessao : LongInt;
        codigoDeAtivacao, dadosVenda : PAnsiChar) : PAnsiChar ; cdecl;
     xSAT_ExtrairLogs : function ( numeroSessao : LongInt;
        codigoDeAtivacao : PAnsiChar ) : PAnsiChar ; cdecl;
     xSAT_TesteFimAFim : function ( numeroSessao : LongInt;
        codigoDeAtivacao, dadosVenda : PAnsiChar) : PAnsiChar ; cdecl;
     xSAT_TrocarCodigoDeAtivacao : function ( numeroSessao : LongInt;
        codigoDeAtivacao : PAnsiChar; opcao : LongInt; novoCodigo,
        confNovoCodigo : PAnsiChar ) : PAnsiChar ; cdecl;

   protected
     procedure LoadDLLFunctions ; override;
     procedure UnLoadDLLFunctions ; override;

   public
     constructor Create( AOwner : TComponent ) ; override;

     function AssociarAssinatura(const CNPJvalue, assinaturaCNPJs : AnsiString ):
       String ; override;
     function AtivarSAT( subComando : Integer; CNPJ: AnsiString; cUF : Integer )
       : String ; override;
     function AtualizarSoftwareSAT : String ; override;
     function BloquearSAT : String ; override;
     function CancelarUltimaVenda( chave, dadosCancelamento : AnsiString ) :
       String ; override;
     function ComunicarCertificadoICPBRASIL( certificado : AnsiString ) :
       String ; override;
     function ConfigurarInterfaceDeRede( dadosConfiguracao : AnsiString ) :
       String ; override;
     function ConsultarNumeroSessao( cNumeroDeSessao : Integer) : String ;
       override;
     function ConsultarSAT : String ; override ;
     function ConsultarStatusOperacional : String ; override;
     function DesbloquearSAT : String ; override;
     function EnviarDadosVenda( dadosVenda : AnsiString ) : String ; override;
     function ExtrairLogs : String ; override;
     function TesteFimAFim( dadosVenda : AnsiString) : String ; override;
     function TrocarCodigoDeAtivacao( codigoDeAtivacaoOuEmergencia: AnsiString;
       opcao : Integer; novoCodigo: AnsiString ) : String ; override;
   end;

implementation

Uses ACBrUtil;

constructor TACBrSATDinamico_cdecl.Create(AOwner : TComponent) ;
begin
  inherited Create(AOwner) ;

  fpModeloStr := 'Emulador_SAT_Dinamico_cdecl' ;
end ;

function TACBrSATDinamico_cdecl.AssociarAssinatura(const CNPJvalue,
  assinaturaCNPJs : AnsiString) : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_AssociarAssinatura(  numeroSessao,
                                    PAnsiChar(codigoDeAtivacao),
                                    PAnsiChar(CNPJvalue),
                                    PAnsiChar(assinaturaCNPJs) ) ;
  Result := String( Resp );
end ;

function TACBrSATDinamico_cdecl.AtivarSAT(subComando : Integer ;
  CNPJ : AnsiString; cUF : Integer) : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_AtivarSAT( numeroSessao, subComando,
                          PAnsiChar(codigoDeAtivacao), PAnsiChar(CNPJ), cUF);
  Result := String( Resp );
end ;

function TACBrSATDinamico_cdecl.AtualizarSoftwareSAT : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_AtualizarSoftwareSAT( numeroSessao, PAnsiChar(codigoDeAtivacao) ) ;
  Result := String( Resp );
end ;

function TACBrSATDinamico_cdecl.BloquearSAT : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_BloquearSAT( numeroSessao, PAnsiChar(codigoDeAtivacao) ) ;
  Result := String( Resp );
end ;

function TACBrSATDinamico_cdecl.CancelarUltimaVenda(chave,
  dadosCancelamento : AnsiString) : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_CancelarUltimaVenda( numeroSessao, PAnsiChar(codigoDeAtivacao),
                                    PAnsiChar(chave), PAnsiChar(dadosCancelamento) ) ;
  Result := String( Resp );
end ;

function TACBrSATDinamico_cdecl.ComunicarCertificadoICPBRASIL(
  certificado : AnsiString) : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_ComunicarCertificadoICPBRASIL( numeroSessao,
                  PAnsiChar(codigoDeAtivacao), PAnsiChar(certificado) ) ;
  Result := String( Resp );
end ;

function TACBrSATDinamico_cdecl.ConfigurarInterfaceDeRede(
  dadosConfiguracao : AnsiString) : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_ConfigurarInterfaceDeRede( numeroSessao,
                 PAnsiChar(codigoDeAtivacao), PAnsiChar(dadosConfiguracao) ) ;
  Result := String( Resp );
end ;

function TACBrSATDinamico_cdecl.ConsultarNumeroSessao(cNumeroDeSessao : Integer
  ) : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_ConsultarNumeroSessao( numeroSessao, PAnsiChar(codigoDeAtivacao),
                                        cNumeroDeSessao) ;
  Result := String( Resp );
end ;

function TACBrSATDinamico_cdecl.ConsultarSAT : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_ConsultarSAT( numeroSessao ) ;
  Result := String( Resp );
end ;

function TACBrSATDinamico_cdecl.ConsultarStatusOperacional : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_ConsultarStatusOperacional( numeroSessao, PAnsiChar(codigoDeAtivacao) ) ;
  Result := String( Resp );
end ;

function TACBrSATDinamico_cdecl.DesbloquearSAT : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_DesbloquearSAT( numeroSessao, PAnsiChar(codigoDeAtivacao) );
  Result := String( Resp );
end ;

function TACBrSATDinamico_cdecl.EnviarDadosVenda(dadosVenda : AnsiString) : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_EnviarDadosVenda( numeroSessao, PAnsiChar(codigoDeAtivacao),
                                   PAnsiChar(dadosVenda) ) ;
  Result := String( Resp );
end ;

function TACBrSATDinamico_cdecl.ExtrairLogs : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_ExtrairLogs( numeroSessao, PAnsiChar(codigoDeAtivacao) ) ;
  Result := String( Resp );
end ;

function TACBrSATDinamico_cdecl.TesteFimAFim(dadosVenda : AnsiString) : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_TesteFimAFim( numeroSessao, PAnsiChar(codigoDeAtivacao),
                               PAnsiChar(dadosVenda) );
  Result := String( Resp );
end ;

function TACBrSATDinamico_cdecl.TrocarCodigoDeAtivacao(
  codigoDeAtivacaoOuEmergencia: AnsiString; opcao: Integer; novoCodigo: AnsiString
  ): String;
Var
  Resp : PAnsiChar;
begin
  if codigoDeAtivacaoOuEmergencia = '' then
    codigoDeAtivacaoOuEmergencia := codigoDeAtivacao;

  Resp := xSAT_TrocarCodigoDeAtivacao( numeroSessao,
                                       PAnsiChar(codigoDeAtivacaoOuEmergencia),
                                       opcao,
                                       PAnsiChar(novoCodigo),
                                       PAnsiChar(novoCodigo) ) ;
  Result := String( Resp );
end ;

procedure TACBrSATDinamico_cdecl.LoadDLLFunctions;
begin
  FunctionDetectLibSAT( 'AssociarAssinatura', @xSAT_AssociarAssinatura );
  FunctionDetectLibSAT( 'AtivarSAT', @xSAT_AtivarSAT );
  FunctionDetectLibSAT( 'AtualizarSoftwareSAT', @xSAT_AtualizarSoftwareSAT) ;
  FunctionDetectLibSAT( 'BloquearSAT', @xSAT_BloquearSAT);
  FunctionDetectLibSAT( 'CancelarUltimaVenda', @xSAT_CancelarUltimaVenda);
  FunctionDetectLibSAT( 'ComunicarCertificadoICPBRASIL', @xSAT_ComunicarCertificadoICPBRASIL);
  FunctionDetectLibSAT( 'ConfigurarInterfaceDeRede', @xSAT_ConfigurarInterfaceDeRede);
  FunctionDetectLibSAT( 'ConsultarNumeroSessao', @xSAT_ConsultarNumeroSessao);
  FunctionDetectLibSAT( 'ConsultarSAT', @xSAT_ConsultarSAT);
  FunctionDetectLibSAT( 'ConsultarStatusOperacional', @xSAT_ConsultarStatusOperacional);
  FunctionDetectLibSAT( 'DesbloquearSAT', @xSAT_DesbloquearSAT);
  FunctionDetectLibSAT( 'EnviarDadosVenda', @xSAT_EnviarDadosVenda);
  FunctionDetectLibSAT( 'ExtrairLogs', @xSAT_ExtrairLogs);
  FunctionDetectLibSAT( 'TesteFimAFim', @xSAT_TesteFimAFim) ;
  FunctionDetectLibSAT( 'TrocarCodigoDeAtivacao', @xSAT_TrocarCodigoDeAtivacao);
end;

procedure TACBrSATDinamico_cdecl.UnLoadDLLFunctions;
begin
  inherited UnLoadDLLFunctions;

  xSAT_AssociarAssinatura             := Nil;
  xSAT_AtivarSAT                      := Nil;
  xSAT_AtualizarSoftwareSAT           := Nil;
  xSAT_BloquearSAT                    := Nil;
  xSAT_CancelarUltimaVenda            := Nil;
  xSAT_ComunicarCertificadoICPBRASIL  := Nil;
  xSAT_ConfigurarInterfaceDeRede      := Nil;
  xSAT_ConsultarNumeroSessao          := Nil;
  xSAT_ConsultarSAT                   := Nil;
  xSAT_ConsultarStatusOperacional     := Nil;
  xSAT_DesbloquearSAT                 := Nil;
  xSAT_EnviarDadosVenda               := Nil;
  xSAT_ExtrairLogs                    := Nil;
  xSAT_TesteFimAFim                   := Nil;
  xSAT_TrocarCodigoDeAtivacao         := Nil;
end;

(* Resposta válida do Kryptus
Result := IntToStr(numeroSessao)+#10+
'|06000'+#10+
'|0000'+#10+
'|Emitido com sucesso + conte?do das notas'+#10+
'|999'+#10+
'|Aviso nao catalogado'+#10+
'|PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz48Q0ZlIHhtbG5zPSJodHRwOi8vd3d3LmZhemVuZGEuc3AuZ292LmJyL3NhdCI+PGluZkNGZSB4bWxucz0iaHR0cDovL3d3dy5mYXplbmRhLnNwLmdvdi5ici9zYXQiIHhtbG5zOnhzZD0iaHR0cDovL3d3dy53My5vcmcvMjAwMS9YTUxTY2hlbWEiIHhtbG5zOnhzaT0iaHR0cDovL3d3dy53My5vcmcvMjAwMS9YTUxTY2hlbWEtaW5zdGFuY2UiIElkPSJDRmUzNTAxMDEwNTc2MTA5ODAwMDExMzU5OTAwMDAwMTg2MDAwMDAxMTkzMzEzMCIgdmVyc2FvPSIwLjAzIiB2ZXJzYW9EYWRvc0VudD0iMC4wMyIgdmVyc2FvU0I9IjAxMDAwMCI+PGlkZT48Y1VGPjM1PC9jVUY+PGNORj4xOTMzMTM8L2NORj48bW9kPjU5PC9tb2Q+PG5zZXJpZVNBVD45MDAwMDAxODY8L25zZXJpZVNBVD48bkNGZT4wMDAwMDE8L25DRmU+PGRFbWk+MjAwMTAxMDE8L2RFbWk+PGhFbWk+MDAwMDAwPC9oRW1pPjxjRFY+MDwvY0RWPjx0cEFtYj4yPC90cEFtYj48Q05QSj4xNjcxNjExNDAwMDE3MjwvQ05QSj48c2lnbkFDPjRGVXljNUg1eFF4N0pGR0dZOGNLbFB4RVNnSFM0RlV5YzVINXhReDdKRkdHWThjS2xQeEVTZ0hTNEZVeWM1SDV4UXg3SkZHR1k4Y0tsUHhFU2dIUzRGVXljNUg1eFF4N0pGR0dZOGNLbFB4RVNnSFM0RlV5YzVINXhReDdKRkdHWThjS2xQeEVTZ0hTNEZVeWM1SDV4UXg3SkZHR1k4Y0tsUHhFU2dIUzRGVXljNUg1eFF4N0pGR0dZOGNLbFB4RVNnSFM0RlV5YzVINXhReDdKRkdHWThjS2xQeEVTZ0hTNE
'|20010101000000'+#10+
'|35010105761098000113599000001860000011933130'+#10+
'|000000000000125'+#10+
'|';
*)

end.

