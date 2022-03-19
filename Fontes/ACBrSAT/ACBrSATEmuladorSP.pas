{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrSATEmuladorSP ;

interface

uses
  Classes, SysUtils, ACBrSATClass, ACBrSATEmuladorSPstatic;

type

   { TACBrSATEmuladorSP }

   TACBrSATEmuladorSP = class( TACBrSATClass )
   private
     xSAT_AssociarAssinatura : function ( numeroSessao : LongInt;
        codigoDeAtivacao, CNPJvalue, assinaturaCNPJs : PAnsiChar ) : PAnsiChar ;
        stdcall;
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
     xSAT_DesligarSAT : function : PAnsiChar ; cdecl;
     xSAT_EnviarDadosVenda : function ( numeroSessao : LongInt;
        codigoDeAtivacao, dadosVenda : PAnsiChar) : PAnsiChar ; cdecl;
     xSAT_ExtrairLogs : function ( numeroSessao : LongInt;
        codigoDeAtivacao : PAnsiChar ) : PAnsiChar ; cdecl;
     xSAT_TesteFimAFim : function ( numeroSessao : LongInt;
        codigoDeAtivacao, dadosVenda : PAnsiChar) : PAnsiChar ; cdecl;
     xSAT_TrocarCodigoDeAtivacao : function ( numeroSessao : LongInt;
        codigoDeAtivacao : PAnsiChar; opcao : LongInt; novoCodigo,
        confNovoCodigo : PAnsiChar ) : PAnsiChar ; cdecl;
     xSAT_ConsultarUltimaSessaoFiscal : function ( numeroSessao : LongInt;
        codigoDeAtivacao : PAnsiChar) : PAnsiChar ; cdecl;

   protected
     procedure LoadDLLFunctions ; override;
     procedure UnLoadDLLFunctions ; override;

   public
     constructor Create( AOwner : TComponent ) ; override;

     function AssociarAssinatura( CNPJvalue, assinaturaCNPJs : AnsiString ):
       String ; override;
     function AtivarSAT( subComando : Integer; CNPJ: String; cUF : Integer )
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
     function DesligarSAT : String ; override;
     function EnviarDadosVenda( dadosVenda : AnsiString ) : String ; override;
     function ExtrairLogs : String ; override;
     function TesteFimAFim( dadosVenda : AnsiString) : String ; override;
     function TrocarCodigoDeAtivacao( codigoDeAtivacaoOuEmergencia: AnsiString;
       opcao : Integer; novoCodigo: AnsiString ) : String ; override;
     function ConsultarUltimaSessaoFiscal : String ; override;       
   end;

implementation

Uses ACBrUtil;

constructor TACBrSATEmuladorSP.Create(AOwner : TComponent) ;
begin
  inherited Create(AOwner) ;

  fpModeloStr := 'Emulador_SAT_SP' ;
end ;

function TACBrSATEmuladorSP.AssociarAssinatura(CNPJvalue,
  assinaturaCNPJs : AnsiString) : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_AssociarAssinatura( numeroSessao, PAnsiChar(codigoDeAtivacao),
                                   PAnsiChar(CNPJvalue), PAnsiChar(assinaturaCNPJs) ) ;
  Result := String( Resp );
end ;

function TACBrSATEmuladorSP.AtivarSAT(subComando : Integer ;
  CNPJ : String ; cUF : Integer) : String ;
Var
  Resp : PAnsiChar;
begin
  {Resp := xSAT_AtivarSAT( numeroSessao, subComando,
                          PAnsiChar(codigoDeAtivacao), PAnsiChar(CNPJ), cUF);}
  Resp := ACBrSATEmuladorSPstatic.AtivarSAT( numeroSessao, subComando,
                          PAnsiChar(codigoDeAtivacao), PAnsiChar(CNPJ), cUF);
  Result := String( Resp );
end ;

function TACBrSATEmuladorSP.AtualizarSoftwareSAT : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_AtualizarSoftwareSAT( numeroSessao, PAnsiChar(codigoDeAtivacao) ) ;
  Result := String( Resp );
end ;

function TACBrSATEmuladorSP.BloquearSAT : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_BloquearSAT( numeroSessao, PAnsiChar(codigoDeAtivacao) ) ;
  Result := String( Resp );
end ;

function TACBrSATEmuladorSP.CancelarUltimaVenda(chave,
  dadosCancelamento : AnsiString) : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_CancelarUltimaVenda( numeroSessao, PAnsiChar(codigoDeAtivacao),
                                    PAnsiChar(chave), PAnsiChar(dadosCancelamento) ) ;
  Result := String( Resp );
end ;

function TACBrSATEmuladorSP.ComunicarCertificadoICPBRASIL(
  certificado : AnsiString) : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_ComunicarCertificadoICPBRASIL( numeroSessao,
                  PAnsiChar(codigoDeAtivacao), PAnsiChar(certificado) ) ;
  Result := String( Resp );
end ;

function TACBrSATEmuladorSP.ConfigurarInterfaceDeRede(
  dadosConfiguracao : AnsiString) : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_ConfigurarInterfaceDeRede( numeroSessao,
                 PAnsiChar(codigoDeAtivacao), PAnsiChar(dadosConfiguracao) ) ;
  Result := String( Resp );
end ;

function TACBrSATEmuladorSP.ConsultarNumeroSessao(cNumeroDeSessao : Integer
  ) : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_ConsultarNumeroSessao( numeroSessao, PAnsiChar(codigoDeAtivacao),
                                      cNumeroDeSessao) ;
  Result := String( Resp );
end ;

function TACBrSATEmuladorSP.ConsultarSAT : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_ConsultarSAT( numeroSessao ) ;
  Result := String( Resp );
end ;

function TACBrSATEmuladorSP.ConsultarStatusOperacional : String ;
Var
  Resp : PAnsiChar;
begin
  //Resp := xSAT_ConsultarStatusOperacional( numeroSessao, PAnsiChar(codigoDeAtivacao) ) ;
  Resp := ACBrSATEmuladorSPstatic.ConsultarStatusOperacional( numeroSessao, PAnsiChar(codigoDeAtivacao) ) ;

  Result := String( Resp );
end ;

function TACBrSATEmuladorSP.ConsultarUltimaSessaoFiscal: String;
Var
  Resp : PAnsiChar;
begin
  if not Assigned(xSAT_ConsultarUltimaSessaoFiscal) then
    raise EACBrSATErro.Create( Format(cACBrSATFuncaoNaoEncontrada, ['ConsultarUltimaSessaoFiscal', NomeDLL]) ) ;

  Resp := xSAT_ConsultarUltimaSessaoFiscal( numeroSessao, PAnsiChar(codigoDeAtivacao) );
  Result := String( Resp );
end;

function TACBrSATEmuladorSP.DesbloquearSAT : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_DesbloquearSAT( numeroSessao, PAnsiChar(codigoDeAtivacao) );
  Result := String( Resp );
end ;

function TACBrSATEmuladorSP.DesligarSAT : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_DesligarSAT ;
  Result := String( Resp );
end ;

function TACBrSATEmuladorSP.EnviarDadosVenda(dadosVenda : AnsiString) : String ;
Var
  Resp : PAnsiChar;
begin
  {Resp := xSAT_EnviarDadosVenda( numeroSessao, PAnsiChar(codigoDeAtivacao),
                                   PAnsiChar(dadosVenda) ) ;}
  Resp := ACBrSATEmuladorSPstatic.EnviarDadosVenda( numeroSessao, PAnsiChar(codigoDeAtivacao),
                                                    PAnsiChar(dadosVenda) ) ;
  Result := String( Resp );
end ;

function TACBrSATEmuladorSP.ExtrairLogs : String ;
Var
  Resp : PAnsiChar;
begin
  Resp := xSAT_ExtrairLogs( numeroSessao, PAnsiChar(codigoDeAtivacao) ) ;
  Result := String( Resp );
end ;

function TACBrSATEmuladorSP.TesteFimAFim(dadosVenda : AnsiString) : String ;
Var
  Resp : PAnsiChar;
begin
  { Resp := xSAT_TesteFimAFim( numeroSessao, PAnsiChar(codigoDeAtivacao),
                               PAnsiChar(dadosVenda) ); }
  Resp := ACBrSATEmuladorSPstatic.TesteFimAFim( numeroSessao, PAnsiChar(codigoDeAtivacao),
                                                PAnsiChar(dadosVenda) );
  Result := String( Resp );
end ;

function TACBrSATEmuladorSP.TrocarCodigoDeAtivacao(
  codigoDeAtivacaoOuEmergencia: AnsiString; opcao: Integer;
  novoCodigo: AnsiString): String;
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

procedure TACBrSATEmuladorSP.LoadDLLFunctions;
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
  FunctionDetectLibSAT( 'DesbloquearSAT', @xSAT_DesbloquearSAT);
  FunctionDetectLibSAT( 'DesligarSAT', @xSAT_DesligarSAT);
  FunctionDetectLibSAT( 'ExtrairLogs', @xSAT_ExtrairLogs);
  FunctionDetectLibSAT( 'TesteFimAFim', @xSAT_TesteFimAFim) ;
  FunctionDetectLibSAT( 'TrocarCodigoDeAtivacao', @xSAT_TrocarCodigoDeAtivacao);
  // Função é nova, e pode não estar disponível nas DLLs antigas
  try
    FunctionDetectLibSAT( 'ConsultarUltimaSessaoFiscal', @xSAT_ConsultarUltimaSessaoFiscal);
  except
  end;
end;

procedure TACBrSATEmuladorSP.UnLoadDLLFunctions;
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
  xSAT_ConsultarUltimaSessaoFiscal    := Nil;
end;

end.

