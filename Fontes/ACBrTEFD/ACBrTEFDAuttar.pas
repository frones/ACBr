{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

{******************************************************************************
|* Historico
|*
|* 21/11/2009: Daniel Simoes de Almeida
|*  - Primeira Versao: Criaçao e Distribuiçao da Primeira Versao
******************************************************************************}

{$I ACBr.inc}

unit ACBrTEFDAuttar;

interface

uses
  Classes, SysUtils, ACBrTEFDClass;

const
  CACBrTEFDAuttar_ArqTemp   = 'C:\Auttar_TefIP\req\intpos.tmp' ;
  CACBrTEFDAuttar_ArqReq    = 'C:\Auttar_TefIP\req\intpos.001' ;
  CACBrTEFDAuttar_ArqResp   = 'C:\Auttar_TefIP\resp\intpos.001' ;
  CACBrTEFDAuttar_ArqSTS    = 'C:\Auttar_TefIP\resp\intpos.sts' ;
  CACBrTEFDAuttar_GPExeName = 'C:\Program Files (x86)\Auttar\IntegradorTEF-IP.exe' ;


type
   { TACBrTEFDAuttar }

   TACBrTEFDAuttar = class( TACBrTEFDClassTXT )
   private
   protected
     procedure FinalizarRequisicao ; override;
   public
     constructor Create( AOwner : TComponent ) ; override ;
   end;

implementation

Uses ACBrUtil, dateutils, ACBrTEFD;

{ TACBrTEFDClass }

constructor TACBrTEFDAuttar.Create(AOwner : TComponent);
var
  DirApp : String ;
begin
  inherited Create(AOwner);

  ArqReq    := CACBrTEFDAuttar_ArqReq ;
  ArqResp   := CACBrTEFDAuttar_ArqResp ;
  ArqSTS    := CACBrTEFDAuttar_ArqSTS ;
  ArqTemp   := CACBrTEFDAuttar_ArqTemp ;

  GPExeName := '';
  DirApp := Trim(GetEnvironmentVariable('ProgramFiles(x86)'));
  if DirApp = '' then
     DirApp := Trim(GetEnvironmentVariable('ProgramFiles'));
  if DirApp <> '' then
     GPExeName := DirApp + PathDelim+'Auttar'+PathDelim+'IntegradorTEF-IP.exe' ;

  if GPExeName = '' then
     GPExeName := CACBrTEFDAuttar_GPExeName ;

  fpTipo    := gpTefAuttar;
  Name      := 'TEFAuttar' ;
end;

procedure TACBrTEFDAuttar.FinalizarRequisicao ;
begin
  VerificarIniciouRequisicao;

  if (pos(Req.Header,'CRT|CNF') > 0) and                       // É CRT ou CNF ?
     ( TACBrTEFD(Owner).RespostasPendentes.Count > 0 ) then    // É acima de 1 cartão ?
     Req.GravaInformacao(099,000,'1'); // Campo obrigatorio após segundo cartão com valor = 1

  inherited FinalizarRequisicao ;
end ;

end.

