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
|* 03/10/2014: Daniel Simoes de Almeida
|*  - Primeira Versao ACBrDISGertecTEC65lib
******************************************************************************}

{$I ACBr.inc}

unit ACBrDISSmakTecladoLib;

interface
uses ACBrDISClass,
     Classes;

{ Nota: - Essa Unit depende da DLL da Smak a qual é carregada dinamicamente ao Ativar }

const
  {$IfDef MSWINDOWS}
  CSKLIB = 'sk_access.dll';
  {$Else}
  CSKLIB = 'libsk_access.so';
  {$EndIf}

type

{ TACBrDISSmakTecladoLib }

TACBrDISSmakTecladoLib = class( TACBrDISClass )
  private
      xReset_Interface : procedure; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
      xFree_Sk_Access: procedure; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
      xGotoxy : procedure (lin, col: byte); {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
      xDisp : procedure (pData: PAnsiChar); {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
      xClear_Disp : procedure; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
      xClear_L1 : procedure; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
      xClear_L2 : procedure; {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

      procedure FunctionDetectLib(FuncName: String; var LibPointer: Pointer);
      procedure LoadDLLFunctions;
      procedure UnLoadDLLFunctions;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Ativar ; override ;
    procedure Desativar; override;
    
    procedure LimparDisplay ; override ;
    procedure LimparLinha( Linha: Integer ) ; override ;

    procedure PosicionarCursor(Linha, Coluna: Integer ) ; override ;
    procedure Escrever( Texto : String ) ; override ;
end ;

implementation
Uses ACBrUtil,
     SysUtils,
     {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, Windows{$ENDIF} ;

{ ACBrDISGertecTEC65lib}

constructor TACBrDISSmakTecladoLib.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpModeloStr := 'SmakTecladoLib' ;
  LinhasCount := 2 ;
  Colunas     := 40 ;
  fpIntervaloEnvioBytes := 0;
end;

destructor TACBrDISSmakTecladoLib.Destroy;
begin
  UnLoadDLLFunctions;
  inherited Destroy;
end;


procedure TACBrDISSmakTecladoLib.LimparDisplay;
begin
  if Assigned(xClear_Disp) then
    xClear_Disp;
end;

procedure TACBrDISSmakTecladoLib.LimparLinha(Linha: Integer);
begin
  if (Linha = 1) then
  begin
    if Assigned(xClear_L1) then
      xClear_L1;
  end
  else
  begin
    if Assigned(xClear_L2) then
      xClear_L2;
  end;
end;

procedure TACBrDISSmakTecladoLib.PosicionarCursor(Linha, Coluna: Integer);
begin
  if Assigned(xGotoxy) then
    xGotoxy( Linha, Coluna);
end;

procedure TACBrDISSmakTecladoLib.Escrever(Texto: String);
begin
  if Assigned(xDisp) then
    xDisp( PAnsiChar( AnsiString(ACBrStrToAnsi(Texto))) );
end;

procedure TACBrDISSmakTecladoLib.Ativar;
begin
  LoadDLLFunctions;
  xReset_Interface;  // Esquece da configuração do ultimo teclado detectado
  fpAtivo := true ;
end;

procedure TACBrDISSmakTecladoLib.Desativar;
begin
  UnLoadDLLFunctions;
  fpAtivo := false ;
end;

procedure TACBrDISSmakTecladoLib.LoadDLLFunctions;
begin
  FunctionDetectLib( 'Reset_Interface', @xReset_Interface );
  FunctionDetectLib( 'Free_Sk_Access' , @xFree_Sk_Access );
  FunctionDetectLib( 'Gotoxy'         , @xGotoxy) ;
  FunctionDetectLib( 'Disp'           , @xDisp);
  FunctionDetectLib( 'Clear_Disp'     , @xClear_Disp);
  FunctionDetectLib( 'Clear_L1'       , @xClear_L1);
  FunctionDetectLib( 'Clear_L2'       , @xClear_L2);
end;

procedure TACBrDISSmakTecladoLib.FunctionDetectLib(FuncName : String ;
  var LibPointer : Pointer) ;
begin
  if not Assigned( LibPointer )  then
  begin
    if not FunctionDetect( CSKLIB, FuncName, LibPointer) then
    begin
       LibPointer := NIL ;
       raise EACBrDISErro.Create( ACBrStr(Format('Erro ao carregar a função: %s na Biblioteca: %s', [FuncName,CSKLIB])) ) ;
    end ;
  end ;
end ;

procedure TACBrDISSmakTecladoLib.UnLoadDLLFunctions;
begin
  if Assigned(xFree_Sk_Access) then
    xFree_Sk_Access;

  UnLoadLibrary( CSKLIB );

  xReset_Interface := Nil;
  xFree_Sk_Access  := Nil;
  xGotoxy          := Nil;
  xDisp            := Nil;
  xClear_Disp      := Nil;
  xClear_L1        := Nil;
  xClear_L2        := Nil;
end;


end.
