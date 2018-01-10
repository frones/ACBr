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

unit ACBrDISGertecTEC65lib;

interface
uses ACBrDISClass,
     Classes;

{ Nota: - Essa Unit depende da DLL da Gertec tec65_32.dll, a qual é carregada 
  dinamicamente ao Ativar }

const
  CTEC65LIB = 'tec65_32.dll';

type

{ TACBrDISGertecTEC65lib }

TACBrDISGertecTEC65lib = class( TACBrDISClass )
  private
      xOpenTec65 : function : integer; stdcall;
      XCloseTec65: function : integer; stdcall;
      xGoToXY : function (lin, col: integer): integer; stdcall;
      xDispStr : function (Str: PAnsiChar): integer; stdcall;
      xFormFeed : function : integer; stdcall;

      procedure FunctionDetectLib(FuncName: String; var LibPointer: Pointer);
      procedure LoadDLLFunctions;
      procedure UnLoadDLLFunctions;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Ativar ; override ;
    procedure Desativar; override;
    
    procedure LimparDisplay ; override ;

    procedure PosicionarCursor(Linha, Coluna: Integer ) ; override ;
    procedure Escrever( Texto : String ) ; override ;
end ;

implementation
Uses ACBrUtil,
     SysUtils,
     {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, Windows{$ENDIF} ;

{ ACBrDISGertecTEC65lib}

constructor TACBrDISGertecTEC65lib.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpModeloStr := 'GertecTeclado65lib' ;
  LinhasCount := 2 ;
  Colunas     := 40 ;
  fpIntervaloEnvioBytes := 0;
end;

destructor TACBrDISGertecTEC65lib.Destroy;
begin
  UnLoadDLLFunctions;
  inherited Destroy;
end;


procedure TACBrDISGertecTEC65lib.LimparDisplay;
begin
  if Assigned(xFormFeed) then
    xFormFeed ;
end;

procedure TACBrDISGertecTEC65lib.PosicionarCursor(Linha, Coluna: Integer);
begin
  if Assigned(xGoToXY) then
    xGoToXY( Linha, Coluna);
end;

procedure TACBrDISGertecTEC65lib.Escrever(Texto: String);
begin
  if Assigned(xDispStr) then
    xDispStr( PAnsiChar( AnsiString(ACBrStrToAnsi(Texto))) );
end;

procedure TACBrDISGertecTEC65lib.Ativar;
begin
  LoadDLLFunctions;
  xOpenTec65;
  fpAtivo := true ;
end;

procedure TACBrDISGertecTEC65lib.Desativar;
begin
  if Assigned(XCloseTec65) then
    XCloseTec65;

  UnLoadDLLFunctions;
  fpAtivo := false ;
end;

procedure TACBrDISGertecTEC65lib.LoadDLLFunctions;
begin
  FunctionDetectLib( 'OpenTec65',  @xOpenTec65 );
  FunctionDetectLib( 'CloseTec65', @XCloseTec65 );
  FunctionDetectLib( 'GoToXY',     @xGoToXY) ;
  FunctionDetectLib( 'DispStr',    @xDispStr);
  FunctionDetectLib( 'FormFeed',   @xFormFeed);
end;

procedure TACBrDISGertecTEC65lib.FunctionDetectLib(FuncName : String ;
  var LibPointer : Pointer) ;
begin
  if not Assigned( LibPointer )  then
  begin
    if not FunctionDetect( CTEC65LIB, FuncName, LibPointer) then
    begin
       LibPointer := NIL ;
       raise EACBrDISErro.Create( ACBrStr(Format('Erro ao carregar a função: %s na Biblioteca: %s', [FuncName,CTEC65LIB])) ) ;
    end ;
  end ;
end ;

procedure TACBrDISGertecTEC65lib.UnLoadDLLFunctions;
begin
  if not Assigned(xOpenTec65) then
    Exit;

  UnLoadLibrary( CTEC65LIB );

  xOpenTec65  := Nil;
  XCloseTec65 := Nil;
  xGoToXY     := Nil;
  xDispStr    := Nil;
  xFormFeed   := Nil;
end;



end.
