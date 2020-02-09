{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 19/07/2012: Daniel Simoes de Almeida
|*  - Primeira Versao ACBrDISSmakTeclado
******************************************************************************}

{$I ACBr.inc}

unit ACBrDISSmakTeclado;

interface
uses ACBrDISClass,
     Classes;

{ Nota: - A comunicação com a Porta AT não é tão rápida quando a Porta Serial,
          por isso, evite o uso excessivo de textos "animados"
        - A funçao Tx1board() funciona normalmente em Win9x,
        - XP /NT /2000, deve-se usar uma DLL que permita acesso direto
          a porta AT  ( inpout32.dll )  http://www.logix4u.net/inpout32.htm
        - Linux: é necessário ser ROOT para acessar /dev/port
          (use: su  ou  chmod u+s SeuPrograma ) }
type

{ TACBrDISSmakTeclado }

TACBrDISSmakTeclado = class( TACBrDISClass )
  public
    constructor Create(AOwner: TComponent); override;

    procedure Ativar ; override ;

    procedure LimparDisplay ; override ;
    procedure LimparLinha( Linha: Integer ) ; override ;

    procedure PosicionarCursor(Linha, Coluna: Integer ) ; override ;
    procedure Escrever( const Texto : String ) ; override ;
end ;

implementation
Uses
     SysUtils,
     {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, Math, Windows{$ENDIF},
     math ;

{ TACBrDISSmakTeclado}

constructor TACBrDISSmakTeclado.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpModeloStr := 'Smak Teclado' ;
  LinhasCount := 2 ;
  Colunas     := 40 ;
end;

procedure TACBrDISSmakTeclado.LimparDisplay;
begin
  TxKeyboard( $097 );
  TxKeyboard( 1 );
  Sleep(2);
end;

procedure TACBrDISSmakTeclado.LimparLinha(Linha: Integer);
begin
  if Linha = 1 then
     TxKeyboard( $08E )
  else
     TxKeyboard( $08F ) ;
  Sleep(2);
end;

procedure TACBrDISSmakTeclado.PosicionarCursor(Linha, Coluna: Integer);
begin
  TxKeyboard( $096 );
  TxKeyboard( min(Coluna, Colunas) );
  TxKeyboard( min(Linha, LinhasCount) );
end;

procedure TACBrDISSmakTeclado.Escrever(const Texto: String);
Var A : Integer ;
begin
  TxKeyboard( $09A );
  for A := 1 to Length( Texto ) do
     TxKeyboard( ord(Texto[A]) ) ;      // Envia um Byte por vez...
  TxKeyboard( $000 );
end;

procedure TACBrDISSmakTeclado.Ativar;
begin
  { Nao precisa de inicializaçao }
  fpAtivo := true ;
end;

end.
