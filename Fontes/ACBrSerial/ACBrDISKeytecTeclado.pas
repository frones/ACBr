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
|* 02/06/2006: Fabio Farias
|*  - Primeira Versao ACBrDISKeytecTeclado
******************************************************************************}

{$I ACBr.inc}

unit ACBrDISKeytecTeclado;

interface
uses ACBrDISClass,
     Classes;

{ Nota: - A comunicação com a Porta AT não é tão rápida quando a Porta Serial,
          por isso, evite o uso excessivo de textos "animados"
        - A funçao TxKeyboard() funciona normalmente em Win9x,
        - XP /NT /2000, deve-se usar uma DLL que permita acesso direto
          a porta AT  ( inpout32.dll )  http://www.logix4u.net/inpout32.htm
        - Linux: é necessário ser ROOT para acessar /dev/port
          (use: su  ou  chmod u+s SeuPrograma ) }
type
TACBrDISKeytecTeclado = class( TACBrDISClass )
  public
    constructor Create(AOwner: TComponent); override;

    procedure Ativar ; override ;

    procedure LimparDisplay ; override ;
    procedure PosicionarCursor(Linha, Coluna: Integer ) ; override ;
    procedure Escrever( const Texto : String ) ; override ;
end ;

implementation
Uses
     SysUtils,
     {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, Math, Windows{$ENDIF} ;

{ TACBrDISKeytecTeclado}

constructor TACBrDISKeytecTeclado.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpModeloStr := 'Keytec Teclado' ;
  LinhasCount := 2 ;
  Colunas     := 40 ;
end;

procedure TACBrDISKeytecTeclado.LimparDisplay;
begin
  TxKeyboard( 160 );
  TxKeyboard( 1 );
end;

procedure TACBrDISKeytecTeclado.PosicionarCursor(Linha, Coluna: Integer);
Var Pos : Byte;
begin
  {
   Tabela de enderecamento do Visor
   Linha 1 começa na coluna endereço = Hex 80  ( 128 decimal )
   Linha 2 começa na coluna endereço = Hex C0  ( 192 decimal )
  }
  if Linha = 1 then
     Pos := 127 + Coluna
  else
     Pos := 191 + Coluna;

  TxKeyboard(160);
  TxKeyboard(Pos);
end;

procedure TACBrDISKeytecTeclado.Escrever(const Texto: String);
Var A : Integer ;
begin
  TxKeyboard( 8 );
  for A := 1 to Length( Texto ) do
     TxKeyboard( ord(Texto[A]) ) ;      // Envia um Byte por vez...
  TxKeyboard( 9 );
end;

procedure TACBrDISKeytecTeclado.Ativar;
begin
  { Nao precisa de inicializaçao }
  fpAtivo := true ;
end;

end.
