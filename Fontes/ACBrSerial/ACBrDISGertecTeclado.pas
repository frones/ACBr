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
|* 30/09/2004: Daniel Simoes de Almeida
|*  - Primeira Versao ACBrDISGertecTeclado
|* 05/11/2004: Daniel Simoes de Almeida
|*  - Adcionado suporte a Linux
******************************************************************************}

{$I ACBr.inc}

unit ACBrDISGertecTeclado;

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

{ TACBrDISGertecTeclado }

TACBrDISGertecTeclado = class( TACBrDISClass )
  public
    constructor Create(AOwner: TComponent); override;

    procedure Ativar ; override ;
    
    procedure LimparDisplay ; override ;
    procedure LimparLinha( Linha: Integer ) ; override ;

    procedure PosicionarCursor(Linha, Coluna: Integer ) ; override ;
    procedure Escrever(const  Texto : String ) ; override ;
end ;

implementation
Uses
     SysUtils,
     {$IFDEF COMPILER6_UP} DateUtils {$ELSE} ACBrD5, Windows{$ENDIF} ;

{ TACBrDISGertecTeclado}

constructor TACBrDISGertecTeclado.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpModeloStr := 'Gertec Teclado' ;
  LinhasCount := 2 ;
  Colunas     := 40 ;
  fpIntervaloEnvioBytes := 0;
end;


procedure TACBrDISGertecTeclado.LimparDisplay;
begin
{ TxKeyboard( 231 );  // Liga display
  TxKeyboard( 12 );   // Limpa Display
  TxKeyboard( 232 );  // DesLiga display }

  TxKeyboard( 212 );  // Limpa Display direto 
end;

procedure TACBrDISGertecTeclado.LimparLinha(Linha: Integer);
begin
  if Linha = 1 then
   begin
     TxKeyboard( 219 ) ;   // DB ou 219 Apaga linha 1
//   TxKeyboard( 217 ) ;   // D9 ou 217 -  Teclados Antigos ??
   end
  else
   begin
     TxKeyboard( 221 ) ;   // DD ou 221 = Apaga linha 2
//   TxKeyboard( 218 ) ;   // DA ou 218 - Teclados Antigos ??
   end ;
end;

procedure TACBrDISGertecTeclado.PosicionarCursor(Linha, Coluna: Integer);
begin
{ TxKeyboard( 231 );  // Liga display
  TxKeyboard( 9 ) ;
  TxKeyboard( ColB );
  TxKeyboard( LinB ) ;
  TxKeyboard( 232 );  // DesLiga display }

  TxKeyboard( 214 ) ;   { posicionamento de cursor direto }
  TxKeyboard( Coluna );
  TxKeyboard( Linha ) ;
end;

procedure TACBrDISGertecTeclado.Escrever(const Texto: String);
Var
  A : Integer ;
begin
  TxKeyboard( 231 );  // Liga display
  For A := 1 to Length( Texto ) do
     TxKeyboard( ord(Texto[A]) ) ;      // Envia um Byte por vez...
  TxKeyboard( 232 );  // DesLiga display
end;

procedure TACBrDISGertecTeclado.Ativar;
begin
  { Nao precisa de inicializaçao }
  fpAtivo := true ;
end;

end.
