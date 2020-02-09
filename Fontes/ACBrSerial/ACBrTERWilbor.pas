{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:     Gabriel Rodrigo Frone                       }
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
|* 31/10/2005: Gabriel Rodrigo Frones
|*  - Primeira Versao ACBrTERWilbor
|
|* 11/05/2011: Marcelo Ferreira (Marcelo-sp)
|*  - Correção   - LeSerial  
|*  - Implemento - Função LeBalanca
******************************************************************************}

{$I ACBr.inc}

Unit ACBrTERWilbor;

Interface

Uses ACBrTERClass,
     Classes;

Type
    TACBrTERWilbor = Class( TACBrTERClass )
      Public
        Constructor Create( AOwner: TComponent );

        Procedure LeBalanca(  Terminal : Word = 0  ); Override;
        Procedure LeSerial( MillisecTimeOut : Integer = 500 ); Override;
        Procedure EnviaString( const Texto : String; Terminal : Word = 0 ); Override;
        Procedure EnviaRotacao( const Texto : String; Linha : Word = 1; Terminal : Word = 0 ); Override;
        Procedure LimpaTela( Terminal : Word = 0 ); Override;
        Procedure PosicionaCursor( Linha, Coluna : Word; Terminal : Word = 0 ); Override;
        Procedure BackSpace( Terminal : Word = 0 ); Override;
    End;

Implementation

Uses ACBrTER, ACBrUtil, Math, 
     {$IFDEF COMPILER6_UP} DateUtils, StrUtils {$ELSE} ACBrD5, Windows{$ENDIF},
     SysUtils ;

{ TACBrTERWilbor }

Constructor TACBrTERWilbor.Create( AOwner: TComponent );
Begin
    Inherited Create( AOwner );

    fpModeloStr := 'Wilbor';
End;

Procedure TACBrTERWilbor.LeBalanca(  Terminal : Word = 0 );
Begin
    if TACBrTER( fpOwner ).Comutadora Then
     begin
       fpDevice.EnviaString( 'S'+Format('%2.2d', [Terminal]) + #5  );
       fpDevice.LeString( 500 );
     end
    else
     begin
       // Não implementado
     end ;
End;

Procedure TACBrTERWilbor.LeSerial( MillisecTimeOut: Integer );
var
  Packet: String;
  nToque: Integer;
Begin
  Try
     nToque := IfThen(fpDuplaConfirmacao, 1, 0);
     Packet := fpDevice.LeString( MillisecTimeOut );
     If TACBrTER( fpOwner ).Comutadora Then
      begin   //Possui Comutadora gerenciando vários Terminais?
        While Length( Packet ) >= 3 Do
        begin
           TACBrTER( fpOwner ).DoRecebeChar( StrToIntDef( Copy( Packet, 1, 2 ), 0 ), Packet[3] );
           Delete( Packet, 1, 3 );
        end;
      end
     else
      begin
        while (Length( Packet ) > nToque) Do
        begin
           TACBrTER( fpOwner ).DoRecebeChar( 0, Packet[1] );
           Delete( Packet, 1, 1 );
        end;
      end;
  Except
    { String não foi recebida (TimeOut) }
  End;
End;

Procedure TACBrTERWilbor.EnviaString( const Texto : String; Terminal : Word = 0 );
Var I : Integer;
Begin
    If TACBrTER( fpOwner ).Comutadora Then
     begin
       For I := 1 To Length( Texto ) Do
       begin
          fpDevice.EnviaString( 'D' + FormatFloat( '00', Terminal ) + Texto[I] + StringOfChar( ' ', 4 ) );
          fpDevice.EnviaString( 'A' + FormatFloat( '00', Terminal ) + StringOfChar( ' ', 12 ) ); //Comando fora do protocolo. Serve para, conforme o Manual, preencher o tempo de espera que o Terminal precisa para Processar esse comando antes de receber o próximo. Veja Apendice A (pag. 13) no Manual da Comutadora.
       end;
     end
    else
       fpDevice.EnviaString( Texto );
End;

Procedure TACBrTERWilbor.EnviaRotacao( const Texto : String; Linha : Word = 1; Terminal : Word = 0 );
var
  TextoProcessado: string;
Begin
    Dec( Linha );
    Linha := min( max(Linha, 0 ), 1) ;

    If Length( Texto ) <= 40 Then
        TextoProcessado := StringOfChar( ' ', 40 ) + Texto;

    TACBrTER( fpOwner ).ListaRotacao.Add( IntToStr( Linha ) + FormatFloat( '00', Terminal ) + TextoProcessado );
End;

Procedure TACBrTERWilbor.LimpaTela( Terminal : Word = 0 );
Begin
    If TACBrTER( fpOwner ).Comutadora Then
     begin
       fpDevice.EnviaString( 'L' + FormatFloat( '00', Terminal ) + StringOfChar( ' ', 12 ) );
       fpDevice.EnviaString( 'A' + FormatFloat( '00', Terminal ) + StringOfChar( ' ', 12 ) ); //Comando fora do protocolo. Serve para, conforme o Manual, preencher o tempo de espera que o Terminal precisa para Processar esse comando antes de receber o próximo. Veja Apendice A (pag. 13) no Manual da Comutadora.
     end
    else
       fpDevice.EnviaString( #27'[H'#27'[J' );
End;

Procedure TACBrTERWilbor.PosicionaCursor( Linha, Coluna : Word; Terminal : Word = 0 );
Begin
    {No MicroTerminal, o índice é zero}
    Dec( Linha );
    Dec( Coluna );

    {Limitando a Linha e a Coluna}
    Linha  := min( max(Linha,  0 ),  1) ;
    Coluna := min( max(Coluna, 0 ), 39) ;

    If TACBrTER( fpOwner ).Comutadora Then
     begin
       fpDevice.EnviaString( 'C' + FormatFloat( '00', Terminal ) + IntToStr( Linha ) + FormatFloat( '00', Coluna ) + StringOfChar( ' ', 12 ) );
       fpDevice.EnviaString( 'A' + FormatFloat( '00', Terminal ) + StringOfChar( ' ', 12 ) ); //Comando fora do protocolo. Serve para, conforme o Manual, preencher o tempo de espera que o Terminal precisa para Processar esse comando antes de receber o próximo. Veja Apendice A (pag. 13) no Manual da Comutadora.
     end
    else
       fpDevice.EnviaString( #27'[' + IntToStr( Linha ) + ';' + IntToStr( Coluna ) + 'H' );
End;

Procedure TACBrTERWilbor.BackSpace( Terminal : Word = 0 );
Begin
    If TACBrTER( fpOwner ).Comutadora Then
     begin
       fpDevice.EnviaString( 'O' + FormatFloat( '00', Terminal ) + Chr(16) + StringOfChar( ' ', 12 ) );
       fpDevice.EnviaString( 'A' + FormatFloat( '00', Terminal ) + StringOfChar( ' ', 12 ) ); //Comando fora do protocolo. Serve para, conforme o Manual, preencher o tempo de espera que o Terminal precisa para Processar esse comando antes de receber o próximo. Veja Apendice A (pag. 13) no Manual da Comutadora.
       EnviaString( ' ', Terminal );
       fpDevice.EnviaString( 'O' + FormatFloat( '00', Terminal ) + Chr(16) + StringOfChar( ' ', 12 ) );
       fpDevice.EnviaString( 'A' + FormatFloat( '00', Terminal ) + StringOfChar( ' ', 12 ) ); //Comando fora do protocolo. Serve para, conforme o Manual, preencher o tempo de espera que o Terminal precisa para Processar esse comando antes de receber o próximo. Veja Apendice A (pag. 13) no Manual da Comutadora.
     end
    else
       fpDevice.EnviaString( #8'[K' );
End;

End.
