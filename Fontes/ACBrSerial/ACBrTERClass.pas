{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004   Gabriel Rodrigo Frones               }
{                                                                              }
{ Colaboradores nesse arquivo:            Daniel Simões de Almeida            }
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
|* 25/10/2005: Gabriel Rodrigo Frones
|*  - Primeira Versao ACBrTERClass
|
|* 11/05/2011: Marcelo Ferreira (Marcelo-sp)
|*  - Implemento - Função LeBalança
******************************************************************************}

{$I ACBr.inc}

Unit ACBrTERClass;

Interface

Uses ACBrDevice,    {Units da ACBr}
     Classes,
     {$IFDEF COMPILER6_UP}Types{$ELSE}Windows{$ENDIF};

Type

{ Classe generica de TERMINAL, nao implementa nenhum modelo especifico, apenas
  declara a Classe. NAO DEVE SER INSTANCIADA. Usada apenas como base para
  as demais Classes de TERMINAL como por exemplo a classe TACBrTERWilbor }

    TACBrTERClass = Class
        Private
            Procedure SetAtivo( Const Value : Boolean );
        Protected
            fpOwner : TComponent;
            fpDevice : TACBrDevice;
            fpAtivo : Boolean;
            fpModeloStr : String;
            fpDuplaConfirmacao: Boolean;
        Public
            Constructor Create(AOwner: TComponent);
            Destructor Destroy; Override;

            Property Ativo : Boolean Read fpAtivo Write SetAtivo;
            Procedure Ativar; Virtual;
            Procedure Desativar; Virtual;

            Procedure LeBalanca( Terminal : Word = 0  ); Virtual;  
            Procedure LeSerial( MillisecTimeOut : Integer = 500 ); Virtual;
            Procedure EnviaString( const Texto : String; Terminal : Word = 0 ); Virtual;
            Procedure EnviaRotacao( const Texto : String; Linha : Word = 0; Terminal : Word = 0 ); Virtual;
            Procedure LimpaTela( Terminal : Word = 0 ); Virtual;
            Procedure PosicionaCursor( Linha, Coluna : Word; Terminal : Word = 0 ); Virtual;
            Procedure BackSpace( Terminal : Word = 0 ); Virtual;
            Property ModeloStr : String Read fpModeloStr;
            Property DuplaConfirmacao: Boolean Read fpDuplaConfirmacao Write fpDuplaConfirmacao;
        End;

Implementation

Uses ACBrTER, ACBrUtil, SysUtils;

{ TACBrTERClass }

Constructor TACBrTERClass.Create( AOwner: TComponent );
Begin
    If Not ( AOwner Is TACBrTER ) Then
        Raise Exception.Create( ACBrStr('Essa Classe deve ser instanciada por TACBrTER') );

  { Criando ponteiro interno para as Propriedade SERIAL de ACBrTER,
    para permitir as Classes Filhas o acesso a essas propriedades do Componente }

    fpOwner := AOwner;

    fpDevice    := ( AOwner As TACBrTER ).Device;
    fpDevice.SetDefaultValues;

    fpAtivo     := False;
    fpModeloStr := 'Não Definida';
    fpDuplaConfirmacao := True;
End;

Destructor TACBrTERClass.Destroy;
Begin
    fpDevice := Nil; { Apenas remove referencia (ponteiros internos) }

    Inherited Destroy;
End;

Procedure TACBrTERClass.SetAtivo( Const Value: Boolean );
Begin
    If Value Then
        Ativar
    Else
        Desativar;
End;

Procedure TACBrTERClass.Ativar;
Begin
    If fpAtivo Then
        Exit;

    If fpDevice.Porta <> '' Then
        fpDevice.Ativar;

    fpAtivo := True;
End;

Procedure TACBrTERClass.Desativar;
Begin
    If Not fpAtivo Then
        Exit;

    If fpDevice.Porta <> '' Then
        fpDevice.Desativar;

    fpAtivo := False;
End;

Procedure TACBrTERClass.LeSerial( MillisecTimeOut : Integer );
Begin
    { Deve ser implementada na Classe Filha }
    Raise Exception.Create(ACBrStr('Procedure LeSerial não implementada em: ') + ModeloStr );
End;

Procedure TACBrTERClass.LimpaTela( Terminal : Word = 0 );
Begin
    { Deve ser implementada na Classe Filha }
    Raise Exception.Create(ACBrStr('Procedure LimpaTela não implementada em: ') + ModeloStr );
End;

Procedure TACBrTERClass.PosicionaCursor( Linha, Coluna : Word; Terminal : Word = 0 );
Begin
    { Deve ser implementada na Classe Filha }
    Raise Exception.Create(ACBrStr('Procedure PosicionaCursor não implementada em: ') + ModeloStr );
End;

Procedure TACBrTERClass.BackSpace( Terminal : Word = 0 );
Begin
    { Deve ser implementada na Classe Filha }
    Raise Exception.Create(ACBrStr('Procedure BackSpace não implementada em: ') + ModeloStr );
End;

Procedure TACBrTERClass.EnviaString( const Texto : String; Terminal : Word = 0 );
Begin
    { Deve ser implementada na Classe Filha }
    Raise Exception.Create(ACBrStr('Procedure EnviaString não implementada em: ') + ModeloStr );
End;

Procedure TACBrTERClass.EnviaRotacao( const Texto : String; Linha : Word = 0; Terminal : Word = 0 );
Begin
    { Deve ser implementada na Classe Filha }
    Raise Exception.Create(ACBrStr('Procedure EnviaRotacao não implementada em: ') + ModeloStr );
End;


Procedure TACBrTERClass.LeBalanca( Terminal : Word = 0  );
Begin
    { Deve ser implementada na Classe Filha }
    Raise Exception.Create(ACBrStr('Procedure LeBalanca não implementada em: ') + ModeloStr );
End;

End.
