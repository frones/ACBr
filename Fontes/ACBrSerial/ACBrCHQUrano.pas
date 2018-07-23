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
|* 29/08/2006: Daniel Simoes de Almeida
|*  - Primeira Versao ACBrCHQUrano
******************************************************************************}

{$I ACBr.inc}

unit ACBrCHQUrano;

interface
uses ACBrCHQClass,  
     Classes
     {$IFNDEF COMPILER6_UP} ,Windows {$ENDIF} ;

type TACBrCHQUrano = class( TACBrCHQClass )
  private

  protected
    function GetChequePronto: Boolean; Override ;

  public
    constructor Create(AOwner: TComponent);

    procedure Ativar ; override ;

    procedure ImprimirCheque ; Override ;
    procedure ImprimirLinha( AString : AnsiString ) ; Override ;
end ;

implementation
Uses ACBrUtil,
     SysUtils
    {$IFDEF COMPILER6_UP}, DateUtils {$ENDIF} ;

{ TACBrCHQUrano }

constructor TACBrCHQUrano.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpDevice.SetDefaultValues ;
  fpModeloStr := 'Urano' ;
end;

procedure TACBrCHQUrano.Ativar;
begin
  if fpDevice.Porta = ''  then
     raise Exception.Create(ACBrStr('Impressora de Cheques '+fpModeloStr+' requer'+#10+
                            'Porta Serial (COMn) ou Paralela (LPTn)'));

  inherited Ativar ; { Abre porta serial }
end;

procedure TACBrCHQUrano.ImprimirCheque;
Var ValStr, DataStr : String ;
begin
  if not fpDevice.EmLinha( 3 ) then  { Impressora está em-linha ? }
    raise Exception.Create(ACBrStr('A impressora de Cheques '+fpModeloStr+
                           ' não está pronta.')) ;

  {  Usando Protocolo Generico 2 (Muito mais simples) }
  { Ligando asteriscos no final da impressao }
  EnviarStr( #27 + 'p1$' ) ;
  { Cidade }
  EnviarStr( #27 + 'c' + PadRight(fpCidade,20) + '$' ) ;
  { Favorecido }
  EnviarStr( #27 + 'f' + PadRight(fpFavorecido,40) + '$' ) ;
  { Data }
  DataStr := FormatDateTime('ddmmyy',fpData) ;
  EnviarStr( #27 + 'd' + DataStr + '$' ) ;
  { Banco }
  EnviarStr( #27 + 'b' + fpBanco + '$' ) ;
  { Valor }
  ValStr := IntToStrZero( Round( fpValor * 100), 14) ;
  EnviarStr( #27 + 'v' + ValStr + '$' ) ;
end;

procedure TACBrCHQUrano.ImprimirLinha(AString: AnsiString);
begin
  EnviarStr( #27 + 'g' + CodificarPaginaDeCodigo(TrimRight(AString)) + #10 + '$' ) ;
end;

function TACBrCHQUrano.GetChequePronto: Boolean;
Var nBit : Byte ;
begin
  Result := true ;

  if not fpDevice.IsSerialPort then
     exit ;

  fpDevice.EnviaString( #170 + #06 + #128 ) ;   // Pede Status
  nBit := fpDevice.LeByte( 200 ) ;

  Result := (nBit <> 2) ;
end;

end.

