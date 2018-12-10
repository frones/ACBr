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
|* 24/08/2004: Daniel Simoes de Almeida
|*  - Primeira Versao ACBrCHQSchalter
|* 12/11/2005: Daniel Simoes de Almeida
|*  - Corrigido métodos ImprimeLinha e ImprimeVerso
******************************************************************************}

{$I ACBr.inc}

unit ACBrCHQSchalter;

interface
uses ACBrCHQClass,  
     Classes ;

type TACBrCHQSchalter = class( TACBrCHQClass )
  private

  protected
    function GetChequePronto: Boolean; Override ;

  public
    constructor Create(AOwner: TComponent);

    procedure Ativar ; override ;

    procedure ImprimirCheque ; Override ;
    procedure ImprimirLinha( const AString : AnsiString ) ; Override ;
    procedure ImprimirVerso( AStringList : TStrings ) ; Override ;
end ;

implementation
Uses ACBrUtil,
     SysUtils,
   {$IFDEF COMPILER6_UP} DateUtils, {$ELSE} Windows,{$ENDIF}
     ACBrDevice;

{ TACBrCHQSchalter }

constructor TACBrCHQSchalter.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpDevice.Stop := s2 ;
  fpModeloStr := 'Schalter' ;
end;

procedure TACBrCHQSchalter.Ativar;
begin
  if fpDevice.Porta = ''  then
     raise Exception.Create(ACBrStr('Impressora de Cheques '+fpModeloStr+' requer'+#10+
                            'Porta Serial (COMn) ou Paralela (LPTn)'));

  inherited Ativar ; { Abre porta serial }
end;

procedure TACBrCHQSchalter.ImprimirCheque;
Var ValStr, DataStr : String ;
begin
  { Banco }
  EnviarStr( #27 + 'B' + fpBanco ) ;
  { Favorecido }
  EnviarStr( #27 + 'F' +  fpFavorecido + '$' ) ;
  { Cidade }
  EnviarStr( #27 + 'C' + fpCidade + '$' ) ;
  { Data }
  DataStr := FormatDateTime('ddmmyy',fpData) ;
  EnviarStr( #27 + 'D' + DataStr ) ;
  { Valor }
  ValStr := IntToStrZero( Round( fpValor * 100), 14) ;
  EnviarStr( #27 + 'V' + ValStr ) ;
  { Envio do comando Valor Inicia a Impressão }
end;

function TACBrCHQSchalter.GetChequePronto: Boolean;
begin
  Result := fpDevice.EmLinha ;
end;

procedure TACBrCHQSchalter.ImprimirLinha(const AString: AnsiString);
var
  NovaString: AnsiString;
begin
  if Trim(AString) <> '' then
     NovaString := StringOfChar(' ',10) + AString
  else
     NovaString := Trim(AString) ;

  EnviarStr( CodificarPaginaDeCodigo(NovaString) + #10 );  { Adciona LF }
end;

procedure TACBrCHQSchalter.ImprimirVerso(AStringList: TStrings);
Var A : Integer ;
begin
  For A := 0 to AStringList.Count - 1 do
     ImprimirLinha( StringOfChar(' ',10) + AStringList[A] );

  EnviarStr( #12 ) { Envia FF } ;
end;

end.
