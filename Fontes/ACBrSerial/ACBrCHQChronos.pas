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
|*  - Primeira Versao ACBrCHQChronos
|* 01/06/2005: Daniel Simoes de Almeida
|*  - Corrigido BUG no envio da Data em Linux (data como dd-mm-aa)
|*    Bug reportado por Matheus Nogueira
******************************************************************************}

{$I ACBr.inc}

unit ACBrCHQChronos;

interface
uses ACBrCHQClass, 
     Classes ;

type TACBrCHQChronos = class( TACBrCHQClass )
  private

  protected
    function GetChequePronto: Boolean; Override ;

  public
    constructor Create(AOwner: TComponent);

    procedure Ativar ; override ;

    procedure ImprimirCheque ; Override ;
    Procedure TravarCheque ;  Override ;
    Procedure DestravarCheque ;  Override ;
end ;

implementation
Uses
  SysUtils,
  {$IFDEF COMPILER6_UP} DateUtils, {$ELSE} Windows,{$ENDIF}
   ACBrUtil, ACBrConsts ;

{ TACBrCHQChronos }

constructor TACBrCHQChronos.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpModeloStr := 'Chronos' ;
end;

procedure TACBrCHQChronos.Ativar;
begin
  if fpDevice.Porta = ''  then
     raise Exception.Create(ACBrStr('Impressora de Cheques '+fpModeloStr+' requer'+#10+
                            'Porta Serial (COMn) ou Paralela (LPTn)'));

  inherited Ativar ; { Abre porta serial }
end;

procedure TACBrCHQChronos.TravarCheque;
begin
  EnviarStr( #27 + #177 );
end;

procedure TACBrCHQChronos.DestravarCheque;
begin
  EnviarStr( #27 + #178 );
end;

procedure TACBrCHQChronos.ImprimirCheque;
Var
  ValStr, DataStr : String ;
begin
  { Banco }
  EnviarStr( #27 + #162 + fpBanco + #13 ) ;
  { Valor }
  ValStr := IntToStrZero( Round( fpValor * 100), 11) ;
  ValStr := copy(ValStr,1,9)+','+copy(ValStr,10,2) ;
  EnviarStr( #27 + #163 + ValStr + #13 ) ;
  { Favorecido }
  EnviarStr( #27 + #160 + fpFavorecido + #13 ) ;
  { Cidade }
  EnviarStr( #27 + #161 + fpCidade + #13 ) ;
  { Data }
  DataStr := FormatDateTime('dd/mm/yy',fpData) ;
  DataStr := StringReplace(DataStr,DateSeparator,'/',[rfReplaceAll]) ;
  EnviarStr( #27 + #164 + DataStr + #13 ) ;

  EnviarStr( #27 + #176 ) ;   // Imprimir...
end;

function TACBrCHQChronos.GetChequePronto: Boolean;
Var
  nBit : Byte ;
begin
  Result := true ;

  if not fpDevice.IsSerialPort then
     exit ;

  EnviarStr( #0 ) ;   // Pede Status
  nBit := fpDevice.LeByte( 200 ) ;

  Result := not TestBit( nBit , 2 ) ;
end;

end.
