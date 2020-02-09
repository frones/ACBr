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
|* 01/06/2005: Daniel Simoes de Almeida
|*  - Primeira Versao ACBrCHQSotomaq
******************************************************************************}

{$I ACBr.inc}

unit ACBrCHQSotomaq;

interface
uses ACBrCHQClass, 
     Classes ;

type TACBrCHQSotomaq = class( TACBrCHQClass )
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
  ACBrUtil, ACBrConsts;

{ TACBrCHQSotomaq }

constructor TACBrCHQSotomaq.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpModeloStr := 'Sotomaq' ;
end;

procedure TACBrCHQSotomaq.Ativar;
begin
  if fpDevice.Porta = ''  then
     raise Exception.Create(ACBrStr('Impressora de Cheques '+fpModeloStr+' requer'+#10+
                            'Porta Serial (COMn) ou Paralela (LPTn)'));

  inherited Ativar ; { Abre porta serial }
end;

procedure TACBrCHQSotomaq.TravarCheque;
begin
  fpDevice.EnviaString( #27 + #178);
  Sleep(100);
end;

procedure TACBrCHQSotomaq.DestravarCheque;
begin
  fpDevice.EnviaString( #27 + #177);
  Sleep(100);
end;

procedure TACBrCHQSotomaq.ImprimirCheque;
Var ValStr, DataStr : String ;
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

function TACBrCHQSotomaq.GetChequePronto: Boolean;
Var nBit : Byte ;
begin
  Result := true ;

  if not fpDevice.IsSerialPort then
     exit ;

  fpDevice.EnviaString( #0 ) ;   // Pede Status
  nBit := fpDevice.LeByte( 200 ) ;

  Result := not TestBit( nBit , 2 ) ;
end;

end.
