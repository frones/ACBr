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
|* Agradecimentos à:
|* Everton H. Cardoso  -  Bematech S/A - Testes deste Modulo  
******************************************************************************}

{$I ACBr.inc}

unit ACBrCHQBematech;

interface
uses ACBrCHQClass,  
     Classes
     {$IFNDEF COMPILER6_UP} ,Windows {$ENDIF} ;

type TACBrCHQBematech = class( TACBrCHQClass )
  private
    FImprimeVerso: boolean;

  public
    constructor Create(AOwner: TComponent);

    procedure Ativar ; override ;

    procedure ProgramarImpressao;
    procedure ImprimirCheque ; Override ;
    Procedure TravarCheque ;  Override ;
    Procedure DestravarCheque ;  Override ;
    Procedure ImprimirVerso(AStringList : TStrings); Override;
end ;

implementation
Uses ACBrUtil.Strings,
     ACBrUtil.Base,
     ACBrConsts,
     SysUtils
    {$IFDEF COMPILER6_UP}, DateUtils {$ENDIF} ;

{ TACBrCHQBematech }

constructor TACBrCHQBematech.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpDevice.HardFlow := true ;
  fpModeloStr := 'Bematech' ;
end;

procedure TACBrCHQBematech.Ativar;
begin
  if fpDevice.Porta = ''  then
     raise Exception.Create(ACBrStr('Impressora de Cheques '+fpModeloStr+' requer'+#10+
                            'Porta Serial (COMn) ou Paralela (LPTn)'));

  fpDevice.HardFlow := true ;  { Ativar RTS/CTS } 
  inherited Ativar ; { Abre porta serial }
end;

procedure TACBrCHQBematech.TravarCheque;
begin
  if FImprimeVerso then
    EnviarStr( #27 + #119 + #1 )
  else
    EnviarStr( #27 + #177 );

end;

procedure TACBrCHQBematech.DestravarCheque;
begin
  if FImprimeVerso then
    EnviarStr( #27 + #119 + #0 )
  else
    EnviarStr( #27 + #176 );

end;

procedure TACBrCHQBematech.ImprimirVerso(AStringList : TStrings);
var
  A : integer;
begin
  FImprimeVerso := True;

  TravarCheque ;

  For A := 0 to AStringList.Count - 1 do
     ImprimirLinha( StringOfChar(' ',10) + AStringList[A] );

  DestravarCheque ;
end;

procedure TACBrCHQBematech.ProgramarImpressao;
begin
  // programa ano com 4 dígitos
  EnviarStr(#27 + #175 + #68 + '4' + #13);
end;

procedure TACBrCHQBematech.ImprimirCheque;
Var ValStr, DataStr : String ;
begin

  FImprimeVerso := False;

  ProgramarImpressao;

  TravarCheque ;

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

  DestravarCheque ;
end;

end.
 
