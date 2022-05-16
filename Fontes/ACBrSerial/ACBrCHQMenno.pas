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

{$I ACBr.inc}

unit ACBrCHQMenno;

interface
uses
  Classes,
  ACBrCHQClass
  {$IFDEF NEXTGEN}
   ,ACBrBase
  {$ENDIF};

type TACBrCHQMenno = class( TACBrCHQClass )
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
Uses ACBrUtil.Strings,
     SysUtils,
   {$IFDEF COMPILER6_UP} DateUtils, {$ELSE} Windows,{$ENDIF}
     ACBrDeviceSerial;

{ TACBrCHQSchalter }

constructor TACBrCHQMenno.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fpDevice.Stop := s2 ;
  fpModeloStr := 'Menno' ;
end;

procedure TACBrCHQMenno.Ativar;
begin
  if fpDevice.Porta = ''  then
     raise Exception.Create(ACBrStr('Impressora de Cheques '+fpModeloStr+' requer'+#10+
                            'Porta Serial (COMn) ou Paralela (LPTn)'));

  inherited Ativar ; { Abre porta serial }
end;

procedure TACBrCHQMenno.ImprimirCheque;
Var ValStr, DataStr : String ;
   vImprime : String;
begin
  //EnviarStr( #27+'@' ) ;

  { Banco }
  vImprime := #27+ '¢' + fpBanco + #13;
  { Valor }
  ValStr := FormatFloat('#####0.00',fpValor);
  vImprime := vImprime + #27+'£' + ValStr + #13;
  { Data }
  //caso habilitado para emitir bom para, a data enviada sera impressa no campo bom para e
  //a data do cheque será a data atual
  if fpBomPara>0 then
     DataStr := FormatDateTime('dd/mm/yyyy',fpBomPara)
  else
     DataStr := FormatDateTime('dd/mm/yyyy',fpData);

  vImprime := vImprime +  #27+'¤' + DataStr + #13;
  { Cidade }
  vImprime := vImprime +  #27+'¡' + fpCidade + #13;
  { Favorecido }
  vImprime := vImprime +  #27+' ' +  fpFavorecido + #13;
  { Numero Cheque - Caso não mande nada vai solicitar na impressora }
  vImprime := vImprime +  #27+'¥' + '000000' + #13;
  { Cruzado }
  vImprime := vImprime + #27+'¨0';  //mude o parametro para ¨1 se desejar cruzar o cheque
  { bom para }
  if fpBomPara>0 then
     vImprime := vImprime + #27+'©1'
  else
     vImprime := vImprime + #27+'©0';

  { cópia de cheque}
  vImprime := vImprime + #27+'ª0';  //mude o parametro para ª1 se desejar emitir copia do cheque

  vImprime := vImprime + #27+'°';

  EnviarStr( vImprime ) ;

end;

function TACBrCHQMenno.GetChequePronto: Boolean;
Var nBit : Byte ;
begin
  Result := true ;

  if not fpDevice.IsSerialPort then
     exit ;

  fpDevice.EnviaString( #27 + #63 ) ;   // Pede Status
  nBit := fpDevice.LeByte( 200 ) ;
  
  Result := (Chr(nBit)='1');
end;

procedure TACBrCHQMenno.ImprimirLinha(const AString: AnsiString);
var
  NovaString: AnsiString;
begin
  NovaString := AString;
  EnviarStr( NovaString+#12 );
end;

procedure TACBrCHQMenno.ImprimirVerso(AStringList: TStrings);
Var A : Integer ;
   vImprime : String;
begin
  vImprime := #27+'@'+#13+#13+#13+#13+#13;
  For A := 0 to AStringList.Count - 1 do
     vImprime := vImprime + #13+StringOfChar(' ',15) + AStringList[A];

  ImprimirLinha( vImprime );


end;

end.
