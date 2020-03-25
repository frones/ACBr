{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{																			   }
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

unit EmulaLCB1;

interface

uses
  synaser, ACBrUtil, 
  SysUtils,
 {$IFDEF Delphi6_UP} Types, {$ELSE} Windows,{$ENDIF}
  Classes, Forms, StdCtrls, Controls ;

type
  TfrEmulador = class(TForm)
    Label1: TLabel;
    edBarra: TEdit;
    Label4: TLabel;
    cbxPorta: TComboBox;
    Label2: TLabel;
    Label5: TLabel;
    edSufixo: TEdit;
    Button1: TButton;
    cbMudaCodigo: TCheckBox;
    Label3: TLabel;
    lEnviado: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbxPortaChange(Sender: TObject);
  private
    { Private declarations }
    fSerial : TBlockSerial ;
    Function Converte( cmd : String) : String;
  public
    { Public declarations }
  end;

var
  frEmulador: TfrEmulador;

implementation
Uses ACBrLCB ;

{$R *.dfm}

function TfrEmulador.Converte(cmd: String): String;
var A : Integer ;
begin
  Result := '' ;
  For A := 1 to length( cmd ) do
  begin
     if not (cmd[A] in ['A'..'Z','a'..'z','0'..'9',
                        ' ','.',',','/','?','<','>',';',':',']','[','{','}',
                        '\','|','=','+','-','_',')','(','*','&','^','%','$',
                        '#','@','!','~',']' ]) then
        Result := Result + '#' + IntToStr(ord( cmd[A] )) + ' '
     else
        Result := Result + cmd[A] + ' ';
  end ;
end;

procedure TfrEmulador.FormCreate(Sender: TObject);
begin
  fSerial := TBlockSerial.Create ;
  fSerial.RaiseExcept := true ;
end;

procedure TfrEmulador.Button1Click(Sender: TObject);
begin
  if not fSerial.InstanceActive then
  begin
     fSerial.Connect( cbxPorta.Text );
     fSerial.Config(9600,8,'N',0,false,false);
  end ;

  fSerial.DeadlockTimeout := 1000 ;
  fSerial.Purge ;                   { Limpa a Porta }

  fSerial.SendString( edBarra.Text + TraduzComando( edSufixo.Text ) );

  lEnviado.Caption := Converte( edBarra.Text + TraduzComando( edSufixo.Text ) ) ;
  if cbMudaCodigo.Checked then
  begin
     edBarra.Text := IntToStr( StrToInt64(copy(edBarra.Text,1,12))+1 ) ;
     edBarra.Text := edBarra.Text + EAN13_DV(edBarra.Text) ;
  end ;
end;

procedure TfrEmulador.cbxPortaChange(Sender: TObject);
begin
  if Assigned( fSerial ) then
     if fSerial.InstanceActive then
        fSerial.CloseSocket ;
end;

end.
