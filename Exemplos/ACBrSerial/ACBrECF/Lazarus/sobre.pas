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

unit Sobre;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, LCLType;

type

  { TfrmSobre }

  TfrmSobre = class(TForm)
    Label3: TLabel;
    lDesenvolvedores1: TLabel;
    lVersao1: TLabel;
    Timer1: TTimer;
    lVersao: TLabel;
    lDesenvolvedores: TLabel;
    lACBr: TLabel;
    BitBtn1: TBitBtn;
    lNome: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    lColaboradores: TLabel;
    bAjuda: TBitBtn;
    Image1: TImage;
    procedure Timer1Timer(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure lACBrClick(Sender: TObject);
    procedure lDesenvolvedoresClick(Sender: TObject);
    procedure bAjudaClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;{%h-}
      Shift: TShiftState; X, Y: Integer);{%h-}
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;{%h-}
      Shift: TShiftState; X, Y: Integer);{%h-}
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);{%h-}
  private
    { Private declarations }
    fsFinal : Integer ;
  public
    { Public declarations }
  end;

var
  frmSobre: TfrmSobre;

implementation

{$R *.lfm}

Uses ACBrUtil, Math ;

procedure TfrmSobre.FormCreate(Sender: TObject);
 Var I : Integer ;
begin
  ClientHeight := 218 ;
  self.DoubleBuffered := True ;

  fsFinal := Height + 20 ;
  For I := 0 to ControlCount -1 do
  begin
     if Controls[I] is TLabel then
     begin
        with Controls[I] as TLabel do
        begin
           fsFinal := max(fsFinal,Top+Height + 20) ;
        end ;
     end ;
  end ;
end;

procedure TfrmSobre.Timer1Timer(Sender: TObject);
Var I : Integer ;
begin
  Update ;
  For I := 0 to ControlCount -1 do
  begin
     if Controls[I] is TLabel then
     begin
        with Controls[I] as TLabel do
        begin
           Top := Top - 1 ;

           if Top+Height < 0 then
              Top := fsFinal-Height
        end ;
     end ;
  end ;

  Application.ProcessMessages ;
end;

procedure TfrmSobre.BitBtn1Click(Sender: TObject);
begin
  Timer1.Enabled := false ;
  close ;
end;

procedure TfrmSobre.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Timer1.Enabled := false ;
  CanClose       := true ;
end;

procedure TfrmSobre.lACBrClick(Sender: TObject);
begin
  OpenURL( 'http://acbr.sf.net' );
end;

procedure TfrmSobre.lDesenvolvedoresClick(Sender: TObject);
begin
  OpenURL('www.djsystem.com.br');
end;

procedure TfrmSobre.bAjudaClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/wiki/index.php/ECF');
end;

procedure TfrmSobre.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Timer1.Enabled := False ;
end;

procedure TfrmSobre.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Timer1.Enabled := True ;
end;

procedure TfrmSobre.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_F1) then
     bAjuda.Click ;
end;

end.

