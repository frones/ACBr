{******************************************************************************}
{ Projeto: ACBr Monitor                                                        }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para }
{ criar uma interface de comunicação com equipamentos de automacao comercial.  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010 Daniel Simões de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na página do Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Este programa é software livre; você pode redistribuí-lo e/ou modificá-lo   }
{ sob os termos da Licença Pública Geral GNU, conforme publicada pela Free     }
{ Software Foundation; tanto a versão 2 da Licença como (a seu critério)       }
{ qualquer versão mais nova.                                                   }
{                                                                              }
{  Este programa é distribuído na expectativa de ser útil, mas SEM NENHUMA     }
{ GARANTIA; nem mesmo a garantia implícita de COMERCIALIZAÇÃO OU DE ADEQUAÇÃO A}
{ QUALQUER PROPÓSITO EM PARTICULAR. Consulte a Licença Pública Geral GNU para  }
{ obter mais detalhes. (Arquivo LICENCA.TXT ou LICENSE.TXT)                    }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral GNU junto com este}
{ programa; se não, escreva para a Free Software Foundation, Inc., 59 Temple   }
{ Place, Suite 330, Boston, MA 02111-1307, USA. Você também pode obter uma     }
{ copia da licença em:  http://www.opensource.org/licenses/gpl-license.php     }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{       Rua Coronel Aureliano de Camargo, 973 - Tatuí - SP - 18270-170         }
{                                                                              }
{******************************************************************************}
unit Sobre;

{$mode objfpc}{$H+}        

interface

uses
  SysUtils, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, HelpIntfs;

type

  { TfrmSobre }

  TfrmSobre = class(TForm)
    bDoar: TBitBtn;
    bSAC : TBitBtn ;
    Image1 : TImage ;
    imgLazarus : TImage ;
    imgSynapse : TImage ;
    Timer1: TTimer;
    lVersao: TLabel;
    lDesenvolvedores: TLabel;
    lACBr: TLabel;
    bOK: TBitBtn;
    lNome: TLabel;
    Label1: TLabel;
    bAjuda: TBitBtn;
    procedure bSACClick(Sender : TObject) ;
    procedure bDoarClick(Sender: TObject);
    procedure imgLazarusClick(Sender : TObject) ;
    procedure imgSynapseClick(Sender : TObject) ;
    procedure Timer1Timer(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure lACBrClick(Sender: TObject);
    procedure lDesenvolvedoresClick(Sender: TObject);
    procedure bAjudaClick(Sender: TObject);
    procedure FormShortCut(Key: Integer; {%H-}Shift: TShiftState;
      var {%H-}Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSobre: TfrmSobre;

implementation
Uses LCLType, ACBrUtil ;

{$R *.lfm}

procedure TfrmSobre.FormCreate(Sender: TObject);
begin
  ClientHeight := 278 ;
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

           if Top <= -60 then
              Top := Parent.Height + 1
        end ;
     end ;
  end ;

  Application.ProcessMessages ;
end;

procedure TfrmSobre.bDoarClick(Sender: TObject);
begin
 OpenURL( 'http://acbr.sourceforge.net/drupal/?q=node/14' );
end;

procedure TfrmSobre.bSACClick(Sender : TObject) ;
begin
 OpenURL('http://www.djsystem.com.br/acbr/sac/');
end;

procedure TfrmSobre.imgLazarusClick(Sender : TObject) ;
begin
  OpenURL('http://www.lazarus.freepascal.org/');
end;

procedure TfrmSobre.imgSynapseClick(Sender : TObject) ;
begin
 OpenURL('http://www.ararat.cz/synapse/');
end;

procedure TfrmSobre.bOKClick(Sender: TObject);
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
 ShowHelpOrErrorForKeyword('','ACBrMonitor/Apresentacao.htm');

 {DirApp := ExtractFilePath(Application.ExeName) ;
 {$IFDEF Linux}
  if FileExists(DirApp+'ACBrMonitor.htm') and DirectoryExists(DirApp+'files') then
     OpenURL(DirApp+'ACBrMonitor.htm')
  else
     OpenURL('http://acbr.sourceforge.net/ACBrMonitor.htm');
 {$ELSE}
  RunCommand( DirApp+'ACBrMonitor.chm');
 {$ENDIF}
 }
end;

procedure TfrmSobre.FormShortCut(Key: Integer; Shift: TShiftState;
  var Handled: Boolean);
begin
  if (Key = VK_HELP) or (Key = VK_F1) then
     bAjuda.Click ;
end;

end.
